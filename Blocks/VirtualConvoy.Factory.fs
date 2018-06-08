/// Put together instances and predicates of virtual convoys.
module SturmovikMission.Blocks.VirtualConvoy.Factory

open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.Links
open SturmovikMission.Blocks.Predicates
open SturmovikMission.Blocks.Conjunction
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.Vehicles
open System.Numerics
open VectorExtension
open SturmovikMission.Blocks.WhileEnemyClose
open SturmovikMission.Blocks.Timer
open SturmovikMission.Blocks.EventReporting
open SturmovikMission.Blocks.IconDisplay
open System.Collections.Generic

// Types for each instance type.
// Those are typically typed ints, but could be typed strings, or any other type suitable for a dictionary key.
type WaypointInstance = WaypointInstance of int
type TruckInConvoyInstance = TruckInConvoyInstance of pos: int
type TimerInstance = TimerInstance of int
type DamagedTruckInstance = DamagedTruckInstance of pos: int
type BridgeDestroyedConjInstance = BridgeDestroyedConjInstance of WaypointInstance * int

/// Path internal nodes can be used to mark areas where there are bridges or other narrow sections.
/// Narrow sections are located between pairs of waypoints called BeforeBridge and AfterBridge in the scenario.
/// Bridges may also be located at other waypoints.
type PathVertexRole =
    | NarrowZoneEntrance
    | NarrowZoneExit
    | Intermediate
with
    member this.Opposite =
        match this with
        | NarrowZoneEntrance -> NarrowZoneExit
        | NarrowZoneExit -> NarrowZoneEntrance
        | Intermediate -> Intermediate

/// <summary>
/// Type used in the arguments of VirtualConvoy.Create. Denotes one vertex of the path of the virtual convoy.
/// </summary>
type PathVertex =
    { Pos : Vector2
      Ori : float32
      Radius : int
      Speed : int
      Priority : int
      SpawnSide : SpawnSide
      Role : PathVertexRole
    }

/// <summary>
/// A virtual convoy, i.e. a convoy that does not actually exist until an enemy approaches its
/// expected position. The intent is to provide the illusion of a mission filled with large numbers
/// of planes and vehicles, while keeping CPU utilization of the server to a low level.
/// See Proto.txt.
/// </summary>
type VirtualConvoy =
    { TheConvoy : Convoy
      TruckInConvoySet : Map<TruckInConvoyInstance, TruckInConvoy>
      BridgeDestroyedConjSet : Map<BridgeDestroyedConjInstance, Conjunction>
      WaypointSet : Map<WaypointInstance, Waypoint>
      BridgeAtWaypoint : Map<WaypointInstance, Mcu.McuEntity>
      StartDelay : Timer
      DiscardDelay : Timer
      TruckDamagedSet : Map<DamagedTruckInstance, EventReporting>
      DepartureReporting : EventReporting
      BlockedReporting : EventReporting
      IconCover : IconDisplay
      IconAttack : IconDisplay
      Api : ConvoyApi

      TruckInConvoy : Set<int * TruckInConvoyInstance>
      BridgeConjunctionAtWaypoint : Set<WaypointInstance * BridgeDestroyedConjInstance>
      Path : Set<WaypointInstance * WaypointInstance>
      PathStart : WaypointInstance
      PathEnd : WaypointInstance
      TruckDamagedOfTruckInConvoy : Set<TruckInConvoyInstance * DamagedTruckInstance>
    }
with
    interface McuUtil.IMcuGroup with
        member this.Content =
            [
            ]
        member this.LcStrings = []
        member this.SubGroups =
            [
                yield this.TheConvoy.All
                for kvp in this.WaypointSet do
                    yield kvp.Value.All
                for kvp in this.TruckInConvoySet do
                    yield kvp.Value.All
                yield this.StartDelay.All
                yield this.DiscardDelay.All
                for kvp in this.TruckDamagedSet do
                    yield kvp.Value.All
                yield this.Api.All
                yield this.DepartureReporting.All
                yield this.BlockedReporting.All
                yield this.IconAttack.All
                yield this.IconCover.All
                for kvp in this.BridgeDestroyedConjSet do
                    yield kvp.Value.All
            ]

    static member MaxConvoySize = 15

    /// <summary>
    /// Create the instances and relations of a virtual convoy.
    /// </summary>
    /// <param name="store">Numerical ID store. All MCUs in a mission must be created using the same store to avoid duplicate identifiers.</param>
    /// <param name="path">Path followed by the convoy.</param>
    /// <param name="bridges">Map bridges to 
    /// <param name="convoySize">Number of vehicle/planes in the column or wing.</param>
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, path : PathVertex list, bridges : (PathVertex * Mcu.McuEntity) list, convoySize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue, convoyName, rankOffset) =
        if convoySize > VirtualConvoy.MaxConvoySize then
            invalidArg "convoySize" "Maximum convoy size exceeded"
        if path.IsEmpty then
            invalidArg "path" "Must be non-empty"
        let startPos = path.Head
        let theConvoy = Convoy.Create(store, startPos.Pos, startPos.Ori, country)
        let truckInConvoySet =
            seq {
                for pos in 1..convoySize do
                    yield (TruckInConvoyInstance(pos), TruckInConvoy.Create(store, startPos.Pos, startPos.Ori, pos, startPos.SpawnSide, country, convoyName))
            }
            |> Map
        let waypointSet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    yield (WaypointInstance i, Waypoint.Create(store, vertex.Pos, vertex.Ori, vertex.Speed, vertex.Priority))
            }
            |> Map.ofSeq
        let getWaypointAt (v : Vector2) =
            waypointSet
            |> Map.toSeq
            |> Seq.minBy (fun (instance, wp) -> (Vector2.FromMcu(wp.Waypoint.Pos) - v).Length())
            |> fst
        let bridgeAtWaypoint =
            seq {
                for v, bridge in bridges do
                    let wp =
                        try
                            getWaypointAt v.Pos
                            |> Some
                        with
                        | _ -> None
                    match wp with
                    | Some wp -> yield (wp, bridge)
                    | None -> ()
            }
            |> Map.ofSeq
        let conjunctionSet =
            seq {
                for wp, bridge in Map.toSeq bridgeAtWaypoint do
                    let conj = Conjunction.Create(store, Vector2.FromMcu bridge.Pos + Vector2(0.0f, 100.0f)).MakeInitiallyFalse(store)
                    let instance = BridgeDestroyedConjInstance(wp, bridge.Index)
                    bridge.OnEvents <-
                        { Mcu.Type = int Mcu.EventTypes.OnKilled
                          Mcu.TarId = conj.SetA.Index }
                        :: bridge.OnEvents
                    yield (instance, conj)
            }
            |> Map.ofSeq
        let bridgeConjAtWaypoint =
            conjunctionSet
            |> Map.toSeq
            |> Seq.map (fun (BridgeDestroyedConjInstance(wp, _) as conjId, _) -> wp, conjId)
            |> Set.ofSeq
        let startDelay = Timer.Create(store, startPos.Pos, 1.0)
        let discardDelay = Timer.Create(store, (List.last path).Pos, 300.0)
        let truckInConvoy =
            seq {
                for pos in 1..convoySize do
                    yield (pos, TruckInConvoyInstance(pos))
            }
            |> Set.ofSeq
        let pathStart = WaypointInstance 0
        let pathEnd = WaypointInstance (List.length path - 1)
        let path2 =
            Seq.indexed path
            |> Seq.map fst
            |> Seq.pairwise
            |> Seq.map (fun (i, j) -> WaypointInstance i, WaypointInstance j)
            |> Set.ofSeq
        let apiPos =
            path
            |> List.fold (fun sum vertex -> sum + vertex.Pos) Vector2.Zero
        let apiPos =
            let n = List.length path
            apiPos / float32 n
        let api = ConvoyApi.Create(store, apiPos, convoySize)

        let departure =
            let name =
                sprintf "%s-D-%d" convoyName rankOffset
            EventReporting.Create(store, country, apiPos + Vector2(-100.0f, -100.0f), name)

        let blocked =
            let name =
                sprintf "%s-B-%d" convoyName rankOffset
            EventReporting.Create(store, country, apiPos + Vector2(-100.0f, -200.0f), name)

        let iconPos =
            let tip = (List.head path).Pos
            let top = (List.last path).Pos
            0.5f * (tip + top)

        let coverIcon, attackIcon = IconDisplay.CreatePair(store, lcStore, iconPos, "", coalition, Mcu.IconIdValue.CoverTransportColumn)

        let damagedTrucks, damagedNotificationOfTruckInConvoy =
            let mixed =
                [
                    let grouped =
                        truckInConvoy
                        |> Seq.groupBy (fun (rank, truck) -> rank)
                    for rank, trucks in grouped do
                        let name = sprintf "%s-K-%d" convoyName (rankOffset + rank - 1)
                        let note = EventReporting.Create(store, country, apiPos + Vector2(-200.0f, -100.0f), name)
                        yield Choice1Of2(DamagedTruckInstance rank, note)
                        for _, truck in trucks do
                            yield Choice2Of2(truck, DamagedTruckInstance rank)
                ]
            mixed
            |> List.choose (function Choice1Of2 x -> Some x | _ -> None)
            |> Map.ofList,
            mixed
            |> List.choose (function Choice2Of2 x -> Some x | _ -> None)
            |> Set.ofList

        { TheConvoy = theConvoy
          TruckInConvoySet = truckInConvoySet
          BridgeDestroyedConjSet = conjunctionSet
          BridgeAtWaypoint = bridgeAtWaypoint
          WaypointSet = waypointSet
          StartDelay  = startDelay
          DiscardDelay = discardDelay
          TruckDamagedSet = damagedTrucks
          DepartureReporting = departure
          BlockedReporting = blocked
          IconCover = coverIcon
          IconAttack = attackIcon
          Api = api
          TruckInConvoy = truckInConvoy
          Path = path2
          PathStart = pathStart
          PathEnd = pathEnd
          TruckDamagedOfTruckInConvoy = damagedNotificationOfTruckInConvoy
          BridgeConjunctionAtWaypoint = bridgeConjAtWaypoint
        }

    /// <param name="rankOffset">Long columns are split into groups (by the caller); this is the rank of the first vehicle in the original column</param>
    static member CreateColumn(store : NumericalIdentifiers.IdStore, lcStore, path, bridges, columnContent : VehicleTypeData list, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue, eventName, rankOffset) =
        let columnContent = Array.ofList columnContent
        let convoy = VirtualConvoy.Create(store, lcStore, path, bridges, columnContent.Length, country, coalition, eventName, rankOffset)
        convoy.IconCover.Icon.IconId <- Mcu.IconIdValue.CoverArmorColumn
        convoy.IconAttack.Icon.IconId <- Mcu.IconIdValue.AttackArmorColumn
        for instance, truck in convoy.TruckInConvoySet |> Map.toSeq do
            let vehicle = getByIndex truck.Entity.MisObjID (McuUtil.deepContentOf truck.All) :?> Mcu.HasEntity
            let (TruckInConvoyInstance(pos)) = instance
            let model = columnContent.[pos - 1]
            model.AssignTo(vehicle)
        convoy


    /// <summary>
    /// Create the links between the MCUs in the virtual convoy.
    /// </summary>
    member this.CreateLinks() =
        let columns =
            [
                // Req.1: Add trucks to columns
                for position, truck in this.TruckInConvoy do
                    yield this.TruckInConvoySet.[truck].Entity, this.TheConvoy.LeadCarEntity, position
            ]
        let objectLinks =
            [
                // Req.2: Connect lead car to waypoints
                for wp in star this.Path this.PathStart do
                    yield this.WaypointSet.[wp].Waypoint :> Mcu.McuTrigger, this.TheConvoy.LeadCarEntity :> Mcu.McuBase
            ]
        let targetLinks =
            [
                // Req.1
                yield this.Api.Start, this.TheConvoy.ActivateGroup :> Mcu.McuBase
                // Req.2
                yield this.Api.Start, upcast this.StartDelay.Start
                yield this.StartDelay.Elapsed, upcast this.WaypointSet.[this.PathStart].Waypoint
                // Req.3
                for wp1, wp2 in this.Path do
                    yield upcast this.WaypointSet.[wp1].Waypoint, upcast this.WaypointSet.[wp2].Waypoint
                // Req.4
                yield upcast this.WaypointSet.[this.PathEnd].Waypoint, upcast this.DiscardDelay.Start
                yield this.DiscardDelay.Elapsed, upcast this.TheConvoy.Discard
                for _, truck in this.TruckInConvoy do
                    yield this.TheConvoy.Discard, upcast this.TruckInConvoySet.[truck].Discard
                // Req.5
                for truck, note in this.TruckDamagedOfTruckInConvoy do
                    yield this.TruckInConvoySet.[truck].Damaged, upcast this.TruckDamagedSet.[note].Trigger
                // Req.6
                yield this.Api.Start, upcast this.IconCover.Show
                yield this.Api.Start, upcast this.IconAttack.Show
                // Req.7
                yield this.Api.Destroyed, upcast this.IconCover.Hide
                yield this.Api.Destroyed, upcast this.IconAttack.Hide
                // Req.8
                yield upcast this.WaypointSet.[this.PathEnd].Waypoint, upcast this.IconCover.Hide
                yield upcast this.WaypointSet.[this.PathEnd].Waypoint, upcast this.IconAttack.Hide
                // Req.9
                for _, truck in this.TruckInConvoy do
                    yield this.TruckInConvoySet.[truck].Damaged, upcast this.Api.Destroyed
                // Req.10
                yield this.Api.Start, upcast this.DepartureReporting.Trigger
                // Req.11
                yield upcast this.WaypointSet.[this.PathEnd].Waypoint, upcast this.Api.Arrived
                // Req.12
                yield this.Api.Blocked, upcast this.DiscardDelay.Start
                // Req.13
                yield this.Api.Blocked, upcast this.BlockedReporting.Trigger
                for wp, conj in this.BridgeConjunctionAtWaypoint do
                    let wp = this.WaypointSet.[wp]
                    let conj = this.BridgeDestroyedConjSet.[conj]
                    // Req.12
                    yield wp.PassedDisable, upcast conj.SetA
                    // Req.13
                    yield conj.SetA, upcast this.Api.Blocked
                    // Req.14
                    yield upcast wp.Waypoint, upcast conj.SetB
                    // Req.15
                    yield conj.AllTrue, upcast this.TheConvoy.StopTravel
            ]
        let events =
            [
                for wp, conj in this.BridgeConjunctionAtWaypoint do
                    match this.BridgeAtWaypoint.TryFind(wp) with
                    | Some bridge ->
                        let conj = this.BridgeDestroyedConjSet.[conj]
                        // Req.12
                        yield bridge, Mcu.EventTypes.OnKilled, conj.SetA :> Mcu.McuBase
                    | None -> ()
            ]
        { Columns = columns
          Objects = objectLinks
          Targets = targetLinks
          Events = events
        }