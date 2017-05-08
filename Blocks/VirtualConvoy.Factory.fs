/// Put together instances and predicates of virtual convoys.
module SturmovikMission.Blocks.VirtualConvoy.Factory

open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.Links
open SturmovikMission.Blocks.Predicates
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.Vehicles
open System.Numerics
open Vector

// Types for each instance type.
// Those are typically typed ints, but could be typed strings, or any other type suitable for a dictionary key.
type ConvoyInstance = ConvoyInstance of int
type WhileEnemyCloseInstance = WhileEnemyCloseInstance of int
type ActiveWaypointInstance = ActiveWaypointInstance of int
type TruckInConvoyInstance = TruckInConvoyInstance of convoy: int * pos: int
type TimerInstance = TimerInstance of int
type AtDestinationInstance =
    | TruckAtDestinationInstance of TruckInConvoyInstance
    | LeadCarAtDestinationInstance of ConvoyInstance
type DamagedTruckInstance = DamagedTruckInstance of pos: int
type SimpleWaypointInstance = SimpleWaypointInstance of int

/// <summary>
/// Type used in the arguments of VirtualConvoy.Create. Denotes one vertex of the path of the virtual convoy.
/// </summary>
type PathVertex =
    { Pos : Vector2
      Ori : float32
      Radius : int
      Speed : int
      Priority : int
    }

/// <summary>
/// A virtual convoy, i.e. a convoy that does not actually exist until an enemy approaches its
/// expected position. The intent is to provide the illusion of a mission filled with large numbers
/// of planes and vehicles, while keeping CPU utilization of the server to a low level.
/// See Proto.txt.
/// </summary>
type VirtualConvoy =
    { ConvoySet : Map<ConvoyInstance, Convoy>
      TruckInConvoy : Set<ConvoyInstance * int * TruckInConvoyInstance>
      WhileEnemyCloseOfConvoy : Set<ConvoyInstance * WhileEnemyCloseInstance>
      
      TruckInConvoySet : Map<TruckInConvoyInstance, TruckInConvoy>
      
      ActiveWaypointSet : Map<ActiveWaypointInstance, ActiveWaypoint>
      ConvoyAtWaypoint : Set<ActiveWaypointInstance * ConvoyInstance>
      EnemyCloseAtWaypoint : Set<ActiveWaypointInstance * WhileEnemyCloseInstance>
      Path : Set<ActiveWaypointInstance * ActiveWaypointInstance>
      PathStart : Set<ActiveWaypointInstance>
      PathEnd : Set<ActiveWaypointInstance>
      TimerBetweenWaypoints : Set<ActiveWaypointInstance * ActiveWaypointInstance * TimerInstance>

      WhileEnemyCloseSet : Map<WhileEnemyCloseInstance, WhileEnemyClose>
      ConvoyOfEnemyClose : Set<WhileEnemyCloseInstance * ConvoyInstance>

      TimerSet : Map<TimerInstance, Timer>
      
      DelaySet : Map<TimerInstance, Timer>
      DelayBeforeDeactivation : Set<ConvoyInstance * TimerInstance>

      SimpleWaypointSet : Map<SimpleWaypointInstance, Mcu.McuWaypoint>
      InvasionPath : Set<SimpleWaypointInstance * SimpleWaypointInstance>
      InvasionStart : Set<SimpleWaypointInstance>
      InvasionEnd : Set<SimpleWaypointInstance>

      AtDestinationSet : Map<AtDestinationInstance, EventReporting>

      TruckDamagedSet : Map<DamagedTruckInstance, EventReporting>
      TruckDamagedOfTruckInConvoy : Set<TruckInConvoyInstance * DamagedTruckInstance>

      DepartureReporting : EventReporting
      IconCover : IconDisplay
      IconAttack : IconDisplay
      Api : ConvoyControl
    }
with
    interface McuUtil.IMcuGroup with
        member this.Content =
            [
                for kvp in this.SimpleWaypointSet do
                    yield upcast kvp.Value
            ]
        member this.LcStrings = []
        member this.SubGroups =
            [
                for kvp in this.ConvoySet do
                    yield kvp.Value.All
                for kvp in this.TruckInConvoySet do
                    yield kvp.Value.All
                for kvp in this.ActiveWaypointSet do
                    yield kvp.Value.All
                for kvp in this.WhileEnemyCloseSet do
                    yield kvp.Value.All
                for kvp in this.TimerSet do
                    yield kvp.Value.All
                for kvp in this.AtDestinationSet do
                    yield kvp.Value.All
                for kvp in this.TruckDamagedSet do
                    yield kvp.Value.All
                for kvp in this.DelaySet do
                    yield kvp.Value.All
                yield this.Api.All
                yield this.DepartureReporting.All
                yield this.IconAttack.All
                yield this.IconCover.All
            ]

    static member MaxConvoySize = 15

    /// <summary>
    /// Create the instances and relations of a virtual convoy.
    /// </summary>
    /// <param name="store">Numerical ID store. All MCUs in a mission must be created using the same store to avoid duplicate identifiers.</param>
    /// <param name="path">Path followed by the convoy.</param>
    /// <param name="convoySize">Number of vehicle/planes in the column or wing.</param>
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, path : PathVertex list, invasion : PathVertex list, convoySize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue, convoyName, rankOffset) =
        if convoySize > VirtualConvoy.MaxConvoySize then
            invalidArg "convoySize" "Maximum convoy size exceeded"
        let convoySet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    yield (ConvoyInstance i, Convoy.Create(store, vertex.Pos, vertex.Ori, country))
            }
            |> Map.ofSeq
        let truckInConvoySet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    for pos in 1..convoySize do
                        yield (TruckInConvoyInstance(i, pos), TruckInConvoy.Create(store, vertex.Pos, vertex.Ori, pos, country, convoyName))
            }
            |> Map
        let whileEnemyCloseSet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    yield (WhileEnemyCloseInstance i, WhileEnemyClose.Create(false, false, store, vertex.Pos, coalition))
            }
            |> Map.ofSeq
        let activeWaypointSet =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    yield (ActiveWaypointInstance i, ActiveWaypoint.Create(store, vertex.Pos, vertex.Ori, vertex.Speed, vertex.Priority))
            }
            |> Map.ofSeq
        let simpleWaypointSet =
            seq {
                let subst = Mcu.substId <| store.GetIdMapper()
                for i, vertex in Seq.indexed invasion do
                    let mcu = newWaypoint i vertex.Pos vertex.Ori vertex.Radius vertex.Speed vertex.Priority
                    subst mcu
                    yield (SimpleWaypointInstance i, mcu)
            }
            |> Map.ofSeq
        let timerSet =
            seq {
                for i, (curr, next) in Seq.zip (Seq.initInfinite id) (Seq.pairwise path) do
                    let expectedTime =
                        let vec =
                            next.Pos - curr.Pos
                        let distance = float(vec.Length())
                        3.6 * distance / float next.Speed
                    yield (TimerInstance i, Timer.Create(store, curr.Pos, 1.1 * expectedTime))
            }
            |> Map.ofSeq
        let delays =
            seq {
                for i, vertex in Seq.indexed path do
                    yield (TimerInstance i, Timer.Create(store, vertex.Pos, 300.0)) // 5 minutes
            }
            |> Map.ofSeq
        let delayBeforeConvoyDeactivation =
            delays
            |> Map.toSeq
            |> Seq.map (fun (TimerInstance i, _) -> ConvoyInstance i, TimerInstance i)
            |> Set.ofSeq

        let truckInConvoy =
            seq {
                for i, vertex in Seq.zip (Seq.initInfinite id) path do
                    for pos in 1..convoySize do
                        yield (ConvoyInstance i, pos, TruckInConvoyInstance(i, pos))
            }
            |> Set.ofSeq
        let whileEnemyCloseOfConvoy =
            path
            |> List.mapi(fun i _ -> (ConvoyInstance i, WhileEnemyCloseInstance i))
            |> Set.ofList
        let convoyAtWaypoint =
            path
            |> List.mapi(fun i _ -> (ActiveWaypointInstance i, ConvoyInstance i))
            |> Set.ofList
        let enemyCloseAtWaypoint =
            path
            |> List.mapi(fun i _ -> (ActiveWaypointInstance i, WhileEnemyCloseInstance i))
            |> Set.ofList
        let path2 =
            path
            |> Seq.mapi(fun i _ -> ActiveWaypointInstance i)
            |> Seq.pairwise
            |> Set.ofSeq
        let pathStart = Set [ ActiveWaypointInstance 0 ]
        let pathEnd = Set [ ActiveWaypointInstance (List.length path - 1) ]
        let invasion2 =
            invasion
            |> Seq.mapi(fun i _ -> SimpleWaypointInstance i)
            |> Seq.pairwise
            |> Set.ofSeq
        let invasionStart =
            match invasion with
            | [] -> Set.empty
            | _ -> Set [SimpleWaypointInstance 0]
        let invasionEnd =
            match invasion with
            | [] -> Set.empty
            | _ -> Set [SimpleWaypointInstance (List.length invasion - 1)]
        let timerBetweenWaypoints =
            path2
            |> Seq.map (fun (ActiveWaypointInstance i as curr, next) -> (curr, next, TimerInstance i))
            |> Set.ofSeq
        let convoyOfEnemyClose =
            activeWaypointSet
            |> Map.map (fun wp _ -> (get enemyCloseAtWaypoint wp, get convoyAtWaypoint wp))
            |> Map.toSeq
            |> Seq.map snd
            |> Set.ofSeq
        let apiPos =
            path
            |> List.fold (fun sum vertex -> sum + vertex.Pos) Vector2.Zero
        let apiPos =
            let n = List.length path
            apiPos / float32 n
        let api = ConvoyControl.Create(store, apiPos, convoySize)

        let atDestinationSet =
            seq {
                for wp in pathEnd do
                    let convoy = get convoyAtWaypoint wp
                    let pos =
                        if invasionEnd.IsEmpty then
                            Vector2.FromMcu(activeWaypointSet.[wp].Waypoint.Pos) + Vector2(0.0f, 100.0f)
                        else
                            let iwp = invasionEnd.MinimumElement
                            Vector2.FromMcu(simpleWaypointSet.[iwp].Pos) + Vector2(0.0f, 100.0f)
                    let name = sprintf "%s-A-%d" convoyName 0
                    yield(LeadCarAtDestinationInstance(convoy), EventReporting.Create(store, country, pos, name))
                    for convoy2, rank, truck in truckInConvoy do
                        if convoy = convoy2 then
                            let name = sprintf "%s-A-%d" convoyName (rank + rankOffset)
                            yield(TruckAtDestinationInstance(truck), EventReporting.Create(store, country, pos, name))
            }
            |> Map.ofSeq

        let departure =
            let name =
                sprintf "%s-D-%d" convoyName rankOffset
            EventReporting.Create(store, country, apiPos + Vector2(-100.0f, -100.0f), name)

        let iconPos =
            let tip = (List.head path).Pos
            let top =
                match invasion with
                | [] -> (List.last path).Pos
                | _ :: _ -> (List.last invasion).Pos
            0.5f * (tip + top)

        let coverIcon, attackIcon = IconDisplay.CreatePair(store, lcStore, iconPos, "", coalition, Mcu.IconIdValue.CoverTransportColumn)

        let damagedTrucks, damagedNotificationOfTruckInConvoy =
            let mixed =
                [
                    let grouped =
                        truckInConvoy
                        |> Seq.groupBy (fun (convoy, rank, truck) -> rank)
                    for rank, trucks in grouped do
                        let name = sprintf "%s-K-%d" convoyName (rankOffset + rank - 1)
                        let note = EventReporting.Create(store, country, apiPos + Vector2(-200.0f, -100.0f), name)
                        yield Choice1Of2(DamagedTruckInstance rank, note)
                        for _, _, truck in trucks do
                            yield Choice2Of2(truck, DamagedTruckInstance rank)
                ]
            mixed
            |> List.choose (function Choice1Of2 x -> Some x | _ -> None)
            |> Map.ofList,
            mixed
            |> List.choose (function Choice2Of2 x -> Some x | _ -> None)
            |> Set.ofList

        { ConvoySet = convoySet
          TruckInConvoy = truckInConvoy
          WhileEnemyCloseOfConvoy = whileEnemyCloseOfConvoy
          TruckInConvoySet = truckInConvoySet
          ActiveWaypointSet = activeWaypointSet
          ConvoyAtWaypoint = convoyAtWaypoint
          EnemyCloseAtWaypoint = enemyCloseAtWaypoint
          Path = path2
          PathStart = pathStart
          PathEnd = pathEnd
          SimpleWaypointSet = simpleWaypointSet
          InvasionPath = invasion2
          InvasionStart = invasionStart
          InvasionEnd = invasionEnd
          TimerBetweenWaypoints = timerBetweenWaypoints
          WhileEnemyCloseSet = whileEnemyCloseSet
          ConvoyOfEnemyClose = convoyOfEnemyClose
          TimerSet = timerSet
          DelaySet = delays
          DelayBeforeDeactivation = delayBeforeConvoyDeactivation
          Api = api
          AtDestinationSet = atDestinationSet
          DepartureReporting = departure
          TruckDamagedSet = damagedTrucks
          TruckDamagedOfTruckInConvoy = damagedNotificationOfTruckInConvoy
          IconCover = coverIcon
          IconAttack = attackIcon
        }


    /// <param name="rankOffset">Long columns are split into groups (by the caller); this is the rank of the first vehicle in the original column</param>
    static member CreateColumn(store : NumericalIdentifiers.IdStore, lcStore, path : PathVertex list, invasionPath : PathVertex list, columnContent : VehicleTypeData list, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue, eventName, rankOffset) =
        let columnContent = Array.ofList columnContent
        let convoy = VirtualConvoy.Create(store, lcStore, path, invasionPath, columnContent.Length, country, coalition, eventName, rankOffset)
        convoy.IconCover.Icon.IconId <- Mcu.IconIdValue.CoverArmorColumn
        convoy.IconAttack.Icon.IconId <- Mcu.IconIdValue.AttackArmorColumn
        for instance, truck in convoy.TruckInConvoySet |> Map.toSeq do
            let vehicle = getByIndex truck.Entity.MisObjID (McuUtil.deepContentOf truck.All) :?> Mcu.HasEntity
            let (TruckInConvoyInstance(_, pos)) = instance
            let model = columnContent.[pos - 1]
            vehicle.Model <- model.Model
            vehicle.Script <- model.Script
        convoy


    /// <summary>
    /// Create the links between the MCUs in the virtual convoy.
    /// </summary>
    member this.CreateLinks() =
        let columns =
            [
                //  Add trucks to columns
                for convoy, position, truck in this.TruckInConvoy do
                    yield this.TruckInConvoySet.[truck].Entity, this.ConvoySet.[convoy].LeadCarEntity, position
            ]
        let objectLinks =
            [
                //  Connect lead car to waypoints
                for wp, convoy in this.ConvoyAtWaypoint do
                    for wp2 in star this.Path wp do
                        yield this.ActiveWaypointSet.[wp2].Waypoint :> Mcu.McuTrigger, this.ConvoySet.[convoy].LeadCarEntity :> Mcu.McuBase
                //  Check zone connected to its convoy leader
                for wec, convoy in this.ConvoyOfEnemyClose do
                    yield this.WhileEnemyCloseSet.[wec].Proximity, this.ConvoySet.[convoy].LeadCarEntity :> Mcu.McuBase
                //  Connect convoy at last active waypoint to invasion waypoints
                for start in this.InvasionStart do
                    for wp in star this.InvasionPath start do
                        for _, convoy in this.ConvoyAtWaypoint do
                            yield this.SimpleWaypointSet.[wp] :> Mcu.McuTrigger, this.ConvoySet.[convoy].LeadCarEntity :> Mcu.McuBase
            ]
        let targetLinks =
            [
                //  Trigger movement to next active waypoint
                for wp, convoy in this.ConvoyAtWaypoint do
                    match tryGet this.Path wp with
                    | Some wp2 ->
                        yield this.ConvoySet.[convoy].TriggerGates, this.ActiveWaypointSet.[wp2].Gate :> Mcu.McuBase
                    | None ->
                        ()
                for wec, convoy in this.ConvoyOfEnemyClose do
                    //  Activate convoy when enemy close
                    yield this.WhileEnemyCloseSet.[wec].WakeUp, this.ConvoySet.[convoy].ActivateGroup :> Mcu.McuBase
                    //  Deactivate convoy when enemy gone
                    yield this.WhileEnemyCloseSet.[wec].Sleep, this.ConvoySet.[convoy].DeactivateGroup :> Mcu.McuBase
                for curr, next in this.Path do
                    let convoy = get this.ConvoyAtWaypoint curr
                    let convoy2 = get this.ConvoyAtWaypoint next
                    //  When leader damaged, delete copy at following waypoint
                    yield this.ConvoySet.[convoy].LeadCarDamaged, this.ConvoySet.[convoy2].DeleteLeadCar :> Mcu.McuBase
                    //  Increase destroyed counter when lead car damaged
                    yield this.ConvoySet.[convoy].LeadCarDamaged, upcast this.Api.Destroyed
                    //  When leader deleted at some waypoint, delete at following waypoint too.
                    yield this.ConvoySet.[convoy].DeleteLeadCar, this.ConvoySet.[convoy2].DeleteLeadCar :> Mcu.McuBase
                    for _, pos, truck in filter3 this.TruckInConvoy (Some convoy, None, None) do
                        for _, _, truck2 in filter3 this.TruckInConvoy (Some convoy2, Some pos, None) do
                            //  When truck damaged, delete copy at following waypoint.
                            yield this.TruckInConvoySet.[truck].Damaged, this.TruckInConvoySet.[truck2].Delete :> Mcu.McuBase
                            //  When truck deleted, delete copy at following waypoint.
                            yield this.TruckInConvoySet.[truck].Delete, this.TruckInConvoySet.[truck2].Delete :> Mcu.McuBase
                // When truck damaged, notify of death
                for _, _, truck in this.TruckInConvoy do
                    let note = get this.TruckDamagedOfTruckInConvoy truck
                    let truck = this.TruckInConvoySet.[truck]
                    let note = this.TruckDamagedSet.[note]
                    yield truck.Damaged, upcast note.Trigger
                for wp, convoy in this.ConvoyAtWaypoint do
                    let wec = get this.WhileEnemyCloseOfConvoy convoy
                    let wec = this.WhileEnemyCloseSet.[wec]
                    let wpData = this.ActiveWaypointSet.[wp]
                    //  Start monitoring when waypoint becomes active (except for the last waypoint)
                    if not(this.PathEnd.Contains(wp)) then
                        yield wpData.Activate, wec.StartMonitoring :> Mcu.McuBase
                    //  Stop monitoring when waypoint becomes inactive
                    yield wpData.Deactivate, wec.StopMonitoring :> Mcu.McuBase
                for curr, next in this.Path do
                    let curr2 = this.ActiveWaypointSet.[curr]
                    let next2 = this.ActiveWaypointSet.[next]
                    yield curr2.Waypoint :> Mcu.McuTrigger, next2.Waypoint :> Mcu.McuBase
                    for _, _, timer in filter3 this.TimerBetweenWaypoints (Some curr, Some next, None) do
                        let timer = this.TimerSet.[timer]
                        //  Activate next waypoint when virtual travel time elapsed, and deactivate current waypoint.
                        yield timer.Elapsed, next2.Activate :> Mcu.McuBase
                        yield timer.Elapsed, curr2.Deactivate :> Mcu.McuBase
                        yield curr2.Activate, timer.Start :> Mcu.McuBase
                for curr, next in this.Path do
                    //  When active waypoint physically reached, lead to next waypoint.
                    yield upcast this.ActiveWaypointSet.[curr].Waypoint, upcast this.ActiveWaypointSet.[next].Waypoint
                    let currWec = get this.EnemyCloseAtWaypoint curr
                    let currWec = this.WhileEnemyCloseSet.[currWec]
                    //  Prevent showing convoys at following waypoints while convoy at current waypoint is shown.
                    for wp in star this.Path next do
                        let afterWec = get this.EnemyCloseAtWaypoint wp
                        let afterWec = this.WhileEnemyCloseSet.[afterWec]
                        yield currWec.WakeUp, afterWec.Deactivate :> Mcu.McuBase
                        yield currWec.Sleep, afterWec.Activate :> Mcu.McuBase
                //  Activate first waypoint upon API start
                for wp in this.PathStart do
                    yield this.Api.Start, this.ActiveWaypointSet.[wp].Activate :> Mcu.McuBase
                //  TODO: remove, not needed.
                for wp in this.PathEnd do
                    for start in this.InvasionStart do
                        yield this.ActiveWaypointSet.[wp].Waypoint :> Mcu.McuTrigger, this.SimpleWaypointSet.[start] :> Mcu.McuBase
                // Increase destroyed counter when truck damaged
                for convoy, _, truck in this.TruckInConvoy do
                    yield this.TruckInConvoySet.[truck].Damaged, this.Api.Destroyed :> Mcu.McuBase
                for wp in this.PathEnd do
                    let finish = this.ActiveWaypointSet.[wp]
                    // Notify of arrival when last waypoint reached (physically)
                    yield finish.Waypoint :> Mcu.McuTrigger, this.Api.Arrived :> Mcu.McuBase
                    // Notify of arrival when last waypoint reached (virtually)
                    yield finish.Activate, this.Api.Arrived :> Mcu.McuBase
                //  First invasion waypoint triggered when last active waypoint reached (virtually or physically)
                for border in this.PathEnd do
                    for enter in this.InvasionStart do
                        match tryGet this.ConvoyAtWaypoint border with
                        | Some convoy ->
                            let convoy = this.ConvoySet.[convoy]
                            let enter = this.SimpleWaypointSet.[enter]
                            yield convoy.TriggerGates, upcast enter
                        | None ->
                            ()
                for kvp in this.AtDestinationSet do
                    let api = this.Api
                    match kvp.Key with
                    | TruckAtDestinationInstance(truck) ->
                        let note = get this.TruckDamagedOfTruckInConvoy truck
                        //  Log arrival of truck WHEN LEADER ARRIVES, unless truck destroyed
                        let truck = this.TruckInConvoySet.[truck]
                        if this.InvasionEnd.IsEmpty then
                            yield api.Arrived, upcast kvp.Value.Trigger
                        else
                            yield api.Captured, upcast kvp.Value.Trigger
                        yield truck.Delete, upcast kvp.Value.Disable
                    | LeadCarAtDestinationInstance(convoy) ->
                        //  Log arrival of leader, unless it has been damaged.
                        let car = this.ConvoySet.[convoy]
                        if this.InvasionEnd.IsEmpty then
                            yield api.Arrived, upcast kvp.Value.Trigger
                        else
                            yield api.Captured, upcast kvp.Value.Trigger
                        yield car.DeleteLeadCar, upcast kvp.Value.Disable
                //  Stop monitoring at last travel waypoint when convoy arrives (last travel waypoint reached, physically or virtually)
                //  FIXME: is this necessary? Maybe avoid starting monitoring. Or maybe do not event include monitor for last travel waypoint.
                //  NOTE: we actually deactivate all WECs when the convoy arrives. OK, but differs from the spec.
                for wp, convoy in this.ConvoyAtWaypoint do
                    let wec = get this.WhileEnemyCloseOfConvoy convoy
                    let wec = this.WhileEnemyCloseSet.[wec]
                    let api = this.Api
                    yield api.Arrived, wec.StopMonitoring :> Mcu.McuBase
                //  Each waypoint in invasion path leads to next waypoint
                for wp1, wp2 in this.InvasionPath do
                    yield this.SimpleWaypointSet.[wp1] :> Mcu.McuTrigger, this.SimpleWaypointSet.[wp2] :> Mcu.McuBase
                for finish in this.PathEnd do
                    for prev, _ in this.Path do
                        match tryGet this.ConvoyAtWaypoint prev with
                        | Some convoy ->
                            let wp = this.ActiveWaypointSet.[finish]
                            let convoy = this.ConvoySet.[convoy]
                            // Hide non-last convoy when last travel waypoint reached (physically)
                            yield upcast wp.Waypoint, upcast convoy.DeactivateGroup
                            // Hide non-last convoy when last travel waypoint reached (virtually)
                            yield wp.Activate, upcast convoy.DeactivateGroup
                        | None -> ()
                    // Show last convoy when last travel waypoint reached (physically or virtually)
                    if not this.InvasionStart.IsEmpty then
                        match tryGet this.ConvoyAtWaypoint finish with
                        | Some convoy ->
                            let wp = this.ActiveWaypointSet.[finish]
                            let convoy = this.ConvoySet.[convoy]
                            yield upcast wp.Waypoint, upcast convoy.ActivateGroup
                            yield wp.Activate, upcast convoy.ActivateGroup
                        | None ->
                            ()
                    // Hide convoy when invasion destination reached
                    for destination in this.InvasionEnd do
                        match tryGet this.ConvoyAtWaypoint finish with
                        | Some convoy ->
                            match tryGet this.DelayBeforeDeactivation convoy with
                            | Some delay ->
                                let delay = this.DelaySet.[delay]
                                let wp = this.SimpleWaypointSet.[destination]
                                let convoy = this.ConvoySet.[convoy]
                                yield upcast wp, upcast delay.Start
                                yield delay.Elapsed, upcast convoy.DeactivateGroup
                            | None ->
                                ()
                        | None ->
                            ()
                // Notify of objective capture when last invasion waypoint reached
                for finish in this.InvasionEnd do
                    let wp = this.SimpleWaypointSet.[finish]
                    yield upcast wp, upcast this.Api.Captured

                // Notify when the convoy starts
                yield this.Api.Start, upcast this.DepartureReporting.Trigger

                // Icon
                yield this.Api.Start, upcast this.IconAttack.Show
                yield this.Api.Start, upcast this.IconCover.Show
                yield this.Api.Destroyed, upcast this.IconAttack.Hide
                yield this.Api.Destroyed, upcast this.IconCover.Hide
                if this.InvasionEnd.IsEmpty then
                    yield this.Api.Arrived, upcast this.IconAttack.Hide
                    yield this.Api.Arrived, upcast this.IconCover.Hide
                else
                    yield this.Api.Captured, upcast this.IconAttack.Hide
                    yield this.Api.Captured, upcast this.IconCover.Hide
            ]
        { Columns = columns
          Objects = objectLinks
          Targets = targetLinks
        }