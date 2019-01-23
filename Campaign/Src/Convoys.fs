// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Campaign.Convoys

open System.Collections.Generic
open SturmovikMission.DataProvider
open System.Numerics
open VectorExtension
open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.Blocks.Train
open SturmovikMission.Blocks.ShipConvoy
open SturmovikMission.Blocks
open SturmovikMission.Blocks.TransportFlight
open SturmovikMission.Blocks.BlocksMissionData

open Util

open Campaign.BasicTypes
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders


/// Data from resupply and column orders needed to build the travel path
type MovementOrder =
    { Start : RegionId
      Destination : RegionId
      Transport : ColumnTransportType
      OriginalOrder : Choice<ResupplyOrder, ColumnMovement> }
with
    static member FromResupplies(orders : ResupplyOrder seq) =
        orders
        |> Seq.choose(fun order ->
            match order.Means with
            | ByRoad ->
                Some { Start = order.Convoy.Start; Destination = order.Convoy.Destination; Transport = ColByRoad; OriginalOrder = Choice1Of2 order }
            | ByRiverShip ->
                Some { Start = order.Convoy.Start; Destination = order.Convoy.Destination; Transport = ColByRiverShip; OriginalOrder = Choice1Of2 order }
            | BySeaShip ->
                Some { Start = order.Convoy.Start; Destination = order.Convoy.Destination; Transport = ColBySeaShip; OriginalOrder = Choice1Of2 order }
            | ByRail ->
                Some { Start = order.Convoy.Start; Destination = order.Convoy.Destination; Transport = ColByTrain; OriginalOrder = Choice1Of2 order }
            | ByAir _ ->
                None
        )

    static member FromColumns(orders : ColumnMovement seq) =
        orders
        |> Seq.map(fun order ->
            { Start = order.Start
              Destination = order.Destination
              Transport = order.TransportType
              OriginalOrder = Choice2Of2 order})

/// Shorten paths so that travel lasts about 1h.
let getMovementPathVertices (world : World) (state : WorldState) (orders : MovementOrder seq) =
    let wg = world.FastAccess
    let fullPaths =
        seq {
            for order in orders do
                let paths =
                    match order.Transport with
                    | ColByTrain -> world.Rails
                    | ColByRoad -> world.Roads
                    | ColBySeaShip -> world.SeaWays
                    | ColByRiverShip -> world.RiverWays
                let fullPath =
                    paths
                    |> List.tryPick(fun road -> road.MatchesEndpoints(order.Start, order.Destination))
                    |> Option.map (fun path -> path.Value)
                match fullPath with
                | Some x -> yield (order, x)
                | None -> ()
        }
    let shortenedPaths =
        seq {
            for order, path in fullPaths do
                let convoySpeed = // km/h
                    match order.Transport with
                    | ColByRoad -> 50
                    | ColByTrain -> 70
                    | ColByRiverShip -> 5
                    | ColBySeaShip -> 8
                let pathVertices =
                    path
                    |> List.map (fun v ->
                        { v with
                            Radius = 100
                            Speed = convoySpeed
                            Priority = 1
                        })
                let targetTravelTime = 1.0f // hours
                let pathVertices =
                    match order.Transport with
                    | ColByRiverShip
                    | ColBySeaShip ->
                        let rec takeUntilTargetDuration (time, prev) waypoints  =
                            if time < 0.0f then
                                []
                            else
                                match waypoints with
                                | [] -> []
                                | (wp : PathVertex) :: rest ->
                                    match prev with
                                    | Some (prev : Vector2)->
                                        let dist = (prev - wp.Pos).Length() / 1000.0f
                                        let t = dist / float32 wp.Speed
                                        wp :: takeUntilTargetDuration (time - t, Some wp.Pos) rest 
                                    | None ->
                                        wp :: takeUntilTargetDuration (time, Some wp.Pos) rest
                        let rec trails xs =
                            seq {
                                yield xs
                                match xs with
                                | _ :: xs ->
                                    yield! trails xs
                                | [] ->
                                    ()
                            }
                        let destinationZone = wg.GetRegion(order.Destination).Boundary
                        pathVertices
                        |> trails
                        |> Seq.map (takeUntilTargetDuration (targetTravelTime, None))
                        |> Seq.tryFind (fun path ->
                            // Goes into destination region?
                            match Seq.tryLast path with
                            | Some ep ->
                                ep.Pos.IsInConvexPolygon(destinationZone)
                            | None ->
                                false)
                        |> Option.defaultVal pathVertices
                    | _ -> pathVertices
                yield order.OriginalOrder, pathVertices
        }
    List.ofSeq shortenedPaths

/// Select bridges along paths, assigning bridges to vertices
/// Note that one bridge may belong to multiple vertices, and one vertex may have multiple bridges.
let selectBridgesAlongPaths (bridges : Mcu.HasEntity list) (paths : ('TChoice * PathVertex list) seq) =
    match bridges |> List.filter (fun bridge -> bridge.Model.Contains("bridge_road_") |> not) with
    | [] -> []
    | goodBridges ->
        // goodBridges is the list of bridges that are not affected by the "bad collision" bug some bridges when given an instance
        let bridges =
            seq {
                for origin, path in paths do
                    for v in path |> Seq.filter (fun v -> v.Role = Intermediate) do
                        let distance, nearest =
                            goodBridges
                            |> Seq.map (fun bridge -> (Vector2.FromMcu(bridge.Pos) - v.Pos).Length(), bridge)
                            |> Seq.minBy fst
                        if distance < 50.0f then
                            // Extend radius to at least 1000m, to give time to convoy to stop before reaching bridge.
                            yield { v with Radius = max 1000 v.Radius }, nearest
                    for v1, v2 in path |> Seq.pairwise |> Seq.filter (fun (v1, v2) -> v1.Role = NarrowZoneEntrance && v2.Role = NarrowZoneExit) do
                        let center = 0.5f * (v1.Pos + v2.Pos)
                        let radius = (v1.Pos - center).Length()
                        yield!
                            goodBridges
                            |> Seq.filter (fun bridge -> (Vector2.FromMcu(bridge.Pos) - center).Length() < radius)
                            |> Seq.map (fun bridge -> v1, bridge) // No need to extend radius, the waypoint is already before the bridge.
            }
            |> Seq.distinct
            |> List.ofSeq
        bridges

/// Select distinct bridges from vertex-bridge pairs, then create entities
let makeBridgeEntities (store : NumericalIdentifiers.IdStore) (verticesAndBridges : (PathVertex * Mcu.HasEntity) seq) =
    let distinctBridges =
        verticesAndBridges
        |> Seq.map snd
        |> Seq.distinct
        |> List.ofSeq

    let entities =
        seq {
            let subst = Mcu.substId <| store.GetIdMapper()
            for i, bridge in Seq.indexed distinctBridges do
                let entity = newEntity (i + 1)
                McuUtil.vecCopy bridge.Pos entity.Pos
                subst entity
                Mcu.connectEntity bridge entity
                yield bridge, entity
        }
    dict entities

/// Convoys are created according to resupply orders. A given number of convoys start at mission start, then new convoys start whenever a convoy arrives or gets destroyed.
let createConvoys (store : NumericalIdentifiers.IdStore) lcStore (world : World) (state : WorldState) (bridgeEntities : IDictionary<Mcu.HasEntity, Mcu.McuEntity>) (bridgesOfVertex : PathVertex -> Mcu.HasEntity list) (orders : (ResupplyOrder * PathVertex list) list) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let convoys =
        [
            for order, pathVertices in orders do
                let convoy = order.Convoy
                let coalition = order.OrderId.Coalition
                match order.Means with
                | ByRoad | ByRail | BySeaShip | ByRiverShip ->
                    let virtualConvoy =
                        let bridgeEntitiesAtVertex =
                            [
                                for v in pathVertices do
                                    for e in bridgesOfVertex v do
                                        match bridgeEntities.TryGetValue(e) with
                                        | true, e ->
                                            yield (v, e)
                                        | false, _ ->
                                            ()
                            ]
                        let convoyName = order.MissionLogEventName
                        match order.Means with
                        | ByRoad ->
                            let size =
                                int (convoy.TransportedSupplies / ResupplyOrder.TruckCapacity)
                                |> min ColumnMovement.MaxColumnSize
                            let withAA = not world.IsWWI
                            VirtualConvoy.Create(store, lcStore, pathVertices, bridgeEntitiesAtVertex, size, withAA, coalition.ToCountry, coalition.ToCoalition, convoyName, 0)
                            |> Choice1Of4
                        | ByRail ->
                            TrainWithNotification.Create(store, lcStore, not world.IsWWI, pathVertices, bridgeEntitiesAtVertex, coalition.ToCountry, convoyName)
                            |> Choice2Of4
                        | BySeaShip
                        | ByRiverShip ->
                            let waterType =
                                match order.Means with
                                | BySeaShip -> Sea
                                | ByRiverShip -> River
                                | _ -> failwith "Unexpected ship transport means"
                            let numShips = int <| ceil (order.Convoy.TransportedSupplies / ResupplyOrder.ShipCapacity)
                            ShipConvoy.Create(store, lcStore, numShips, waterType, pathVertices, coalition.ToCountry, convoyName)
                            |> Choice3Of4
                        | ByAir _ -> failwith "Cannot handle air cargo"
                    let links =
                        match virtualConvoy with
                        | Choice1Of4 x -> x.CreateLinks()
                        | Choice2Of4 x -> x.CreateLinks()
                        | Choice3Of4 x -> Links.Links.Empty
                        | Choice4Of4 _ -> failwith "Cannot handle air cargo"
                    let mcuGroup =
                        match virtualConvoy with
                        | Choice1Of4 x -> x :> McuUtil.IMcuGroup
                        | Choice2Of4 x -> x :> McuUtil.IMcuGroup
                        | Choice3Of4 x -> x.All
                        | Choice4Of4 _ -> failwith "Cannot handle air cargo"
                    links.Apply(McuUtil.deepContentOf mcuGroup)
                    yield order.OrderId, pathVertices.Head.Pos, virtualConvoy
                | ByAir(afStart, afDestination) ->
                    let airName = order.MissionLogEventName
                    let startPos, startDir = sg.GetAirfield(afStart).Runway
                    let landPos, landDir = sg.GetAirfield(afDestination).Runway
                    let flight = TransportFlight.Create(store, lcStore, startPos, startDir, landPos, landDir, coalition.ToCountry, airName)
                    yield order.OrderId, startPos, Choice4Of4 flight
        ]
    let nodes =
        let positions =
            convoys
            |> List.map (fun (_, x, _) -> x)
        PriorityList.NodeList.Create(store, positions)
    nodes, convoys |> List.map (fun (orderId, _, convoy) -> orderId, convoy)


let splitCompositions random vehicles =
    vehicles
    |> Array.chunkBySize ColumnMovement.MaxColumnSize
    |> List.ofArray

/// Large columns are split to fit in the maximum size of columns, and each group starts separated by a given interval.
let createColumns
        (random : System.Random)
        (store : NumericalIdentifiers.IdStore)
        lcStore
        (world : World)
        (state : WorldState)
        (missionBegin : Mcu.McuTrigger)
        interval
        maxColumnSplit
        (missionLength : int)
        (bridgeEntities : IDictionary<Mcu.HasEntity, Mcu.McuEntity>)
        (bridgesOfVertex : PathVertex -> Mcu.HasEntity list)
        (orders : (ColumnMovement * PathVertex list) list) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    [
        for order, pathVertices in orders do
            let regState = sg.GetRegion(order.Start)
            let coalition = order.OrderId.Coalition
            let expectedTravelTime =
                pathVertices
                |> Seq.pairwise
                |> Seq.sumBy (fun (v1, v2) -> (v2.Pos - v1.Pos).Length() / (3.6f * float32 v1.Speed))
            let initialDelay =
                let x = newTimer 1
                let subst = Mcu.substId <| store.GetIdMapper()
                subst x
                Mcu.addTargetLink missionBegin x.Index
                let delayValue =
                    random.NextDouble()
                    |> float32
                    |> (*) ((float32 missionLength) * 60.0f - expectedTravelTime)
                    |> max 0.0f
                x.Time <- float delayValue
                x.Name <- "Initial delay"
                match pathVertices with
                | v :: _ -> (v.Pos + Vector2(-100.0f, 0.0f)).AssignTo x.Pos
                | [] -> ()
                x
            let prevStart = ref initialDelay
            let columnName = order.MissionLogEventName
            let bridgeEntitiesAtVertex =
                [
                    for v in pathVertices do
                        for e in bridgesOfVertex v do
                            match bridgeEntities.TryGetValue(e) with
                            | true, e ->
                                yield (v, e)
                            | false, _ ->
                                ()
                ]
            match order.TransportType with
            | ColByRoad ->
                let rankOffset = ref 0
                yield
                    order.OrderId,
                    initialDelay,
                    [
                        for composition in splitCompositions random order.Composition |> List.truncate maxColumnSplit do
                            let columnContent =
                                composition
                                |> List.ofArray
                                |> List.map (fun vehicleType -> vehicleType.GetModel(coalition, true))
                            let column =
                                if world.IsWWI then
                                    VirtualConvoy.Create(store, lcStore, pathVertices, bridgeEntitiesAtVertex, List.length columnContent, false, coalition.ToCountry, coalition.ToCoalition, columnName, !rankOffset)
                                else
                                    VirtualConvoy.CreateColumn(store, lcStore, pathVertices, bridgeEntitiesAtVertex, columnContent, coalition.ToCountry, coalition.ToCoalition, columnName, !rankOffset)
                            let links = column.CreateLinks()
                            links.Apply(McuUtil.deepContentOf column)
                            Mcu.addTargetLink prevStart.Value column.Api.Start.Index
                            let beforeNext =
                                let x = newTimer 1
                                let subst = Mcu.substId <| store.GetIdMapper()
                                subst x
                                x
                            beforeNext.Time <- interval
                            Mcu.addTargetLink column.Api.Start beforeNext.Index
                            prevStart := beforeNext
                            rankOffset := rankOffset.Value + ColumnMovement.MaxColumnSize
                            yield column :> McuUtil.IMcuGroup
                            yield McuUtil.groupFromList [ beforeNext ]
                    ]
            | ColByTrain ->
                let train = TrainWithNotification.Create(store, lcStore, not world.IsWWI, pathVertices, bridgeEntitiesAtVertex, coalition.ToCountry, columnName)
                Mcu.addTargetLink prevStart.Value train.TheTrain.Start.Index
                let links = train.CreateLinks()
                links.Apply(McuUtil.deepContentOf train)
                yield order.OrderId, initialDelay, [ train :> McuUtil.IMcuGroup ]
            | ColByRiverShip
            | ColBySeaShip ->
                let convoySpeed =
                    match order.TransportType with
                    | ColBySeaShip -> 8.0f // km/h
                    | ColByRiverShip -> 5.0f
                    | _ -> failwith "Unexpected column transport type"
                let targetTravelTime = 1.0f // hours
                // select last segment of path, we want the landing party to get as close as possible to the shore
                let pathVertices =
                    let rec takeUntilTargetDuration (time, prev) waypoints =
                        if time < 0.0f then
                            []
                        else
                            match waypoints with
                            | [] -> []
                            | (wp : PathVertex) :: rest ->
                                match prev with
                                | Some (prev : Vector2)->
                                    let dist = (prev - wp.Pos).Length() / 1000.0f
                                    let t = dist / convoySpeed
                                    wp :: takeUntilTargetDuration (time - t, Some wp.Pos) rest 
                                | None ->
                                    wp :: takeUntilTargetDuration (time, Some wp.Pos) rest
                    pathVertices
                    |> List.rev
                    |> takeUntilTargetDuration (targetTravelTime, None)
                    |> List.rev
                let waterType =
                    match order.TransportType with
                    | ColBySeaShip -> Sea
                    | ColByRiverShip -> River
                    | _ -> failwith "Unexpected column transport type"
                let numShips = int <| ceil ((float32 order.Composition.Length) / (float32 shipVehicleCapacity))
                let ships = ShipConvoy.Create(store, lcStore, numShips, waterType, pathVertices, coalition.ToCountry, columnName)
                ships.MakeAsLandShips(waterType)
                Mcu.addTargetLink prevStart.Value ships.Start.Index
                yield order.OrderId, initialDelay, [ ships.All ]
    ]
