module Campaign.Convoys

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

/// Convoys are created according to resupply orders. A given number of convoys start at mission start, then new convoys start whenever a convoy arrives or gets destroyed.
let createConvoys store lcStore (world : World) (state : WorldState) (orders : ResupplyOrder list) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let convoys =
        [
            for order in orders do
                let convoy = order.Convoy
                let country, coalition =
                    match sg.GetRegion(convoy.Start).Owner with
                    | None -> failwithf "Convoy starting from a neutral region '%A'" convoy.Start
                    | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                    | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                match order.Means with
                | ByRoad | ByRail | BySeaShip | ByRiverShip ->
                    let path =
                        let paths =
                            match order.Means with
                            | ByRail -> world.Rails
                            | ByRoad -> world.Roads
                            | BySeaShip -> world.SeaWays
                            | ByRiverShip -> world.RiverWays
                            | ByAir _ -> failwith "Cannot handle air cargo"
                        paths
                        |> List.tryPick(fun road -> road.MatchesEndpoints(convoy.Start, convoy.Destination))
                        |> Option.map (fun path -> path.Value)
                    match path with
                    | Some path ->
                        let convoySpeed = // km/h
                            match order.Means with
                            | ByRoad -> 50
                            | ByRail -> 70
                            | ByRiverShip -> 5
                            | BySeaShip -> 8
                            | ByAir _ -> 300
                        let pathVertices =
                            path
                            |> List.map (fun (v, yori, side) ->
                                { Pos = v
                                  Ori = yori
                                  Radius = 100
                                  Speed = convoySpeed
                                  Priority = 1
                                  SpawnSide = side
                                }
                            )
                        let targetTravelTime = 1.0f // hours
                        let pathVertices =
                            match order.Means with
                            | ByRiverShip
                            | BySeaShip ->
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
                                let destinationZone = wg.GetRegion(convoy.Destination).Boundary
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
                        let virtualConvoy =
                            let convoyName = order.MissionLogEventName
                            match order.Means with
                            | ByRoad ->
                                let size =
                                    int (convoy.TransportedSupplies / ResupplyOrder.TruckCapacity)
                                    |> min ColumnMovement.MaxColumnSize
                                VirtualConvoy.Create(store, lcStore, pathVertices, size, country, coalition, convoyName, 0)
                                |> Choice1Of4
                            | ByRail ->
                                let startV = pathVertices.Head
                                let endV = pathVertices |> List.last
                                TrainWithNotification.Create(store, lcStore, startV.Pos, startV.Ori, endV.Pos, country, convoyName)
                                |> Choice2Of4
                            | BySeaShip
                            | ByRiverShip ->
                                let waterType =
                                    match order.Means with
                                    | BySeaShip -> Sea
                                    | ByRiverShip -> River
                                    | _ -> failwith "Unexpected ship transport means"
                                ShipConvoy.Create(store, lcStore, waterType, pathVertices, country, convoyName)
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
                    | None ->
                        ()
                | ByAir(afStart, afDestination) ->
                    let airName = order.MissionLogEventName
                    let startPos, startDir = sg.GetAirfield(afStart).Runway
                    let landPos, landDir = sg.GetAirfield(afDestination).Runway
                    let flight = TransportFlight.Create(store, lcStore, startPos, startDir, landPos, landDir, country, airName)
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
let createColumns (random : System.Random) (store : NumericalIdentifiers.IdStore) lcStore (world : World) (state : WorldState) (missionBegin : Mcu.McuTrigger) interval maxColumnSplit (missionLength : int) (orders : ColumnMovement list) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    [
        for order in orders do
            let regState = sg.GetRegion(order.Start)
            let coalition = regState.Owner
            match coalition with
            | Some coalition ->
                let path =
                    match order.TransportType with
                    | ColByRoad -> world.Roads
                    | ColByTrain -> world.Rails
                    | ColBySeaShip -> world.SeaWays
                    | ColByRiverShip -> world.RiverWays
                    |> Seq.tryPick (fun path -> path.MatchesEndpoints(order.Start, order.Destination))
                    |> Option.map (fun x -> x.Value)
                match path with
                | Some path ->
                    let convoySpeed =
                        match order.TransportType with
                        | ColByRoad -> 20
                        | ColByTrain -> 50
                        | ColByRiverShip -> 5
                        | ColBySeaShip -> 8
                    let toVertex(v, yori, side) =
                        { Pos = v
                          Ori = yori
                          Speed = convoySpeed
                          Radius = 100
                          Priority = 1
                          SpawnSide = side
                        }
                    let travel =
                        path
                        |> List.map toVertex
                    let expectedTravelTime =
                        let speed =
                            (float32 convoySpeed) / 3.6f
                        path
                        |> Seq.pairwise
                        |> Seq.sumBy (fun ((v1, _, _), (v2, _, _)) -> (v1 - v2).Length() / speed)
                        |> (*) 1.5f
                        |> (*) (ceil (float32 order.Composition.Length / float32 ColumnMovement.MaxColumnSize) |> min (float32 maxColumnSplit))
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
                        match path with
                        | (pos, _, _) :: _ -> (pos + Vector2(-100.0f, 0.0f)).AssignTo x.Pos
                        | [] -> ()
                        x
                    let prevStart = ref initialDelay
                    let columnName = order.MissionLogEventName
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
                                    let column = VirtualConvoy.CreateColumn(store, lcStore, travel, columnContent, coalition.ToCountry, coalition.ToCoalition, columnName, !rankOffset)
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
                        let train = TrainWithNotification.Create(store, lcStore, travel.Head.Pos, travel.Head.Ori, (Seq.last travel).Pos, coalition.ToCountry, columnName)
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
                            travel
                            |> List.rev
                            |> takeUntilTargetDuration (targetTravelTime, None)
                            |> List.rev
                        let waterType =
                            match order.TransportType with
                            | ColBySeaShip -> Sea
                            | ColByRiverShip -> River
                            | _ -> failwith "Unexpected column transport type"
                        let ships = ShipConvoy.Create(store, lcStore, waterType, pathVertices, coalition.ToCountry, columnName)
                        ships.MakeAsLandShips(waterType)
                        Mcu.addTargetLink prevStart.Value ships.Start.Index
                        yield order.OrderId, initialDelay, [ ships.All ]
                | None -> ()
            | None ->
                ()
    ]
