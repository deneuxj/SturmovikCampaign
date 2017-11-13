module Campaign.MissionGeneration

open System.Numerics
open System.IO
open System.Collections.Generic

open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Cached

open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.StaticDefenses.Factory
open SturmovikMission.Blocks.StaticDefenses.Types
open SturmovikMission.Blocks.Train
open SturmovikMission.Blocks.Effect
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks
open SturmovikMission.Blocks.IO
open SturmovikMission.Blocks.BlocksMissionData.CommonMethods
open SturmovikMission.Blocks.TransportFlight
open SturmovikMission.Blocks.ShipConvoy
open SturmovikMission.Blocks.FireLoop
open SturmovikMission.Blocks.ParaDrop
open SturmovikMission.Blocks.WhileEnemyClose
open SturmovikMission.Blocks.MissionEnd

open VectorExtension

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Weather
open Campaign.Orders
open Campaign.Util
open Campaign.MapGraphics
open Campaign.ParkingArea
open Campaign.BasicTypes
open Campaign.PlaneModel
open SturmovikMission.Blocks.Vehicles

type ArtilleryGroup = {
    All : Mcu.McuBase list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = []
        member x.SubGroups = []

    static member Create(random : System.Random, store, lcStore, includeSearchLights, missionBegin : Mcu.McuTrigger, world : World, state : WorldState, attackingColumns : ColumnMovement list) =
        let wg = world.FastAccess
        let sg = state.FastAccess
        let regionFillLevel =
            state.GetAmmoCostPerRegion(world)
            |> Map.map (fun region cost ->
                if cost > 0.0f<E> then
                    sg.GetRegion(region).Supplies / cost
                else
                    1.0f
                |> max 0.0f
                |> min 1.0f)
        let all =
            [
                for area in world.AntiAirDefenses do
                    let numUnits =
                        regionFillLevel
                        |> Map.tryFind area.Home
                        |> Option.defaultVal 0.0f
                        |> ((*) (float32 area.MaxNumGuns))
                        |> ceil
                        |> int
                    if numUnits > 0 then
                        let owner = sg.GetRegion(area.Home).Owner
                        let country, coalition =
                            match owner with
                            | None -> failwithf "No owner found for group of anti-air defenses '%A'" area.DefenseAreaId
                            | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                            | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                        let group = StaticDefenseGroup.Create(area.Role, includeSearchLights, random, store, lcStore, area.Boundary, area.Position.Rotation, numUnits, country, coalition)
                        let links = group.CreateLinks()
                        let mcus = McuUtil.deepContentOf group
                        links.Apply(mcus)
                        Mcu.addTargetLink missionBegin group.Api.Start.Index
                        yield! mcus
            ]
        { All = all
        }

let inline createBlocksGen mkDamaged (random : System.Random) (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (inAttackArea : Vector2 -> bool) (blocks : ^T list) =
    let tryGetRegionAt(v : Vector2) =
        world.Regions
        |> List.tryFind (fun region ->
            v.IsInConvexPolygon(region.Boundary)
        )
    let getState =
        let m =
            state.Regions
            |> Seq.map (fun region -> region.RegionId, region)
            |> dict
        fun x -> m.[x]
    let getHealth (region : Region) (regionState : RegionState) (v : Vector2) =
        let afStorageWithHealth =
            List.zip world.Airfields state.Airfields
            |> List.filter (fun (af, _) -> af.Region = region.RegionId)
            |> List.collect (fun (af, afState) -> List.zip af.Storage afState.StorageHealth)
        let dist, health =
            try
                Seq.zip (region.Storage @ region.Production) (regionState.StorageHealth @ regionState.ProductionHealth)
                |> Seq.append afStorageWithHealth
                |> Seq.map (fun (block, health) -> (block.Pos.Pos - v).LengthSquared(), health)
                |> Seq.minBy fst
            with
            | _ -> 10.0f, 1.0f
        if dist < 1.0f then
            Some health
        else
            None
    let playArea =
        world.Regions
        |> List.map (fun region -> region.Boundary)
        |> List.concat
        |> convexHull
    [
        for block in blocks do
            let v = Vector2.FromPos(block)
            let subst = Mcu.substId <| store.GetIdMapper()
            let model : string = valueOf(getModel block)
            match tryGetRegionAt v with
            | None when v.IsInConvexPolygon playArea ->
                // Include all objects in the convex hull of all regions
                // This fixes a bug where bridges located in the space between two neighbouring regions were culled.
                let mcu =
                    createMcu block
                subst mcu
                yield mcu
            | None ->
                ()
            | Some region ->
                let state = getState region.RegionId
                let health = getHealth region state v
                match health with
                | None ->
                    // No strategic value, health is not tracked, show without damage
                    let mcu =
                        createMcu block
                    subst mcu
                    yield mcu
                | Some health ->
                    // Has health and strategic value, show damage if any
                    let health = float health
                    let building = StaticGroup.FromBlock block
                    let damagedBlock =
                        block
                        |> setDamaged (
                            mkDamaged (
                                let subBlocks = building.SubBlocks
                                let numSubs = List.length subBlocks |> float
                                let subDamage = 1.0 / numSubs
                                subBlocks
                                |> List.fold (fun (items, damage) sub ->
                                    if damage > 0.5 * subDamage then
                                        (sub, T.Float 1.0) :: items, damage - subDamage
                                    else
                                        items, damage
                                    ) ([], 1.0 - health)
                                |> fst
                                |> List.rev
                                |> Map.ofList))
                        |> setDurability (StaticGroup.FromBlock(block).Durability |> T.Integer)
                        |> setIndex (T.Integer 1)
                        |> setLinkTrId (T.Integer 0) // No entity
                        |> createMcu
                        :?> Mcu.HasEntity
                    match state.Owner with
                    | Some Allies ->
                        damagedBlock.Country <- Mcu.CountryValue.Russia
                    | Some Axis ->
                        damagedBlock.Country <- Mcu.CountryValue.Germany
                    | _ ->
                        ()
                    subst damagedBlock
                    // Give an entity if located in an area attacked by AIs, so that AIs will target the block.
                    if inAttackArea (Vector2.FromMcu damagedBlock.Pos) then
                        let subst2 = Mcu.substId <| store.GetIdMapper()
                        let entity = newEntity 1
                        McuUtil.vecCopy damagedBlock.Pos entity.Pos
                        McuUtil.vecCopy damagedBlock.Ori entity.Ori
                        subst2 entity
                        Mcu.connectEntity damagedBlock entity
                        yield upcast entity
                    // Result
                    yield upcast damagedBlock
    ]

let createBlocks random store world state inAttackArea (blocks : T.Block list) = createBlocksGen T.Block.Damaged random store world state inAttackArea blocks

let createBridges random store world state inAttackArea (blocks : T.Bridge list) = createBlocksGen T.Bridge.Damaged random store world state inAttackArea blocks

let createGrounds (store : NumericalIdentifiers.IdStore) (blocks : T.Ground list) =
    [
        for block in blocks do
            let subst = Mcu.substId <| store.GetIdMapper()
            let mcu =
                createMcu block
            subst mcu
            yield mcu
    ]

let createAirfieldSpawns (maxCapturedPlanes : int) (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) =
    let getOwner =
        let m =
            state.Regions
            |> Seq.map(fun region -> region.RegionId, region.Owner)
            |> dict
        fun x -> m.[x]
    [
        for airfield, state in Seq.zip world.Airfields state.Airfields do
            let subst = Mcu.substId <| store.GetIdMapper()
            match getOwner airfield.Region with
            | None -> ()
            | Some coalition ->
                let af =
                    let spawn =
                        airfield.Spawn
                        |> List.minBy(fun spawn ->
                            let chart = spawn.TryGetChart()
                            match chart with
                            | None -> System.Single.MaxValue
                            | Some chart ->
                                let points = chart.GetPoints()
                                let distance =
                                    points
                                    |> List.pick(fun p1 ->
                                        if p1.GetType().Value = 2 then
                                            let p =
                                                Vector2(float32 <| p1.GetX().Value, float32 <| p1.GetY().Value).Rotate(float32 (spawn.GetYOri().Value)) + Vector2.FromPos(spawn)
                                            Some ((p - fst state.Runway).Length())
                                        else
                                            None)
                                distance)
                        |> fun spawn ->
                            spawn
                                .SetReturnPlanes(T.Boolean true)
                                .SetRefuelFriendlies(T.Boolean true)
                                .SetRearmFriendlies(T.Boolean true)
                    match coalition with
                    | Axis ->
                        spawn.SetCountry(T.Integer(int(Mcu.CountryValue.Germany)))
                    | Allies ->
                        spawn.SetCountry(T.Integer(int(Mcu.CountryValue.Russia)))
                let planeSpecs : T.Airfield.Planes.Plane list =
                    state.NumPlanes
                    |> Map.map (fun _ number -> number |> floor |> int)
                    // Limit number of captured planes available for spawning
                    |> Util.expandMap
                    |> Array.shuffle (new System.Random())
                    |> Array.fold (fun (filtered, capturedLeft) plane ->
                        if plane.Coalition <> coalition then
                            if capturedLeft > 0 then
                                plane :: filtered, capturedLeft - 1
                            else
                                filtered, 0
                        else
                            plane :: filtered, capturedLeft
                    ) ([], maxCapturedPlanes)
                    |> fst
                    |> Util.compactSeq
                    // Create plane spec
                    |> Map.map (fun plane number ->
                        let model = plane.ScriptModel
                        newAirfieldPlane("", "", 0, 0, "", "", int number)
                            .SetScript(T.String model.Script)
                            .SetModel(T.String model.Model)
                            .SetStartInAir(T.Integer 2)
                            .SetAvPayloads(T.String(plane.LoadOuts(state.Supplies / bombCost)))
                    )
                    |> Map.toSeq
                    |> Seq.map snd
                    |> List.ofSeq
                let planes =
                    T.Airfield.Planes()
                        .SetPlane(planeSpecs)
                let afName = sprintf "%s (%d Kg)" (af.GetName().Value) (state.Supplies / bombCost |> float32 |> ceil |> int)
                let af = af.SetPlanes(planes).SetIndex(T.Integer 1).SetLinkTrId(T.Integer 2).SetName(T.String afName)
                let entity = newEntity 2
                entity.MisObjID <- 1
                let mcu = af.CreateMcu()
                subst mcu
                subst entity
                let runwayStartPos =
                    af.TryGetChart()
                    |> Option.map (fun chart -> chart.GetPoints() |> Seq.find (fun point -> point.GetType().Value = 2))
                    |> Option.map (fun point -> Vector2(point.GetX().Value |> float32, point.GetY().Value |> float32).Rotate(af.GetYOri().Value |> float32) + Vector2.FromPos(af))
                    |> Option.map (fun pos -> pos, coalition)
                match runwayStartPos with
                | Some x -> yield (x, [ mcu; upcast entity ])
                | None -> ()
    ]

let createAirCargo store lcStore (order : ResupplyOrder) (world : World) (state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let convoy = order.Convoy
    let country, coalition =
        match sg.GetRegion(convoy.Start).Owner with
        | None -> failwithf "Convoy starting from a neutral region '%A'" convoy.Start
        | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
        | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
    ()

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
                | ByRoad | ByRail | ByShip ->
                    let path =
                        let paths =
                            match order.Means with
                            | ByRail -> world.Rails
                            | ByRoad -> world.Roads
                            | ByShip -> world.SeaWays
                            | ByAir _ -> failwith "Cannot handle air cargo"
                        paths
                        |> List.tryPick(fun road -> road.MatchesEndpoints(convoy.Start, convoy.Destination))
                        |> Option.map (fun path -> path.Value)
                    match path with
                    | Some path ->
                        let pathVertices =
                            path
                            |> List.map (fun (v, yori, side) ->
                                { Pos = v
                                  Ori = yori
                                  Radius = 10000
                                  Speed = 50
                                  Priority = 1
                                  SpawnSide = side
                                }
                            )
                        let convoySpeed = // km/h
                            match order.Means with
                            | ByRoad -> 50.0f
                            | ByRail -> 70.0f
                            | ByShip -> 8.0f
                            | ByAir _ -> 300.0f
                        let targetTravelTime = 1.0f // hours
                        let pathVertices =
                            let rec takeUntilTargetDuration (time, prev) waypoints  =
                                if time < 0.0f then
                                    []
                                else
                                    match waypoints with
                                    | [] -> []
                                    | (wp : PathVertex) :: rest ->
                                        match prev with
                                        | Some (prev : Vector2)->
                                            let dist = (prev - wp.Pos).Length()
                                            let t = dist / convoySpeed
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
                            | ByShip ->
                                ShipConvoy.Create(store, lcStore, pathVertices |> List.map (fun x -> x.Pos, x.Ori), country, convoyName)
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
                        yield pathVertices.Head.Pos, virtualConvoy
                    | None ->
                        ()
                | ByAir(afStart, afDestination) ->
                    let airName = order.MissionLogEventName
                    let startPos, startDir = sg.GetAirfield(afStart).Runway
                    let landPos, landDir = sg.GetAirfield(afDestination).Runway
                    let flight = TransportFlight.Create(store, lcStore, startPos, startDir, landPos, landDir, country, airName)
                    yield startPos, Choice4Of4 flight
        ]
    let nodes =
        let positions =
            convoys
            |> List.map fst
        PriorityList.NodeList.Create(store, positions)
    nodes, convoys |> List.map snd


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
                    world.Roads
                    |> Seq.tryPick (fun path -> path.MatchesEndpoints(order.Start, order.Destination))
                    |> Option.map (fun x -> x.Value)
                match path with
                | Some path ->
                    let toVertex(v, yori, side) =
                        { Pos = v
                          Ori = yori
                          Speed = 20
                          Radius = 100
                          Priority = 1
                          SpawnSide = side
                        }
                    let travel =
                        path
                        |> List.map toVertex
                    let expectedTravelTime =
                        let speed =
                            20000.0f / 3600.0f
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
                    yield McuUtil.groupFromList [ initialDelay ]
                    let prevStart = ref initialDelay
                    let rankOffset = ref 0
                    for composition in splitCompositions random order.Composition |> List.truncate maxColumnSplit do
                        let columnContent =
                            composition
                            |> List.ofArray
                            |> List.map (fun vehicleType -> vehicleType.GetModel(coalition, true))
                        let columnName = order.MissionLogEventName
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
                | None -> ()
            | None ->
                ()
    ]


let createParkedPlanes store (world : World) (state : WorldState) inAttackArea =
    let mkParkedPlane(model : PlaneModel, pos : OrientedPosition, country) =
        let modelScript = model.StaticScriptModel
        let mcus =
            let durability = 1500
            if inAttackArea pos.Pos then
                let block, entity = newBlockWithEntityMcu store country modelScript.Model modelScript.Script durability
                [ block; upcast entity ]
            else
                [ newBlockMcu store country modelScript.Model modelScript.Script durability ]
        let p = McuUtil.newVec3(float pos.Pos.X, 0.0, float pos.Pos.Y)
        let ori = McuUtil.newVec3(0.0, float pos.Rotation, 0.0)
        for mcu in mcus do
            McuUtil.vecCopy p mcu.Pos
            McuUtil.vecCopy ori mcu.Ori
        mcus

    let wg = world.FastAccess
    let sg = state.FastAccess

    [
        for afs in state.Airfields do
            let af = wg.GetAirfield afs.AirfieldId
            let reg = sg.GetRegion af.Region
            match reg.Owner with
            | Some coalition ->
                let country = coalition.ToCountry
                let fighterPlaces = ref af.ParkedFighters
                let attackerPlaces = ref af.ParkedAttackers
                let bomberPlaces = ref af.ParkedBombers
                for (model, qty) in afs.NumPlanes |> Map.toSeq do
                    let parking =
                        match model.PlaneType with
                        | PlaneType.Fighter -> fighterPlaces
                        | PlaneType.Attacker -> attackerPlaces
                        | PlaneType.Bomber | PlaneType.Transport -> bomberPlaces
                    let positions =
                        List.truncate (int qty) parking.Value
                    parking :=
                        try
                            List.skip (int qty) parking.Value
                        with
                        | _ -> []
                    yield!
                        positions
                        |> List.collect (fun pos -> mkParkedPlane(model, pos, int country))
            | None ->
                ()
    ]

/// Set the country of entity owners in a list of Mcus depending on the region where they are located.
let setCountries (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (group : Mcu.McuBase list) =
    let subst = Mcu.substId <| store.GetIdMapper()
    for mcu in group do
        subst mcu
    for mcu in group do
        match mcu with
        | :? Mcu.HasEntity as flag ->
            let pos = Vector2.FromMcu(flag.Pos)
            let owner =
                List.zip world.Regions state.Regions
                |> List.tryPick (fun (region, regState) ->
                    if pos.IsInConvexPolygon region.Boundary then
                        regState.Owner
                    else
                        None
                )
                |> function
                    | Some Axis -> Mcu.CountryValue.Germany
                    | Some Allies -> Mcu.CountryValue.Russia
                    | None -> Mcu.CountryValue.Russia
            flag.Country <- owner
        | _ ->
            ()

let createParkedTanks store (world : World) (state : WorldState) inAttackArea (orders : OrderPackage) (coalition : CoalitionId) =
    let country = coalition.ToCountry |> int
    [
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && not(List.isEmpty region.Parking) then
                let subtracted =
                    orders.Columns
                    |> List.filter (fun order -> order.Start = region.RegionId)
                    |> List.map (fun order -> order.Composition)
                    |> Array.concat
                    |> compactSeq
                let parked =
                    subMaps regState.NumVehicles subtracted
                    |> expandMap
                    |> Array.shuffle (System.Random())
                let parkingPositions = computeParkingPositions region.Parking parked.Length
                if parked.Length > 0 then
                    let mutable x0 = System.Single.PositiveInfinity
                    let mutable x1 = System.Single.NegativeInfinity
                    let mutable z0 = System.Single.PositiveInfinity
                    let mutable z1 = System.Single.NegativeInfinity
                    for vehicle, pos in Seq.zip parked parkingPositions do
                        x0 <- min x0 pos.X
                        x1 <- max x1 pos.X
                        z0 <- min z0 pos.Y
                        z1 <- max z1 pos.Y
                        let model =
                            match vehicle, coalition with
                            | HeavyTank, Axis -> Vehicles.vehicles.GermanStaticHeavyTank
                            | MediumTank, Axis -> Vehicles.vehicles.GermanStaticMediumTank
                            | LightArmor, Axis -> Vehicles.vehicles.GermanStaticLightArmor
                            | HeavyTank, Allies -> Vehicles.vehicles.RussianStaticHeavyTank
                            | MediumTank, Allies -> Vehicles.vehicles.RussianStaticMediumTank
                            | LightArmor, Allies -> Vehicles.vehicles.RussianStaticLightArmor
                        let position =
                            newBlockMcu store country Vehicles.vehicles.TankPosition.Model Vehicles.vehicles.TankPosition.Script 3000
                        let mcus =
                            if inAttackArea pos then
                                let block, entity = newBlockWithEntityMcu store country model.Model model.Script vehicle.Durability
                                [ block; upcast entity ]
                            else
                                [ newBlockMcu store country model.Model model.Script vehicle.Durability ]
                        let mcus = position :: mcus
                        for mcu in mcus do
                            pos.AssignTo mcu.Pos
                        yield! mcus
                    let x0 = x0 - maxParkingSpacing
                    let x1 = x1 + maxParkingSpacing
                    let z0 = z0 - maxParkingSpacing
                    let z1 = z1 + maxParkingSpacing
                    // fuel storage north and south of the group
                    let pos = Vector2(x1, 0.5f * (z0 + z1))
                    let block = newBlockMcu store country Vehicles.vehicles.Fuel.Model Vehicles.vehicles.Fuel.Script 1000
                    pos.AssignTo block.Pos
                    yield block
                    let pos = Vector2(x0, 0.5f * (z0 + z1))
                    let block = newBlockMcu store country Vehicles.vehicles.Fuel.Model Vehicles.vehicles.Fuel.Script 1000
                    pos.AssignTo block.Pos
                    yield block
                    // towers at the four corners
                    for x in [ x0; x1 ] do
                        for z in [ z0; z1 ] do
                            let pos = Vector2(x, z)
                            let block = newBlockMcu store country Vehicles.vehicles.Tower.Model Vehicles.vehicles.Tower.Script 2000
                            pos.AssignTo block.Pos
                            yield block
    ]

let createLandFires (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (missionBegin : Mcu.McuTrigger) (group : Mcu.McuBase list) =
    let subst = Mcu.substId <| store.GetIdMapper()
    for mcu in group do
        subst mcu
    let areClose (mcu1 : Mcu.McuBase) (mcu2 : Mcu.McuBase) =
        let v1 = Vector2.FromMcu(mcu1.Pos)
        let v2 = Vector2.FromMcu(mcu2.Pos)
        (v1 - v2).Length() < 750.0f
    let parted =
        Algo.computePartition areClose group
    [
        for grp in parted do
            match grp with
            | [] -> ()
            | hd :: _ ->
                let pos = Vector2.FromMcu(hd.Pos)
                let owner =
                    List.zip world.Regions state.Regions
                    |> List.tryPick(fun (region, regState) ->
                        if pos.IsInConvexPolygon region.Boundary then
                            regState.Owner
                        else
                            None
                    )
                match owner with
                | None -> ()
                | Some owner ->
                    yield McuUtil.groupFromList grp
                    let coalition = owner.Other.ToCoalition
                    // Actually looking for friendlies, not enemies. It works the same, even though the name is misleading.
                    let wec = WhileEnemyClose.Create(true, true, store, pos, coalition)
                    yield wec.All
                    Mcu.addTargetLink missionBegin wec.StartMonitoring.Index
                    for mcu in grp do
                        match mcu with
                        | :? Mcu.McuEntity as entity ->
                            let startStop = Effect.EffectControl.Create(store, pos)
                            Mcu.addObjectLink startStop.Start entity.Index
                            Mcu.addObjectLink startStop.Stop entity.Index
                            Mcu.addTargetLink wec.WakeUp startStop.Start.Index
                            Mcu.addTargetLink wec.Sleep startStop.Stop.Index
                            yield startStop.All
                        | _ -> ()
    ]

let createLandLights(store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (missionBegin : Mcu.McuTrigger) (runwayStarts : (Vector2 * CoalitionId) list) (landLights : Mcu.McuBase list) =
    let lightsOn(lights : Mcu.McuEntity list) =
        let subst = Mcu.substId <| store.GetIdMapper()
        let prio = T.Integer 0
        let lowPrio = T.MCU_CMD_ForceComplete(T.String "Switch lights on", T.Integer 1, T.String "LightsOn", T.VectorOfIntegers[], prio, T.VectorOfIntegers[], T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0).CreateMcu() :?> Mcu.McuTrigger
        subst lowPrio
        for light in lights do
            Mcu.addObjectLink lowPrio light.Index
        let pos =
            let sum =
                lights
                |> List.sumBy (fun light -> Vector2.FromMcu light.Pos)
            sum / (float32 lights.Length)
        pos.AssignTo(lowPrio.Pos)
        Mcu.addTargetLink missionBegin lowPrio.Index
        lowPrio

    let subst = Mcu.substId <| store.GetIdMapper()
    for mcu in landLights do
        subst mcu

    [
        for mcu in landLights do
            match mcu with
            | :? Mcu.HasEntity as light when light.Name = "LandLight" ->
                let entity =
                    try
                        McuUtil.getEntityByIndex light.LinkTrId landLights
                    with _ ->
                        failwith "land lights must all have entities"
                let lightPos = Vector2.FromMcu light.Pos
                let runwayStart =
                    runwayStarts
                    |> List.tryFind (fun (pos, owner) -> (pos - lightPos).Length() < 200.0f)
                match runwayStart with
                | Some(_, owner) ->
                    light.Country <- owner.ToCountry
                    match owner with
                    | Allies ->
                        light.Model <- vehicles.RussianLandLight.Model
                        light.Script <- vehicles.RussianLandLight.Script
                    | Axis ->
                        light.Model <- vehicles.GermanLandLight.Model
                        light.Script <- vehicles.GermanLandLight.Script
                    let prioNode = lightsOn([entity])
                    yield light :> Mcu.McuBase
                    yield upcast entity
                    yield upcast prioNode
                | None ->
                    ()
            | _ ->
                ()
    ]

let createBuildingFires store (world : World) (state : WorldState) (windDirection : float32) =
    let maxFires = 20
    let fires = state.FirePositions(world, maxFires)
    [
        for pos, alt, size in fires do
            if size >= smallDamage then
                let fireType =
                    if size >= bigDamage then
                        FireType.CityFire
                    elif size >= mediumDamage then
                        FireType.CityFireSmall
                    else
                        FireType.VillageSmoke
                yield FireLoop.Create(store, pos, alt, windDirection, fireType)
    ]

let createParaTrooperDrops (world : World) store lcStore (battlefields : (DefenseAreaId * CoalitionId) seq) =
    let wg = world.FastAccess
    battlefields
    |> Seq.map (fun (bf, defending) ->
        let bf = wg.GetAntiTankDefenses(bf)
        [ ParaDrop.Create(store, lcStore, bf.DefensePos, defending.ToCountry, "D-" + string bf.Home)
          ParaDrop.Create(store, lcStore, bf.AttackPos, defending.Other.ToCountry, "A-" + string bf.Home) ])
    |> List.concat

let addMultiplayerPlaneConfigs (planeSet : PlaneModel.PlaneSet) (options : T.Options) =
    let configs =
        PlaneModel.AllModels(planeSet)
        |> List.map (fun model -> T.String(model.ScriptModel.Script))
    options.SetMultiplayerPlaneConfig(configs)

type MissionGenerationParameters = {
    PlaneSet : PlaneSet
    MaxCapturedPlanes : int
    Author : string
    MissionName : string
    Briefing : string
    MissionLength : int
    ColumnSplitInterval : int
    MaxSimultaneousConvoys : int
    MaxSimultaneousFerryFlights : int
    MaxVehiclesInBattle : int
    StrategyMissionFile : string
}

type MissionData = {
    World : World
    Random : System.Random
    Weather : WeatherState
    State : WorldState
    AxisOrders : OrderPackage
    AlliesOrders : OrderPackage
}

let writeMissionFile (missionParams : MissionGenerationParameters) (missionData : MissionData) (filename : string) =
    let wg = WorldFastAccess.Create(missionData.World)
    let strategyMissionData = T.GroupData(Parsing.Stream.FromFile missionParams.StrategyMissionFile)
    let options = strategyMissionData.ListOfOptions.Head
    let store = NumericalIdentifiers.IdStore()
    let lcStore = NumericalIdentifiers.IdStore()
    lcStore.SetNextId 3
    let getId = store.GetIdMapper()
    let missionBegin = newMissionBegin (getId 1)
    let includeSearchLights =
        missionData.State.Date.Hour <= 8 || missionData.State.Date.Hour + missionParams.MissionLength / 60 >= 18
    let inAttackArea(pos : Vector2) =
        missionData.AxisOrders.Attacks @ missionData.AlliesOrders.Attacks
        |> List.exists (fun attack -> (attack.Target - pos).Length() < 3000.0f)
    let staticDefenses = ArtilleryGroup.Create(missionData.Random, store, lcStore, includeSearchLights, missionBegin, missionData.World, missionData.State, missionData.AxisOrders.Columns @ missionData.AlliesOrders.Columns)
    let icons = MapIcons.CreateRegions(store, lcStore, missionData.World, missionData.State)
    let icons2 = MapIcons.CreateSupplyLevels(store, lcStore, missionData.World, missionData.State)
    let spotting = createStorageIcons store lcStore missionBegin missionData.World missionData.State
    let blocks =
        let allBlocks = strategyMissionData.ListOfBlock
        let parkedPlanes =
            strategyMissionData.GetGroup("Parked planes").CreateMcuList()
            |> List.map (fun mcu -> mcu.Index)
            |> Set.ofList
        allBlocks
        |> List.filter(fun block -> not(parkedPlanes.Contains(block.GetIndex().Value)))
        |> createBlocks missionData.Random store missionData.World missionData.State inAttackArea
    let bridges =
        strategyMissionData.ListOfBridge
        |> createBridges missionData.Random store missionData.World missionData.State inAttackArea
    let ground =
        strategyMissionData.ListOfGround
        |> createGrounds store
    let spawns = createAirfieldSpawns missionParams.MaxCapturedPlanes store missionData.World missionData.State
    let mkConvoyNodes orders =
        let convoyPrioNodes, convoys = createConvoys store lcStore missionData.World missionData.State orders
        for node, convoy in List.zip convoyPrioNodes.Nodes convoys do
            let start, destroyed, arrived =
                match convoy with
                | Choice1Of4 trucks ->
                    trucks.Api.Start, trucks.Api.Destroyed, trucks.Api.Arrived
                | Choice2Of4 train ->
                    train.TheTrain.Start, train.TheTrain.Killed, train.TheTrain.Arrived
                | Choice3Of4 ships ->
                    ships.Start, ships.Killed, ships.Arrived
                | Choice4Of4 flight ->
                    flight.Start, flight.Killed, flight.Arrived
            Mcu.addTargetLink node.Do start.Index
            Mcu.addTargetLink destroyed convoyPrioNodes.Try.Index
            Mcu.addTargetLink arrived convoyPrioNodes.Try.Index
        for i, node in Seq.indexed convoyPrioNodes.Nodes do
            if i < missionParams.MaxSimultaneousConvoys then
                Mcu.addTargetLink missionBegin node.Do.Index
            else
                Mcu.addTargetLink missionBegin node.Enable.Index
        let convoys : McuUtil.IMcuGroup list =
            convoys
            |> List.map (
                function
                | Choice1Of4 x -> x :> McuUtil.IMcuGroup
                | Choice2Of4 x -> x :> McuUtil.IMcuGroup
                | Choice3Of4 x -> x.All
                | Choice4Of4 x -> x.All)
        convoyPrioNodes.All, convoys
    let mkColumns orders =
        let maxColumnSplit = max 1 (missionParams.MissionLength / missionParams.ColumnSplitInterval - 1)
        orders
        |> createColumns missionData.Random store lcStore missionData.World missionData.State missionBegin (60.0 * float missionParams.ColumnSplitInterval) maxColumnSplit missionParams.MissionLength
    let columns = mkColumns (missionData.AxisOrders.Columns @ missionData.AlliesOrders.Columns)
    let arrows =
        [Axis; Allies]
        |> List.map (fun coalition -> MapGraphics.MapIcons.CreateArrows(store, lcStore, missionData.World, missionData.State, missionData.AxisOrders, missionData.AlliesOrders, coalition))
        |> List.map (fun icons -> icons :> McuUtil.IMcuGroup)
    let battles =
        Battlefield.generateBattlefields missionParams.MaxVehiclesInBattle missionData.Random store lcStore missionData.World missionData.State
    for bf in battles do
        for start in bf.Starts do
            Mcu.addTargetLink missionBegin start.Index
    let battles = battles |> List.map (fun bf -> bf.All)
    let paraDrops =
        createParaTrooperDrops missionData.World store lcStore (Battlefield.identifyBattleAreas missionData.World missionData.State)
        |> List.map (fun p -> p.All)
    let parkedPlanes =
        createParkedPlanes store missionData.World missionData.State inAttackArea
        |> McuUtil.groupFromList
    let parkedTanks =
        createParkedTanks store missionData.World missionData.State inAttackArea missionData.AxisOrders Axis @ createParkedTanks store missionData.World missionData.State inAttackArea missionData.AlliesOrders Allies
        |> McuUtil.groupFromList
    let axisPrio, axisConvoys = mkConvoyNodes missionData.AxisOrders.Resupply
    let alliesPrio, alliesConvoys = mkConvoyNodes missionData.AlliesOrders.Resupply
    let flags = strategyMissionData.GetGroup("Windsocks").CreateMcuList()
    setCountries store missionData.World missionData.State flags
    let ndbs = strategyMissionData.GetGroup("NDBs").CreateMcuList()
    setCountries store missionData.World missionData.State ndbs
    let ndbIcons =
        ndbs
        |> List.choose (fun ndb ->
            match ndb with
            | :? Mcu.HasEntity as ndb ->
                let coalition =
                    match ndb.Country with
                    | Mcu.CountryValue.Germany -> Mcu.CoalitionValue.Axis
                    | Mcu.CountryValue.Russia -> Mcu.CoalitionValue.Allies
                    | _ -> Mcu.CoalitionValue.Neutral
                let icon =
                    IconDisplay.IconDisplay.Create(store, lcStore, Vector2.FromMcu ndb.Pos, "NDB", coalition, Mcu.IconIdValue.Waypoint)
                Mcu.addTargetLink missionBegin icon.Show.Index
                Some icon.All
            | _ ->
                None)
    let landFires =
        if includeSearchLights then
            strategyMissionData.GetGroup("Land fires").CreateMcuList()
            |> createLandFires store missionData.World missionData.State missionBegin
        else
            []
    let landlights =
        if includeSearchLights then
            strategyMissionData.GetGroup("Land lights").CreateMcuList()
            |> createLandLights store missionData.World missionData.State missionBegin (spawns |> List.map fst)
        else
            []
    let allPatrols =
        let axisPatrols =
            missionData.AxisOrders.Patrols |> List.map (fun patrol -> patrol.ToPatrolBlock(store, lcStore))
        let alliesPatrols =
            missionData.AlliesOrders.Patrols |> List.map (fun patrol -> patrol.ToPatrolBlock(store, lcStore))
        [
            for allMcus, blocks in axisPatrols @ alliesPatrols do
                for block in blocks do
                    Mcu.addTargetLink missionBegin block.Start.Index
                yield allMcus
        ]
    let allAttacks =
        let axisAttacks =
            missionData.AxisOrders.Attacks |> List.map (fun attack -> attack.ToPatrolBlock(store, lcStore))
        let alliesAttacks =
            missionData.AlliesOrders.Attacks |> List.map (fun attack -> attack.ToPatrolBlock(store, lcStore))
        let mkAttackStarts (attacks : (_ * GroundAttack.Attacker list) list) =
            // Spawn "wing" immediately after "leader"
            for (_, blocks) in attacks do
                for b1, b2 in Seq.pairwise blocks do
                    Mcu.addTargetLink b1.Spawned b2.ImmediateStart.Index
            // Spawn pairs one minute after previous pair
            for (_, block1), (_, block2) in Seq.pairwise attacks do
                match block1, block2 with
                | b1 :: _, b2 :: _ ->
                    Mcu.addTargetLink b1.Spawned b2.DelayedStart.Index
                | _, _ ->
                    failwith "Expected at least one block in each attack list"
            // Start first pair one minute after mission start
            match attacks with
            | (_, hd :: _) :: _ ->
                Mcu.addTargetLink missionBegin hd.DelayedStart.Index
            | _ -> ()
        mkAttackStarts axisAttacks
        mkAttackStarts alliesAttacks
        axisAttacks @ alliesAttacks |> List.map fst
    let axisPlaneFerries =
        missionData.AxisOrders.PlaneFerries
        |> PlaneFerry.generatePlaneTransfer store lcStore missionData.World missionData.State missionBegin missionParams.MaxSimultaneousFerryFlights
    let alliesPlaneFerries =
        missionData.AlliesOrders.PlaneFerries
        |> PlaneFerry.generatePlaneTransfer store lcStore missionData.World missionData.State missionBegin missionParams.MaxSimultaneousFerryFlights
    let buildingFires =
        createBuildingFires store missionData.World missionData.State (float32 missionData.Weather.Wind.Direction)
        |> List.map (fun fire -> fire.All)
    let options =
        (Weather.setOptions missionData.Weather missionData.State.Date options)
            .SetMissionType(T.Integer 2) // deathmatch
            |> addMultiplayerPlaneConfigs missionParams.PlaneSet
    let optionStrings =
        { new McuUtil.IMcuGroup with
              member x.Content = []
              member x.LcStrings =
                [ (0, missionParams.MissionName)
                  (1, missionParams.Briefing)
                  (2, missionParams.Author)
                ]
              member x.SubGroups = []
        }
    let serverInputMissionEnd = MissionEnd.Create(store)
    let allGroups =
        [ optionStrings
          McuUtil.groupFromList [missionBegin]
          upcast staticDefenses
          upcast icons
          upcast icons2
          McuUtil.groupFromList blocks
          McuUtil.groupFromList bridges
          McuUtil.groupFromList ground
          McuUtil.groupFromList (spawns |> List.collect snd)
          McuUtil.groupFromList flags
          McuUtil.groupFromList ndbs
          McuUtil.groupFromList landlights
          parkedPlanes
          parkedTanks
          axisPrio
          alliesPrio
          axisPlaneFerries
          alliesPlaneFerries
          serverInputMissionEnd.All ] @ axisConvoys @ alliesConvoys @ spotting @ landFires @ arrows @ allPatrols @ allAttacks @ buildingFires @ columns @ battles @ paraDrops @ ndbIcons
    writeMissionFiles "eng" filename options allGroups