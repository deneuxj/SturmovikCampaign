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
open Vector

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Weather
open Campaign.Orders
open Campaign.Util
open Campaign.MapGraphics
open Campaign.ParkingArea

type ArtilleryGroup = {
    All : Mcu.McuBase list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = []
        member x.SubGroups = []

    static member Create(random : System.Random, store, lcStore, includeSearchLights, missionBegin : Mcu.McuTrigger, world : World, state : WorldState) =
        let getAreaState =
            let m =
                state.AntiAirDefenses @ state.AntiTankDefenses
                |> Seq.map (fun area -> area.DefenseAreaId, area)
                |> dict
            fun x -> m.[x]
        let getOwner =
            let m =
                state.Regions
                |> Seq.map (fun region -> region.RegionId, region.Owner)
                |> dict
            fun x -> m.[x]
        let all =
            [
                for area in world.AntiTankDefenses do
                    let state = getAreaState area.DefenseAreaId
                    if state.NumUnits > 0 then
                        let owner = getOwner area.Home.Home
                        let country, coalition =
                            match owner with
                            | None -> failwithf "No owner found for group of anti-tank defenses '%A'" area.DefenseAreaId
                            | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                            | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                        let group = StaticDefenseGroup.Create(AntiTank, false, random, store, lcStore, area.Boundary, area.Position.Rotation, state.NumUnits, country, coalition)
                        let links = group.CreateLinks()
                        let mcus = McuUtil.deepContentOf group
                        links.Apply(mcus)
                        Mcu.addTargetLink missionBegin group.Api.Start.Index
                        yield! mcus
                for area in world.AntiAirDefenses do
                    let state = getAreaState area.DefenseAreaId
                    if state.NumUnits > 0 then
                        let owner = getOwner area.Home.Home
                        let country, coalition =
                            match owner with
                            | None -> failwithf "No owner found for group of anti-air defenses '%A'" area.DefenseAreaId
                            | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                            | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                        let group = StaticDefenseGroup.Create(AntiAir, includeSearchLights, random, store, lcStore, area.Boundary, area.Position.Rotation, state.NumUnits, country, coalition)
                        let links = group.CreateLinks()
                        let mcus = McuUtil.deepContentOf group
                        links.Apply(mcus)
                        Mcu.addTargetLink missionBegin group.Api.Start.Index
                        yield! mcus
            ]
        { All = all
        }

let inline createBlocksGen mkDamaged (random : System.Random) (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (blocks : ^T list) =
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
        let airfields =
            world.Airfields
            |> List.filter (fun af -> af.Region = region.RegionId)
        let afStates =
            state.Airfields
            |> List.filter (fun s -> airfields |> List.exists (fun af -> af.AirfieldId = s.AirfieldId))
        let afStorageWithHealth =
            let blocks =
                airfields
                |> List.map (fun af -> af.Storage)
                |> List.concat
            let healths =
                afStates
                |> List.map (fun af -> af.StorageHealth)
                |> List.concat
            Seq.zip blocks healths
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
    [
        for block in blocks do
            let v = Vector2.FromPos(block)
            let subst = Mcu.substId <| store.GetIdMapper()
            match tryGetRegionAt v with
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
                                subBlocks
                                |> List.mapi (fun i sub -> if float i / numSubs < health then (sub, T.Float 0.0) else (sub, T.Float 1.0))
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
                    let damagedBlock = damagedBlock :> Mcu.McuBase
                    subst damagedBlock
                    yield damagedBlock
    ]

let createBlocks random store world state (blocks : T.Block list) = createBlocksGen T.Block.Damaged random store world state blocks

let createBridges random store world state (blocks : T.Bridge list) = createBlocksGen T.Bridge.Damaged random store world state blocks

let createAirfieldSpawns (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (windDirection : Vector2) =
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
                        |> List.maxBy(fun spawn ->
                            let chart = spawn.TryGetChart()
                            match chart with
                            | None -> 0.0f
                            | Some chart ->
                                let points = chart.GetPoints()
                                let direction =
                                    points
                                    |> List.pairwise
                                    |> List.pick(fun (p1, p2) ->
                                        if p1.GetType().Value = 2 && p2.GetType().Value = 2 then
                                            let ex = Vector2.FromYOri(spawn)
                                            let ey = -ex.Rotate(90.0f)
                                            let mkVec(p : T.Airfield.Chart.Point) =
                                                (float32 <| p.GetX().Value) * ex + (float32 <| p.GetY().Value) * ey
                                            Some(mkVec(p2) - mkVec(p1))
                                        else
                                            None)
                                let len = direction.Length()
                                let direction = direction / len
                                Vector2.Dot(direction, windDirection))
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
                    |> Map.filter (fun _ number -> number >= 1.0f)
                    |> Map.map (fun plane number ->
                        let model = plane.ScriptModel
                        newAirfieldPlane("", "", 0, 0, "", "NOICON", int number)
                            .SetScript(T.String model.Script)
                            .SetModel(T.String model.Model)
                    )
                    |> Map.toSeq
                    |> Seq.map snd
                    |> List.ofSeq
                let planes =
                    T.Airfield.Planes()
                        .SetPlane(planeSpecs)
                let af = af.SetPlanes(planes).SetIndex(T.Integer 1).SetLinkTrId(T.Integer 2)
                let entity = newEntity 2
                entity.MisObjID <- 1
                let mcu = af.CreateMcu()
                subst mcu
                subst entity
                yield mcu
                yield entity :> Mcu.McuBase
    ]

/// Convoys are created according to resupply orders. A given number of convoys start at mission start, then new convoys start whenever a convoy arrives or gets destroyed.
let createConvoys store lcStore (world : World) (state : WorldState) (orders : ResupplyOrder list) =
    let sg = WorldStateFastAccess.Create state
    let convoys =
        [
            for order in orders do
                let convoy = order.Convoy
                let paths =
                    match order.Means with
                    | ByRail -> world.Rails
                    | ByRoad -> world.Roads
                let path =
                    paths
                    |> List.tryPick(fun road -> road.MatchesEndpoints(convoy.Start, convoy.Destination))
                    |> Option.map (fun path -> path.Value)
                match path with
                | Some path ->
                    let pathVertices =
                        path
                        |> List.map (fun (v, yori) ->
                            { Pos = v
                              Ori = yori
                              Radius = 10000
                              Speed = 50
                              Priority = 1
                            }
                        )
                    let country, coalition =
                        match sg.GetRegion(convoy.Start).Owner with
                        | None -> failwithf "Convoy starting from a neutral region '%A'" convoy.Start
                        | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                        | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                    let virtualConvoy =
                        let convoyName = order.MissionLogEventName
                        match order.Means with
                        | ByRoad ->
                            let size =
                                int (convoy.TransportedSupplies / ResupplyOrder.TruckCapacity)
                                |> min ColumnMovement.MaxColumnSize
                            VirtualConvoy.Create(store, lcStore, pathVertices, [], size, country, coalition, convoyName, 0)
                            |> Choice1Of2
                        | ByRail ->
                            let startV = pathVertices.Head
                            let endV = pathVertices |> List.last
                            TrainWithNotification.Create(store, lcStore, startV.Pos, startV.Ori, endV.Pos, country, convoyName)
                            |> Choice2Of2
                    let links =
                        match virtualConvoy with
                        | Choice1Of2 x -> x.CreateLinks()
                        | Choice2Of2 x -> x.CreateLinks()
                    let mcuGroup =
                        match virtualConvoy with
                        | Choice1Of2 x -> x :> McuUtil.IMcuGroup
                        | Choice2Of2 x -> x :> McuUtil.IMcuGroup
                    links.Apply(McuUtil.deepContentOf mcuGroup)
                    yield pathVertices.Head.Pos, virtualConvoy
                | None ->
                    ()
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
let createColumns random store lcStore (world : World) (state : WorldState) (missionBegin : Mcu.McuTrigger) interval maxColumnSplit (orders : ColumnMovement list) =
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
                    let travel, invasion =
                        match sg.GetRegion(order.Destination).Owner with
                        | Some owner when owner = coalition ->
                            path, []
                        | _ ->
                            path
                            |> List.partition (fun (v, _) -> v.IsInConvexPolygon (wg.GetRegion(order.Start).Boundary))
                    let toVertex(v, yori) =
                        { Pos = v
                          Ori = yori
                          Speed = 40
                          Radius = 100
                          Priority = 1
                        }
                    let travel =
                        travel
                        |> List.map toVertex
                    let invasion =
                        invasion
                        |> List.map (fun x -> { toVertex(x) with Priority = 0 })
                    let prevStart = ref missionBegin
                    let rankOffset = ref 0
                    for composition in splitCompositions random order.Composition |> List.truncate maxColumnSplit do
                        let columnContent =
                            composition
                            |> List.ofArray
                            |> List.map (fun vehicleType -> vehicleType.GetModel(coalition))
                        let columnName = order.MissionLogEventName
                        let column = VirtualConvoy.CreateColumn(store, lcStore, travel, invasion, columnContent, coalition.ToCountry, coalition.ToCoalition, columnName, !rankOffset)
                        let links = column.CreateLinks()
                        links.Apply(McuUtil.deepContentOf column)
                        Mcu.addTargetLink prevStart.Value column.Api.Start.Index
                        let beforeNext = newTimer 1
                        let subst = Mcu.substId <| store.GetIdMapper()
                        subst beforeNext
                        beforeNext.Time <- interval
                        Mcu.addTargetLink column.Api.Start beforeNext.Index
                        prevStart := upcast beforeNext
                        rankOffset := rankOffset.Value + ColumnMovement.MaxColumnSize
                        yield column, beforeNext :> Mcu.McuBase
                | None -> ()
            | None ->
                ()
    ]


let createParkedPlanes store (world : World) (state : WorldState) =
    let mkParkedPlane(model : PlaneModel, pos : OrientedPosition, country) =
        let modelScript = model.StaticScriptModel
        let block = newBlockMcu store country modelScript.Model modelScript.Script
        let p = McuUtil.newVec3(float pos.Pos.X, 0.0, float pos.Pos.Y)
        let ori = McuUtil.newVec3(0.0, float pos.Rotation, 0.0)
        McuUtil.vecCopy p block.Pos
        McuUtil.vecCopy ori block.Ori
        block

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
                        match model with
                        | Bf109e7 | Bf109f2 | Mc202 | I16 | Mig3 | P40 -> fighterPlaces
                        | Bf110e | IL2M41 -> attackerPlaces
                        | Ju88a4 | Ju52 | Pe2s35 -> bomberPlaces
                    let positions =
                        List.truncate (int qty) parking.Value
                    parking :=
                        try
                            List.skip (int qty) parking.Value
                        with
                        | _ -> []
                    yield!
                        positions
                        |> List.map(fun pos -> mkParkedPlane(model, pos, int country))
            | None ->
                ()
    ]

/// Set the country of entity owners in a list of Mcus depending on the region where they are located.
let setCountries (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (ori : float option) (group : Mcu.McuBase list) =
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
            match ori with
            | Some ori ->
                flag.Ori.Y <- ori
            | None ->
                ()
        | _ ->
            ()

let createParkedTanks store (world : World) (state : WorldState) (orders : OrderPackage) coalition =
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
                for vehicle, pos in Seq.zip parked parkingPositions do
                    let model =
                        match vehicle, coalition with
                        | HeavyTank, Axis -> Vehicles.germanStaticHeavyTank
                        | MediumTank, Axis -> Vehicles.germanStaticMediumTank
                        | LightArmor, Axis -> Vehicles.germanStaticLightArmor
                        | HeavyTank, Allies -> Vehicles.russianStaticHeavyTank
                        | MediumTank, Allies -> Vehicles.russianStaticMediumTank
                        | LightArmor, Allies -> Vehicles.russianStaticLightArmor
                    let block = newBlockMcu store (int coalition.ToCountry) model.Model model.Script
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

let writeMissionFile random weather author missionName briefing missionLength convoySpacing maxSimultaneousConvoys (strategyMissionFile : string) (world : World) (state : WorldState) (axisOrders : OrderPackage) (alliesOrders : OrderPackage) (filename : string) =
    let strategyMissionData = T.GroupData(Parsing.Stream.FromFile strategyMissionFile)
    let options = strategyMissionData.ListOfOptions.Head
    let blocks = strategyMissionData.ListOfBlock
    let bridges = strategyMissionData.ListOfBridge
    let store = NumericalIdentifiers.IdStore()
    let lcStore = NumericalIdentifiers.IdStore()
    lcStore.SetNextId 3
    let getId = store.GetIdMapper()
    let missionBegin = newMissionBegin (getId 1)
    let includeSearchLights =
        state.Date.Hour <= 8 || state.Date.Hour + missionLength / 60 >= 18
    let staticDefenses = ArtilleryGroup.Create(random, store, lcStore, includeSearchLights, missionBegin, world, state)
    let icons = MapIcons.CreateRegions(store, lcStore, world, state)
    let icons2 = MapIcons.CreateSupplyLevels(store, lcStore, world, state)
    let spotting = createStorageIcons store lcStore missionBegin world state
    let blocks = createBlocks random store world state blocks
    let bridges = createBridges random store world state bridges
    let spawns = createAirfieldSpawns store world state (Vector2.UnitX.Rotate(float32 weather.Wind.Direction))
    let mkConvoyNodes orders =
        let convoyPrioNodes, convoys = createConvoys store lcStore world state orders
        for node, convoy in List.zip convoyPrioNodes.Nodes convoys do
            let start, destroyed, arrived =
                match convoy with
                | Choice1Of2 trucks ->
                    trucks.Api.Start, trucks.Api.Destroyed, trucks.Api.Arrived
                | Choice2Of2 train ->
                    train.TheTrain.Start, train.TheTrain.Killed, train.TheTrain.Arrived
            Mcu.addTargetLink node.Do start.Index
            Mcu.addTargetLink destroyed convoyPrioNodes.Try.Index
            Mcu.addTargetLink arrived convoyPrioNodes.Try.Index
        for i, node in Seq.indexed convoyPrioNodes.Nodes do
            if i < maxSimultaneousConvoys then
                Mcu.addTargetLink missionBegin node.Do.Index
            else
                Mcu.addTargetLink missionBegin node.Enable.Index
        let convoys : McuUtil.IMcuGroup list =
            convoys
            |> List.map (
                function
                | Choice1Of2 x -> x :> McuUtil.IMcuGroup
                | Choice2Of2 x -> x :> McuUtil.IMcuGroup)
        convoyPrioNodes.All, convoys
    let mkColumns orders =
        let maxColumnSplit = max 1 (missionLength / convoySpacing - 1)
        let columns =
            orders
            |> createColumns random store lcStore world state missionBegin (60.0 * float convoySpacing) maxColumnSplit
            |> List.map (fun (x, t) -> x :> McuUtil.IMcuGroup, t)
        List.map fst columns, List.map snd columns
    let columns, columnTimers = mkColumns (axisOrders.Columns @ alliesOrders.Columns)
    let parkedPlanes =
        createParkedPlanes store world state
        |> McuUtil.groupFromList
    let parkedTanks =
        createParkedTanks store world state axisOrders Axis @ createParkedTanks store world state alliesOrders Allies
        |> McuUtil.groupFromList
    let axisPrio, axisConvoys = mkConvoyNodes axisOrders.Resupply
    let alliesPrio, alliesConvoys = mkConvoyNodes alliesOrders.Resupply
    let flags = strategyMissionData.GetGroup("Windsocks").CreateMcuList()
    setCountries store world state (Some weather.Wind.Direction) flags
    let ndbs = strategyMissionData.GetGroup("NDBs").CreateMcuList()
    setCountries store world state None ndbs
    let landFires =
        strategyMissionData.GetGroup("Land fires").CreateMcuList()
        |> createLandFires store world state missionBegin
    let options =
        (Weather.setOptions weather state.Date options)
            .SetMissionType(T.Integer 2) // deathmatch
    let optionStrings =
        { new McuUtil.IMcuGroup with
              member x.Content = []
              member x.LcStrings =
                [ (0, missionName)
                  (1, briefing)
                  (2, author)
                ]
              member x.SubGroups = []
        }
    let allGroups =
        [ optionStrings
          McuUtil.groupFromList [missionBegin]
          upcast staticDefenses
          upcast icons
          upcast icons2
          McuUtil.groupFromList blocks
          McuUtil.groupFromList bridges
          McuUtil.groupFromList spawns
          McuUtil.groupFromList columnTimers
          McuUtil.groupFromList flags
          McuUtil.groupFromList ndbs
          parkedPlanes
          parkedTanks
          axisPrio
          alliesPrio ] @ axisConvoys @ alliesConvoys @ columns @ spotting @ landFires
    writeMissionFiles "eng" filename options allGroups