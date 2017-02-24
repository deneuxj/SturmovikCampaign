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
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks
open SturmovikMission.Blocks.IO
open Vector

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Weather
open Campaign.Orders


type ArtilleryGroup = {
    All : Mcu.McuBase list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = []
        member x.SubGroups = []

    static member Create(random : System.Random, store : NumericalIdentifiers.IdStore, missionBegin : Mcu.McuTrigger, world : World, state : WorldState) =
        let getAreaState =
            let m =
                state.DefenseAreas
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
                        let group = StaticDefenseGroup.Create(AntiTank, random, store, area.Boundary, area.Position.Rotation, state.NumUnits, country, coalition)
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
                        let group = StaticDefenseGroup.Create(AntiAir, random, store, area.Boundary, area.Position.Rotation, state.NumUnits, country, coalition)
                        let links = group.CreateLinks()
                        let mcus = McuUtil.deepContentOf group
                        links.Apply(mcus)
                        Mcu.addTargetLink missionBegin group.Api.Start.Index
                        yield! mcus
            ]
        { All = all
        }

type SegmentType =
    | OuterBorder
    | InnerBorder of RegionId * RegionId

type Segment = {
    Kind : SegmentType
    Edge : Vector2 * Vector2
}

type MapIcons = {
    All : Mcu.McuBase list
    LcStrings : (int * string) list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = x.LcStrings
        member x.SubGroups = []

    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, world : World, state : WorldState) =
        let dist2 =
            let dist = 1000.0f
            dist * dist
        let getRepresentant =
            let equivClasses =
                let allVecs =
                    world.Regions
                    |> Seq.map (fun region -> region.Boundary)
                    |> List.concat
                let singletons =
                    allVecs
                    |> List.map (fun v -> [v])
                allVecs
                |> Seq.fold (fun equivClasses v ->
                    let near, far =
                        equivClasses
                        |> List.partition (fun points ->
                            points
                            |> List.exists (fun v2 -> (v - v2).LengthSquared() < dist2)
                        )
                    List.concat near :: far
                ) singletons
            let m =
                equivClasses
                |> Seq.map (fun cl ->
                    match cl with
                    | lead :: rest ->
                        cl
                        |> List.map (fun v -> v, lead)
                    | [] ->
                        []
                )
                |> Seq.concat
                |> dict
            fun v -> m.[v]
        let getState =
            let m =
                state.Regions
                |> Seq.map (fun region -> region.RegionId, region)
                |> dict
            fun x -> m.[x]
        let getRegion =
            let m =
                world.Regions
                |> Seq.map (fun region -> region.RegionId, region)
                |> dict
            fun x -> m.[x]
        let segments =
            [
                for region in world.Regions do
                    let state = getState region.RegionId
                    let getCycled vertices =
                        match vertices with
                        | [] -> []
                        | x :: _ -> vertices @ [x]
                    let boundary = getCycled region.Boundary
                    for (u1, u2) in Seq.pairwise boundary do
                        let sharedEdges =
                            seq {
                                for next in region.Neighbours do
                                    let nextRegion = getRegion next
                                    let boundary2 = getCycled nextRegion.Boundary
                                    let haveShared =
                                        boundary2
                                        |> Seq.pairwise
                                        |> Seq.exists (fun (v1, v2) ->
                                            (u2 - v1).LengthSquared() < dist2 &&
                                            (u1 - v2).LengthSquared() < dist2
                                        )
                                    if haveShared then
                                        yield {
                                            Kind = InnerBorder(region.RegionId, next)
                                            Edge = (u1, u2)
                                        }
                            }
                        if Seq.isEmpty sharedEdges then
                            yield {
                                Kind = OuterBorder
                                Edge = (u1, u2)
                            }
                        else
                            yield Seq.head sharedEdges
            ]
        let defaultIcon =
            let lcDesc = 1
            let lcName = 2
            T.MCU_Icon(
                T.Integer(0),
                T.VectorOfIntegers[1;2],
                T.Boolean true,
                T.Integer(0),
                T.Integer(0),
                T.Integer(1),
                T.Integer(lcDesc),
                T.Integer(lcName),
                T.Integer(0),
                T.VectorOfIntegers[],
                T.Integer(0),
                T.VectorOfIntegers[],
                T.Float(0.0),
                T.Float(0.0),
                T.Float(0.0),
                T.Float(0.0),
                T.Float(0.0),
                T.Float(0.0)
            )
        let mkIcon(lineType : int) (red, green, blue) (v : Vector2) =
            let subst = Mcu.substId <| store.GetIdMapper()
            let substLc = Mcu.substLCId <| store.GetIdMapper()
            let mcu =
                defaultIcon
                    .SetXPos(T.Float(float v.X))
                    .SetZPos(T.Float(float v.Y))
                    .SetLineType(T.Integer lineType)
                    .SetRColor(T.Integer red)
                    .SetGColor(T.Integer green)
                    .SetBColor(T.Integer blue)
                    .CreateMcu()
                    :?> Mcu.McuIcon
            subst mcu
            substLc mcu
            mcu
        let mkSegmentIcons mkIcon (segment : Vector2 * Vector2) =
            let icon1 : Mcu.McuIcon = mkIcon(fst segment)
            let icon2 = mkIcon(snd segment)
            icon1.Targets <- icon2.Index :: icon1.Targets
        let outerIcons =
            let cache = Dictionary<_,_>()
            let getIcon v =
                mkIcon 1 (0, 0, 0) v
            let getIcon = getRepresentant >> (cached cache getIcon)
            let mkSegment = mkSegmentIcons getIcon
            for segment in segments do
                match segment.Kind with
                | OuterBorder ->
                    mkSegment segment.Edge
                | InnerBorder _ ->
                    ()
            cache.Values
            |> List.ofSeq
        let frontLineIcons =
            let cache = Dictionary<_,_>()
            let getIcon v =
                mkIcon 13 (255, 255, 255) v
            let getIcon = getRepresentant >> (cached cache getIcon)
            let mkSegment = mkSegmentIcons getIcon
            for segment in segments do
                match segment.Kind with
                | OuterBorder -> ()
                | InnerBorder(home, other) ->
                    let homeState = getState home
                    let otherState = getState other
                    match homeState, otherState with
                    | { Owner = Some Allies }, { Owner = Some Axis } ->
                        mkSegment segment.Edge
                    | _ ->
                        ()
            cache.Values
            |> List.ofSeq
        let germanBorderLineIcons =
            let cache = Dictionary<_,_>()
            let getIcon v =
                mkIcon 1 (86, 105, 135) v
            let getIcon = getRepresentant >> (cached cache getIcon)
            let mkSegment = mkSegmentIcons getIcon
            for segment in segments do
                match segment.Kind with
                | OuterBorder -> ()
                | InnerBorder(home, other) ->
                    let homeState = getState home
                    let otherState = getState other
                    match homeState, otherState with
                    | { Owner = Some Axis }, { Owner = None } ->
                        mkSegment segment.Edge
                    | _ ->
                        ()
            cache.Values
            |> List.ofSeq
        let russianBorderLineIcons =
            let cache = Dictionary<_,_>()
            let getIcon v =
                mkIcon 1 (240, 0, 0) v
            let getIcon = getRepresentant >> (cached cache getIcon)
            let mkSegment = mkSegmentIcons getIcon
            for segment in segments do
                match segment.Kind with
                | OuterBorder -> ()
                | InnerBorder(home, other) ->
                    let homeState = getState home
                    let otherState = getState other
                    match homeState, otherState with
                    | { Owner = Some Allies }, { Owner = None } ->
                        mkSegment segment.Edge
                    | _ ->
                        ()
            cache.Values
            |> List.ofSeq
        let allIcons = List.concat [outerIcons; frontLineIcons; germanBorderLineIcons; russianBorderLineIcons]
        let lcStrings =
            [
                for icon in allIcons do
                    match icon.IconLC with
                    | Some data ->
                        yield (data.LCDesc, "")
                        yield (data.LCName, "")
                    | None ->
                        ()
            ]
        { All = allIcons |> List.map (fun x -> upcast x)
          LcStrings = lcStrings
        }

open SturmovikMission.Blocks.BlocksMissionData.CommonMethods

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
                    // No strategic value, create without entity.
                    let mcu =
                        createMcu block
                    subst mcu
                    yield mcu
                | Some health ->
                    // Has health and strategic value, create with an entity.
                    let health = float health
                    let damagedBlock =
                        block
                        |> setDamaged (
                            mkDamaged (
                                Seq.init 128 (fun i -> i, T.Float(if random.NextDouble() < health then 0.0 else 1.0))
                                |> Map.ofSeq))
                        |> setDurability (T.Integer(Functions.getDurabilityForBuilding(block |> getModel |> valueOf)))
                        |> setIndex (T.Integer 1)
                        |> setLinkTrId (T.Integer 2)
                        |> createMcu
                        :?> Mcu.HasEntity
                    match state.Owner with
                    | Some Allies ->
                        damagedBlock.Country <- Mcu.CountryValue.Russia
                    | Some Axis ->
                        damagedBlock.Country <- Mcu.CountryValue.Germany
                    | _ ->
                        ()
                    let entity = newEntity(2)
                    entity.MisObjID <- 1
                    let damagedBlock = damagedBlock :> Mcu.McuBase
                    let entity = entity :> Mcu.McuBase
                    subst damagedBlock
                    subst entity
                    yield damagedBlock
                    yield entity
    ]

let createBlocks random store world state (blocks : T.Block list) = createBlocksGen T.Block.Damaged random store world state blocks

let createBridges random store world state (blocks : T.Bridge list) = createBlocksGen T.Bridge.Damaged random store world state blocks

let createAirfieldSpawns (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) =
    let getOwner =
        let m =
            state.Regions
            |> Seq.map(fun region -> region.RegionId, region.Owner)
            |> dict
        fun x -> m.[x]
    let getPlaneModel x =
        match x with
        | Bf109e7 -> Vehicles.germanFighter1
        | Bf109f2 -> Vehicles.germanFighter2
        | Mc202 -> Vehicles.germanFighter3
        | Bf110e -> Vehicles.germanAttacker1
        | Ju88a4 -> Vehicles.germanBomber1
        | Ju52 -> Vehicles.germanBomber2
        | I16 -> Vehicles.russianFighter1
        | Mig3 -> Vehicles.russianFighter2
        | P40 -> Vehicles.russianFighter3
        | IL2M41 -> Vehicles.russianAttacker1
        | Pe2s35 -> Vehicles.russianBomber1
    [
        for airfield, state in Seq.zip world.Airfields state.Airfields do
            let subst = Mcu.substId <| store.GetIdMapper()
            match getOwner airfield.Region with
            | None -> ()
            | Some coalition ->
                let af =
                    match coalition with
                    | Axis ->
                        airfield.Spawn.SetCountry(T.Integer(int(Mcu.CountryValue.Germany)))
                    | Allies ->
                        airfield.Spawn.SetCountry(T.Integer(int(Mcu.CountryValue.Russia)))
                let planeSpecs : T.Airfield.Planes.Plane list =
                    state.NumPlanes
                    |> Map.map (fun plane number ->
                        let model = getPlaneModel plane
                        newAirfieldPlane("", "", 0, 0, "", "", number)
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


let createConvoys (missionLengthMinutes : int) (startInterval : int) (store : NumericalIdentifiers.IdStore) lcStore  (world : World) (state : WorldState) (orders : ResupplyOrder list) =
    let getOwner =
        let m =
            state.Regions
            |> Seq.map(fun region -> region.RegionId, region.Owner)
            |> dict
        fun x -> m.[x]
    [
        for order in orders do
            let convoy = order.Convoy
            let paths =
                match order.Means with
                | ByRail -> world.Rails
                | ByRoad -> world.Roads
            let path =
                paths
                |> List.tryFind(fun road ->
                    road.StartId = convoy.Start && road.EndId = convoy.Destination || road.StartId = convoy.Destination && road.EndId = convoy.Start)
            match path with
            | Some path ->
                let vertices, kdir =
                    if path.StartId = convoy.Start then
                        path.Locations, float
                    else
                        path.Locations |> List.rev, fun yori -> float (if yori < 180.0f then yori + 180.0f else yori - 180.0f)
                let pathVertices =
                    vertices
                    |> List.map (fun (v, yori) ->
                        { Pos = McuUtil.newVec3(float v.X, 0.0, float v.Y)
                          Ori = McuUtil.newVec3(0.0, kdir yori, 0.0)
                          Radius = 10000
                          Speed = 50
                          Priority = 1
                        }
                    )
                let country, coalition =
                    match getOwner convoy.Start with
                    | None -> failwithf "Convoy starting from a neutral region '%A'" convoy.Start
                    | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                    | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                let virtualConvoys =
                    [
                        for i in 1 .. startInterval .. missionLengthMinutes do
                            let virtualConvoy =
                                match order.Means with
                                | ByRoad ->
                                    VirtualConvoy.Create(store, lcStore, pathVertices, convoy.Size, country, coalition)
                                | ByRail ->
                                    VirtualConvoy.CreateTrain(store, lcStore, pathVertices, country, coalition)
                            let links = virtualConvoy.CreateLinks()
                            links.Apply(McuUtil.deepContentOf virtualConvoy)
                            yield virtualConvoy
                    ]
                let timers =
                    [
                        for conv1, conv2 in Seq.pairwise virtualConvoys do
                            let getId = store.GetIdMapper()
                            let timer = newTimer (getId 1)
                            timer.Time <- float startInterval * 60.0
                            Mcu.addTargetLink conv1.Api.Start (timer.Index)
                            Mcu.addTargetLink timer conv2.Api.Start.Index
                            yield timer
                    ]
                yield virtualConvoys, timers
            | None ->
                ()
    ]


let createParkedPlanes store (world : World) (state : WorldState) =
    let mkParkedPlane(model : PlaneModel, pos : OrientedPosition, country) =
        let modelScript = model.StaticScriptModel
        let block, entity = newBlockWithEntityMcu store country modelScript.Model modelScript.Script
        let p = McuUtil.newVec3(float pos.Pos.X, 0.0, float pos.Pos.Y)
        let ori = McuUtil.newVec3(0.0, float pos.Rotation, 0.0)
        McuUtil.vecCopy p block.Pos
        McuUtil.vecCopy p entity.Pos
        McuUtil.vecCopy ori block.Ori
        McuUtil.vecCopy ori entity.Ori
        [block; upcast entity]

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
                        List.truncate qty parking.Value
                    parking :=
                        try
                            List.skip qty parking.Value
                        with
                        | _ -> []
                    yield!
                        positions
                        |> List.map(fun pos -> mkParkedPlane(model, pos, int country))
            | None ->
                ()
    ]
    |> List.concat


let writeMissionFile (random : System.Random) author missionName briefing (options : T.Options) (blocks : T.Block list) (bridges : T.Bridge list) (world : World) (state : WorldState) (orders : ResupplyOrder list) (filename : string) =
    let daysOffset = System.TimeSpan(int64(world.WeatherDaysOffset * 3600.0 * 24.0  * 1.0e7))
    let weather = Weather.getWeather random (state.Date + daysOffset)
    let store = NumericalIdentifiers.IdStore()
    let lcStore = NumericalIdentifiers.IdStore()
    lcStore.SetNextId 3
    let getId = store.GetIdMapper()
    let missionBegin = newMissionBegin (getId 1)
    let antiTankDefenses = ArtilleryGroup.Create(random, store, missionBegin, world, state)
    let icons = MapIcons.Create(store, lcStore, world, state)
    let blocks = createBlocks random store world state blocks
    let bridges = createBridges random store world state bridges
    let spawns = createAirfieldSpawns store world state
    let convoysAndTimers = createConvoys 240 60 store lcStore world state orders
    for convoys, _ in convoysAndTimers do
        match convoys with
        | first :: _ ->
            Mcu.addTargetLink missionBegin first.Api.Start.Index
        | [] ->
            ()
    let convoys : McuUtil.IMcuGroup list =
        convoysAndTimers
        |> Seq.map fst
        |> Seq.concat
        |> Seq.map (fun x -> x :> McuUtil.IMcuGroup)
        |> List.ofSeq
    let interConvoyTimers =
        convoysAndTimers
        |> Seq.map snd
        |> Seq.concat
        |> Seq.map (fun x -> McuUtil.groupFromList [x])
        |> List.ofSeq
    let parkedPlanes =
        createParkedPlanes store world state
        |> McuUtil.groupFromList
    let missionBegin = McuUtil.groupFromList [missionBegin]
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
          missionBegin
          upcast antiTankDefenses
          upcast icons
          McuUtil.groupFromList blocks
          McuUtil.groupFromList bridges
          McuUtil.groupFromList spawns
          parkedPlanes ] @ convoys @ interConvoyTimers
    writeMissionFiles "eng" filename options allGroups