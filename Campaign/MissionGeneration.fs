module Campaign.MissionGeneration

open SturmovikMission.DataProvider
open Vector
open Campaign.WorldDescription
open Campaign.WorldState
open SturmovikMission.Blocks.StaticDefenses.Factory
open SturmovikMission.Blocks.StaticDefenses.Types
open System.Numerics
open System.IO
open SturmovikMission.Blocks.BlocksMissionData
open System.Collections.Generic
open SturmovikMission.DataProvider.Cached

type Buildings = {
    All : Mcu.McuBase list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = []
        member x.SubGroups = []
         
    static member Create(store : NumericalIdentifiers.IdStore, blocks : T.Block list, world : World, state : WorldState) =
        let getOwner =
            let m =
                state.Regions
                |> Seq.map (fun region -> region.RegionId, region.Owner)
                |> dict
            fun x -> m.[x]
        let tryGetBlockState =
            let m =
                seq {
                    for region, state in Seq.zip world.Regions state.Regions do
                        for factory, health in Seq.zip region.Production state.ProductionHealth do
                            yield (factory.Pos.Pos.X, factory.Pos.Pos.Y), (factory, health, state.Owner)
                        for storage, health in Seq.zip region.Storage state.StorageHealth do
                            yield (storage.Pos.Pos.X, storage.Pos.Pos.Y), (storage, health, state.Owner)
                    for airfield, state in Seq.zip world.Airfields state.Airfields do
                        let owner = getOwner airfield.Region
                        for storage, health in Seq.zip airfield.Storage state.StorageHealth do
                            yield (storage.Pos.Pos.X, storage.Pos.Pos.Y), (storage, health, owner)
                }
                |> dict
            fun (block : T.Block) ->
                let x = float32 block.XPos.Value
                let y = float32 block.ZPos.Value
                match m.TryGetValue((x, y)) with
                | false, _ -> None
                | true, x -> Some x
        let mcus =
            blocks
            |> List.map (fun block ->
                match tryGetBlockState block with
                | Some(factory, health, owner) ->
                    let block =
                        if health < 0.9f then
                            let damaged =
                                block.Damaged.Value
                                |> Map.map(fun _ _ -> T.Float 0.0)
                                |> fun x -> T.Damaged(x)
                            block.SetDamaged(damaged)
                        else
                            block
                    let owner =
                        match owner with
                        | None -> block.Country
                        | Some(Allies) -> T.Integer(int Mcu.CountryValue.Russia)
                        | Some(Axis) -> T.Integer(int Mcu.CountryValue.Germany)
                    block.SetCountry(owner)
                | None ->
                    block
                |> fun x -> x.CreateMcu()
            )
        let subst = Mcu.substId <| store.GetIdMapper()
        for mcu in mcus do
            subst mcu
        { All = mcus
        }

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


let createBlocks (random : System.Random) (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (blocks : T.Block list) =
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
                        block.CreateMcu()
                    subst mcu
                    yield mcu
                | Some health ->
                    // Has health and strategic value, create with an entity.
                    let health = float health
                    let damagedBlock =
                        block.SetDamaged(
                            T.Damaged(
                                Seq.init 128 (fun i -> i, T.Float(if random.NextDouble() < health then 0.0 else 1.0))
                                |> Map.ofSeq
                            )
                        ).SetIndex(T.Integer 1).SetLinkTrId(T.Integer 2).CreateMcu() :?> Mcu.HasEntity
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


let writeGroupFile (blocks : T.Block list) (world : World) (state : WorldState) (filename : string) =
    let random = System.Random(0)
    let store = NumericalIdentifiers.IdStore()
    let lcStore = NumericalIdentifiers.IdStore()
    let getId = store.GetIdMapper()
    let missionBegin = newMissionBegin (getId 1)
    let antiTankDefenses = ArtilleryGroup.Create(random, store, missionBegin, world, state)
    let icons = MapIcons.Create(store, lcStore, world, state)
    let blocks = createBlocks random store world state blocks
    use file = File.CreateText(filename)
    let mcus =
        missionBegin :> Mcu.McuBase :: antiTankDefenses.All @ icons.All @ blocks
    let groupStr =
        mcus
        |> McuUtil.moveEntitiesAfterOwners
        |> Seq.map (fun mcu -> mcu.AsString())
        |> String.concat "\n"
    file.Write(groupStr)