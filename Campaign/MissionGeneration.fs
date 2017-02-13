﻿module Campaign.MissionGeneration

open SturmovikMission.DataProvider
open Vector
open Campaign.WorldDescription
open Campaign.WorldState
open SturmovikMission.Blocks.AntiTank.Factory
open System.Numerics
open System.IO

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

    static member Create(random : System.Random, store : NumericalIdentifiers.IdStore, world : World, state : WorldState) =
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
                    let owner = getOwner area.Home.Home
                    let country, coalition =
                        match owner with
                        | None -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Neutral
                        | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                        | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                    let group = AntiTankGroup.Create(random, store, area.Boundary, area.Position.Rotation, state.NumUnits, country, coalition)
                    let links = group.CreateLinks()
                    let mcus = McuUtil.deepContentOf group
                    links.Apply(mcus)
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
                    match state.Owner with
                    | None -> ()
                    | Some owner ->
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
                                        let radius2 = 1000.0f * 1000.0f
                                        let haveShared =
                                            boundary2
                                            |> Seq.pairwise
                                            |> Seq.exists (fun (v1, v2) ->
                                                (u2 - v1).LengthSquared() < radius2 &&
                                                (u1 - v2).LengthSquared() < radius2
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
                T.VectorOfIntegers[],
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
        let mkIcon(v : Vector2) =
            defaultIcon
                .SetXPos(T.Float(float v.X))
                .SetZPos(T.Float(float v.Y))
                .CreateMcu()
                :?> Mcu.McuIcon
        let outerIcons =
            [
                for segment in segments do
                    match segment.Kind with
                    | OuterBorder ->
                        let icon1 = mkIcon(fst segment.Edge)
                        let icon2 = mkIcon(snd segment.Edge)
                        icon2.Index <- 2
                        icon1.Targets <- icon2.Index :: icon1.Targets
                        let subst = Mcu.substId <| store.GetIdMapper()
                        let substLc = Mcu.substLCId <| store.GetIdMapper()
                        subst icon1
                        subst icon2
                        substLc icon1
                        substLc icon2
                        yield icon1
                        yield icon2
                    | InnerBorder _ ->
                        ()
            ]
        let lcStrings =
            [
                for icon in outerIcons do
                    match icon.IconLC with
                    | Some data ->
                        yield (data.LCDesc, "")
                        yield (data.LCName, "")
                    | None ->
                        ()
            ]
        { All = outerIcons |> List.map (fun x -> upcast x)
          LcStrings = lcStrings
        }


let writeGroupFile (world : World) (state : WorldState) (filename : string) =
    let random = System.Random(0)
    let store = NumericalIdentifiers.IdStore()
    let lcStore = NumericalIdentifiers.IdStore()
    let antiTankDefenses = ArtilleryGroup.Create(random, store, world, state)
    let icons = MapIcons.Create(store, lcStore, world, state)
    use file = File.CreateText(filename)
    let mcus =
        antiTankDefenses.All @ icons.All
    let groupStr =
        mcus
        |> McuUtil.moveEntitiesAfterOwners
        |> Seq.map (fun mcu -> mcu.AsString())
        |> String.concat "\n"
    file.Write(groupStr)