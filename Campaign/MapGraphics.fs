module Campaign.MapGraphics

open System.Numerics
open System.Collections.Generic
open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider.Cached
open Vector
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Util

/// Merge vertices of region boundaries that are close to each other.
let getRepresentant (world : World) =
    let dist2 =
        let dist = 1000.0f
        dist * dist
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

/// Default icon from which all icons are cloned.
let private defaultIcon =
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

/// Make an icon.
let mkIcon (store : NumericalIdentifiers.IdStore) (lcStore : NumericalIdentifiers.IdStore) (lineType : int) (red, green, blue) (v : Vector2) =
    let subst = Mcu.substId <| store.GetIdMapper()
    let substLc = Mcu.substLCId <| lcStore.GetIdMapper()
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

/// Make two icons connected by a line.
let mkSegmentIcons mkIcon (segment : Vector2 * Vector2) =
    let icon1 : Mcu.McuIcon = mkIcon(fst segment)
    let icon2 = mkIcon(snd segment)
    icon1.Targets <- icon2.Index :: icon1.Targets

/// Type of segments rendering a part of a region boundary.
type SegmentType =
    | OuterBorder of RegionId
    | InnerBorder of RegionId * RegionId

/// Segment data.
type Segment = {
    Kind : SegmentType
    Edge : Vector2 * Vector2
}

/// Groups icons and their labels.
type MapIcons = {
    All : Mcu.McuBase list
    LcStrings : (int * string) list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = x.LcStrings
        member x.SubGroups = []

    /// Render region boundaries
    static member CreateRegions(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, world : World, state : WorldState) =
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
                let dist2 =
                    let dist = 1000.0f
                    dist * dist
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
                                Kind = OuterBorder region.RegionId
                                Edge = (u1, u2)
                            }
                        else
                            yield Seq.head sharedEdges
            ]
        let mkIcon = mkIcon store lcStore
        let frontLineIcons =
            let cache = Dictionary<_,_>()
            let getIcon v =
                mkIcon 13 (255, 255, 255) v
            let getIcon = getRepresentant world >> (cached cache getIcon)
            let mkSegment = mkSegmentIcons getIcon
            for segment in segments do
                match segment.Kind with
                | OuterBorder _ -> ()
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
        let otherLineIcons =
            let mkSegment viewers color =
                let cache = Dictionary<_,_>()
                let mkIcon v =
                    let icon = mkIcon 1 color v
                    if not(List.isEmpty viewers) then
                        icon.Coalitions <- viewers
                    icon
                let getIcon = getRepresentant world >> (cached cache mkIcon)
                cache, mkSegmentIcons getIcon
            let mkEnemySegment coalition = mkSegment [coalition] (10, 0, 0)
            let mkFriendlySegment coalition = mkSegment [coalition] (0, 0, 10)
            let mkColoredSegment color = mkSegment [] color
            let cache1, mkAxisSegmentByAllies = mkEnemySegment Mcu.CoalitionValue.Allies
            let cache2, mkAlliesSegmentByAxis = mkEnemySegment Mcu.CoalitionValue.Axis
            let cache3, mkAxisSegment = mkFriendlySegment Mcu.CoalitionValue.Axis
            let cache4, mkAlliesSegment = mkFriendlySegment Mcu.CoalitionValue.Allies
            let mkCoalitionSegment coalition =
                match coalition with
                | Axis -> fun (x, y) ->
                    mkAxisSegment(x, y)
                    mkAxisSegmentByAllies(x, y)
                | Allies -> fun (x, y) ->
                    mkAlliesSegment(x, y)
                    mkAlliesSegmentByAxis(x, y)
            let cache5, mkDarkSegment = mkColoredSegment (30, 0, 0)
            for segment in segments do
                match segment.Kind with
                | OuterBorder region ->
                    let owner = (getState region).Owner
                    match owner with
                    // Outer border of non-neutral coalition rendered using the coalition's color.
                    | Some coalition -> mkCoalitionSegment coalition segment.Edge
                    // Outer border of neutral coalition not rendered at all.
                    | None -> ()
                | InnerBorder(home, other) ->
                    let homeState = getState home
                    let otherState = getState other
                    match homeState, otherState with
                    | { Owner = Some coalition }, { Owner = None } ->
                        // Inner border of non-neutral with neutral rendered using coalition's color
                        mkCoalitionSegment coalition segment.Edge
                    | { Owner = Some coalition1 }, { Owner = Some coalition2 } when coalition1 = coalition2 ->
                        // Internal border rendered as thin dark segment
                        //mkDarkSegment segment.Edge
                        ()
                    | _ ->
                        ()
            // Rewrite line type of icons for dark segments as thin dotted line
            for icon in cache5.Values do
                icon.LineType <- Mcu.LineTypeValue.Normal
            // Extract and return all created icons from the caches
            [
                for cache in [ cache1; cache2; cache3; cache4; cache5 ] do
                    yield! List.ofSeq cache.Values
            ]
        let allIcons = List.concat [frontLineIcons; otherLineIcons]
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

    /// Create health bars for each region showing supply and damage levels.
    static member CreateSupplyLevels(store, lcStore, world, state) =
        let length = 2000.0f
        let renderSupplyBar (pos : Vector2) health color supply =
            let mkIcon = mkIcon store lcStore
            let l0 = mkIcon (int Mcu.LineTypeValue.SectorType1) (64, 64, 64) pos
            let l1 = mkIcon (int Mcu.LineTypeValue.SectorType1) (64, 64, 64) (pos + length * Vector2.UnitY)
            let h0 = mkIcon (int Mcu.LineTypeValue.Bold) (0, 0, 128) (pos)
            let h1 = mkIcon (int Mcu.LineTypeValue.Bold) (0, 0, 128) (pos + length * health * Vector2.UnitY)
            let s0 = mkIcon (int Mcu.LineTypeValue.Normal) color (pos + length * supply * Vector2.UnitY + Vector2(0.05f * length, 0.0f))
            let s1 = mkIcon (int Mcu.LineTypeValue.Normal) color (pos + length * supply * Vector2.UnitY - Vector2(0.05f * length, 0.0f))
            l0.Targets <- [l1.Index]
            h0.Targets <- [h1.Index]
            s0.Targets <- [s1.Index]
            [ l0; l1; h0; h1; s0; s1 ]
        let needs = AutoOrder.computeSupplyNeeds world state
        let supplies = AutoOrder.computeStorage world state
        let fullCapacities = AutoOrder.computeStorageCapacity world
        let actualCapacities = AutoOrder.computeActualStorageCapacity world state
        let icons = [
            for region, regState in List.zip world.Regions state.Regions do
                let needs =
                    Map.tryFind region.RegionId needs
                    |> Option.defaultVal 0.0f<E>
                let supplies =
                    Map.tryFind region.RegionId supplies
                    |> Option.defaultVal 0.0f<E>
                let capacity =
                    Map.tryFind region.RegionId actualCapacities
                    |> Option.defaultVal 0.0f<E>
                let fullCapacity =
                    Map.tryFind region.RegionId fullCapacities
                    |> Option.defaultVal 0.0f<E>
                let color =
                    if needs < 0.0f<E> then
                        (0, 255, 0)
                    elif needs < 0.25f * supplies then
                        (200, 175, 32)
                    else
                        (200, 0, 0)
                let level =
                    if fullCapacity = 0.0f<E> then
                        0.0f
                    else
                        supplies / fullCapacity
                        |> max 0.0f
                        |> min 1.0f
                let health =
                    if fullCapacity = 0.0f<E> then
                        0.0f
                    else
                        capacity / fullCapacity
                        |> max 0.0f
                        |> min 1.0f
                yield! renderSupplyBar region.Position health color level
        ]
        let capitals = [
            for region, regState in List.zip world.Regions state.Regions do
                let capital = mkIcon store lcStore 0 (106, 0, 0) (region.Position + Vector2(0.1f * length, 0.5f * length))
                let (RegionId name) = region.RegionId
                yield capital, name
        ]
        let lcStrings =
            [
                for icon in icons do
                    match icon.IconLC with
                    | Some data ->
                        yield (data.LCDesc, "")
                        yield (data.LCName, "")
                    | None ->
                        ()
                for capital, label in capitals do
                    match capital.IconLC with
                    | Some data ->
                        yield (data.LCDesc, "")
                        yield (data.LCName, label)
                    | None ->
                        ()
            ]
        let allIcons =
            [ icons; capitals |> List.map fst ]
            |> List.concat
        { All = allIcons |> List.map (fun x -> upcast x)
          LcStrings = lcStrings
        }
