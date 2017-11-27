module Campaign.MapGraphics

open System.Numerics
open System.Collections.Generic

open VectorExtension

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.Proximity
open SturmovikMission.DataProvider.Cached
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.IconDisplay

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Util
open Campaign.BasicTypes


/// Merge vertices of region boundaries that are close to each other.
let getRepresentative (world : World) =
    let dist2 =
        let dist = 1000.0f
        dist * dist
    let areSimilar (v1 : Vector2) (v2 : Vector2) =
        (v1 - v2).LengthSquared() < dist2
    let allVecs =
        world.Regions
        |> Seq.map (fun region -> region.Boundary)
        |> List.concat
    let equivClasses = Algo.computePartition areSimilar allVecs
    Algo.getEquivalent equivClasses

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
    [ icon1; icon2 ]

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
            let getIcon v =
                mkIcon 13 (255, 255, 255) v
            let getIcon = getRepresentative world >> getIcon
            let mkSegment = mkSegmentIcons getIcon
            let icons =
                [
                    for segment in segments do
                        match segment.Kind with
                        | OuterBorder _ -> ()
                        | InnerBorder(home, other) ->
                            let homeState = getState home
                            let otherState = getState other
                            match homeState, otherState with
                            | { Owner = Some Allies }, { Owner = Some Axis } ->
                                yield! mkSegment segment.Edge
                            | _ ->
                                ()
                ]
            icons
        let otherLineIcons =
            let mkSegment viewers color =
                let cache = Dictionary<_,_>()
                let mkIcon v =
                    let icon = mkIcon 1 color v
                    if not(List.isEmpty viewers) then
                        icon.Coalitions <- viewers
                    icon
                let getIcon = getRepresentative world >> (cached cache mkIcon)
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
                    mkAxisSegment(x, y) |> ignore
                    mkAxisSegmentByAllies(x, y) |> ignore
                | Allies -> fun (x, y) ->
                    mkAlliesSegment(x, y)  |> ignore
                    mkAlliesSegmentByAxis(x, y) |> ignore
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
        let renderSupplyBar (pos : Vector2) health color =
            let mkIcon = mkIcon store lcStore
            // Gray thin background bar representing max storage
            let l0 = mkIcon (int Mcu.LineTypeValue.SectorType1) (64, 64, 64) pos
            let l1 = mkIcon (int Mcu.LineTypeValue.SectorType1) (64, 64, 64) (pos + length * Vector2.UnitY)
            // Thick colored foreground bar representing available storage; color shows supply level relative to ammo needs
            let h0 = mkIcon (int Mcu.LineTypeValue.Bold) color (pos)
            let h1 = mkIcon (int Mcu.LineTypeValue.Bold) color (pos + length * health * Vector2.UnitY)
            l0.Targets <- [l1.Index]
            h0.Targets <- [h1.Index]
            [ l0; l1; h0; h1 ]
        let supplies = AutoOrder.computeStorage world state
        let fullCapacities = AutoOrder.computeStorageCapacity world
        let actualCapacities = AutoOrder.computeActualStorageCapacity world state
        let icons = [
            for region, regState in List.zip world.Regions state.Regions do
                let needs =
                    world.AntiAirDefenses @ world.AntiTankDefenses
                    |> List.filter (fun area -> area.Home = region.RegionId)
                    |> List.sumBy (fun area -> area.AmmoCost)
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
                    let k =
                        supplies / needs
                        |> max 0.0f
                        |> min 1.0f
                    let red = 200.0f * (1.0f - k)
                    let green = 255.0f * k
                    (int red, int green, 0)
                let health =
                    if fullCapacity = 0.0f<E> then
                        0.0f
                    else
                        capacity / fullCapacity
                        |> max 0.0f
                        |> min 1.0f
                yield! renderSupplyBar region.Position health color
        ]
        let capitals = [
            for region, regState in List.zip world.Regions state.Regions do
                let capital = mkIcon store lcStore 0 (106, 0, 0) (region.Position + Vector2(0.1f * length, 0.5f * length))
                let (RegionId name) = region.RegionId
                let name =
                    if region.Production.IsEmpty then
                        name
                    else
                        name.ToUpper()
                let label =
                    // Show number of vehicles beside the region's name
                    let numVehicles =
                        regState.NumVehicles
                        |> Map.toSeq
                        |> Seq.sumBy snd
                    if numVehicles > 0 then
                        sprintf "%s (%d)" name numVehicles
                    else
                        name
                yield capital, label
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

    static member CreateArrows(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, world : World, state : WorldState, axisOrders : Orders.OrderPackage, alliesOrers : Orders.OrderPackage, coalition : CoalitionId) =
        let sg = state.FastAccess
        let wg = world.FastAccess
        let renderArrow(start : Vector2, tip : Vector2, width : float32, headAngle : float32, color) =
            let dir =
                let x = (tip - start)
                x / x.Length()
            let points =
                let mkIcon =
                    mkIcon store lcStore (int Mcu.LineTypeValue.Attack) color
                    >> fun icon -> icon.Coalitions <- [coalition.ToCoalition]; icon
                [
                    yield mkIcon (start + width * dir.Rotate(90.0f))
                    yield mkIcon start
                    yield mkIcon tip
                    yield mkIcon (tip + width * dir.Rotate(90.0f + headAngle))
                ]
            for p1, p2 in Seq.pairwise points do
                p1.Targets <- [p2.Index]
            points
        let computeArrowWidth x =
            2000.0f * x + 500.0f * (1.0f - x)
        let mkRoadTravelArrow(startRegion : RegionId, endRegion : RegionId, color, numVehicles : int) =
            match world.Roads |> List.tryPick (fun path -> path.MatchesEndpoints(startRegion, endRegion)) with
            | Some x ->
                let start, _, _ = x.Value.Head
                let tip, _, _ = List.last x.Value
                let width =
                    let x = (float32 numVehicles) / 15.0f |> min 1.0f
                    computeArrowWidth x
                renderArrow(start, tip, width, 45.0f, color)
            | None ->
                []
        let mkRailTravelArrow(startRegion : RegionId, endRegion : RegionId, color, qty) =
            match world.Rails |> List.tryPick (fun path -> path.MatchesEndpoints(startRegion, endRegion)) with
            | Some x ->
                let start, _, _ = x.Value.Head
                let tip, _, _ = List.last x.Value
                let width =
                    let x = (qty / Orders.ResupplyOrder.TrainCapacity) |> min 1.0f
                    computeArrowWidth x
                renderArrow(start, tip, width, 45.0f, color)
            | None ->
                []
        let mkWaterTravelArrow(startRegion : RegionId, endRegion : RegionId, color, qty) =
            match world.SeaWays @ world.RiverWays |> List.tryPick (fun path -> path.MatchesEndpoints(startRegion, endRegion)) with
            | Some x ->
                let start, _, _ = x.Value.Head
                let tip, _, _ = List.last x.Value
                let width =
                    let x = (qty / Orders.ResupplyOrder.ShipCapacity) |> min 1.0f
                    computeArrowWidth x
                renderArrow(start, tip, width, 45.0f, color)
            | None ->
                []
        let mkAirTravelArrow(startAirfield : AirfieldId, endAirfield : AirfieldId, color, qty) =
            let af1, af2 = wg.GetAirfield(startAirfield), wg.GetAirfield(endAirfield)
            let start = af1.Pos
            let tip = af2.Pos
            let width =
                let x = (qty / (PlaneModel.Ju52.CargoCapacity * bombCost)) |> min 1.0f
                computeArrowWidth x
            renderArrow(start, tip, width, 45.0f, color)
        let friendlyOrders, enemyOrders =
            match coalition with
            | Axis -> axisOrders, alliesOrers
            | Allies -> alliesOrers, axisOrders
        let arrowIcons =
            [
                let handleResupplyOrder color (order : Orders.ResupplyOrder) =
                    seq {
                        match order.Means with
                        | Orders.ByRoad ->
                            let iconId = Mcu.IconIdValue.CoverTransportColumn
                            yield! mkRoadTravelArrow(order.Convoy.Start, order.Convoy.Destination, color, int(order.Convoy.TransportedSupplies / Orders.ResupplyOrder.TruckCapacity))
                        | Orders.ByRail ->
                            let iconId = Mcu.IconIdValue.CoverTrains
                            yield! mkRailTravelArrow(order.Convoy.Start, order.Convoy.Destination, color, order.Convoy.TransportedSupplies)
                        | Orders.ByRiverShip
                        | Orders.BySeaShip ->
                            let iconId = Mcu.IconIdValue.CoverShips
                            yield! mkWaterTravelArrow(order.Convoy.Start, order.Convoy.Destination, color, order.Convoy.TransportedSupplies)
                        | Orders.ByAir(start, destination) ->
                            let iconId = Mcu.IconIdValue.CoverBombersFlight
                            yield! mkAirTravelArrow(start, destination, color, order.Convoy.TransportedSupplies)
                    }
                let handleColumnOrder color (order : Orders.ColumnMovement) =
                    seq {
                        let iconId = Mcu.IconIdValue.CoverArmorColumn
                        match order.TransportType with
                        | Orders.ColByRoad ->
                            yield! mkRoadTravelArrow(order.Start, order.Destination, color, order.Composition.Length)
                        | Orders.ColByTrain ->
                            yield! mkRailTravelArrow(order.Start, order.Destination, color, (float32 order.Composition.Length / 15.0f) * Orders.ResupplyOrder.TrainCapacity)
                        | Orders.ColByRiverShip
                        | Orders.ColBySeaShip ->
                            yield! mkWaterTravelArrow(order.Start, order.Destination, color, (float32 order.Composition.Length / 15.0f) * Orders.ResupplyOrder.ShipCapacity)
                    }
                for order in friendlyOrders.Resupply do
                    yield! handleResupplyOrder (0, 0, 10) order
                for order in enemyOrders.Resupply do
                    yield! handleResupplyOrder (10, 0, 0) order
                for order in friendlyOrders.Columns do
                    yield! handleColumnOrder (0, 0, 10) order
                for order in enemyOrders.Columns do
                    yield! handleColumnOrder (10, 0, 0) order
            ]
        let lcStrings =
            [
                for icon in arrowIcons do
                    match icon.IconLC with
                    | Some data ->
                        yield (data.LCDesc, "")
                        yield (data.LCName, "")
                    | None ->
                        ()
            ]
        { All = arrowIcons |> List.map (fun x -> upcast x)
          LcStrings = lcStrings }

/// Create icons for filled storage areas that appear when the storage area has been spotted by a plane.
let createStorageIcons store lcStore missionBegin (world : World) (state : WorldState) =
    [
        for region, regState in List.zip world.Regions state.Regions do
            match regState.Owner with
            | Some owner ->
                let classes =
                    region.Storage
                    |> Algo.computePartition (fun sto1 sto2 -> (sto1.Pos.Pos - sto2.Pos.Pos).Length() < 1000.0f)
                for group in classes do
                    match group with
                    | sto :: _ ->
                        let icon = IconDisplay.Create(store, lcStore, sto.Pos.Pos, "", owner.Other.ToCoalition, Mcu.IconIdValue.AttackBuildings)
                        icon.Show.Time <- 300.0 // Delay icon by 5 minutes
                        let wec = Proximity.Create(store, owner.Other.ToCoalition, 2500, sto.Pos.Pos)
                        Mcu.addTargetLink wec.Out icon.Show.Index
                        Mcu.addTargetLink missionBegin wec.Start.Index
                        yield icon.All
                        yield wec.All
                    | [] ->
                        ()
            | None ->
                ()
    ]
