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
open SturmovikMission.Blocks.MapGraphics

open Campaign.WorldDescription
open Campaign.WorldState
open Util
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


/// Type of segments rendering a part of a region boundary.
type SegmentType =
    | OuterBorder of RegionId
    | InnerBorder of RegionId * RegionId
with
    member this.Region =
        match this with
        | OuterBorder r
        | InnerBorder(r, _)  -> r

/// Segment data.
type Segment = {
    Kind : SegmentType
    Edge : Vector2 * Vector2
}
with
    member this.Region = this.Kind.Region

    /// Build segments at the border of regions
    static member CreateSegments(world : World, state : WorldState) =
        let wg = world.FastAccess
        let sg = state.FastAccess
        let segments =
            [
                let dist2 =
                    let dist = 1000.0f
                    dist * dist
                for region in world.Regions do
                    let state = sg.GetRegion(region.RegionId)
                    let getCycled vertices =
                        match vertices with
                        | [] -> []
                        | x :: _ -> vertices @ [x]
                    let boundary = getCycled region.Boundary
                    for (u1, u2) in Seq.pairwise boundary do
                        let sharedEdges =
                            seq {
                                for next in region.Neighbours do
                                    let nextRegion = wg.GetRegion(next)
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
        segments

    /// Turn a list of segments to lits of loops, i.e. list of connected segments.
    static member MakeLoops(state : WorldState, segments : Segment list) =
        let sg = state.FastAccess
        let rec pullString (bit : Segment) (working : Segment list) loop =
            match working with
            | [] -> bit :: loop, []
            | _ :: _ ->
                let owner = sg.GetRegion(bit.Region).Owner
                let next =
                    working
                    |> List.tryFind (fun next ->
                        let nextOwner = sg.GetRegion(next.Region).Owner
                        let isBorder =
                            match next.Kind with
                            | OuterBorder _ -> true
                            | InnerBorder(_, other) -> sg.GetRegion(other).Owner <> owner
                        let isClose = ((fst next.Edge) - (snd bit.Edge)).Length() < 1000.0f
                        isClose && nextOwner = owner && isBorder)
                match next with
                | Some next ->
                    let working =
                        working
                        |> List.filter ((<>) next)
                    pullString next working (bit :: loop)
                | None ->
                    bit :: loop, working
        let rec work (working : Segment list) (loops : Segment list list) =
            match working with
            | [] -> loops
            | start :: working ->
                let proceed =
                    match start.Kind with
                    | OuterBorder _ ->
                        true
                    | InnerBorder(r1, r2) ->
                        sg.GetRegion(r1).Owner <> sg.GetRegion(r2).Owner
                if proceed then
                    let loop, working = pullString start working []
                    let loop = List.rev loop
                    work working (loop :: loops)
                else
                    work working loops
        work segments []

type SturmovikMission.Blocks.MapGraphics.MapIcons with
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
          Show = None
          Hide = None
          LcStrings = lcStrings
        }

    /// Create health bars for each region showing supply and damage levels.
    static member CreateSupplyLevels(store, lcStore, missionLength, world, state) =
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
            let fills = state.GetAmmoFillLevelPerRegion(world, missionLength)
            for region in world.Regions do
                let fill = fills.TryFind(region.RegionId) |> Option.defaultValue 0.0f
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
                    let red = 200.0f * (1.0f - fill)
                    let green = 255.0f * fill
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
                let pos = region.Position // region.Parking |> Seq.sum |> ((*) (1.0f / float32 region.Parking.Length))
                let capital = mkIcon store lcStore 0 (106, 0, 0) (pos + Vector2(0.1f * length, 0.5f * length))
                let (RegionId name) = region.RegionId
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
          Show = None
          Hide = None
        }

    static member CreateArrows(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, world : World, state : WorldState, axisOrders : Orders.OrderPackage, alliesOrders : Orders.OrderPackage, coalition : CoalitionId) =
        let sg = state.FastAccess
        let wg = world.FastAccess
        let computeArrowWidth x =
            2000.0f * x + 500.0f * (1.0f - x)
        let renderArrow = renderArrow (store, lcStore, [coalition.ToCoalition])
        let mkRoadTravelArrow(startRegion : RegionId, endRegion : RegionId, color, numVehicles : int) =
            match world.Roads |> List.tryPick (fun path -> path.MatchesEndpoints(startRegion, endRegion)) with
            | Some x ->
                let start = x.Value.Head.Pos
                let tip = (List.last x.Value).Pos
                let width =
                    let x = (float32 numVehicles) / 15.0f |> min 1.0f
                    computeArrowWidth x
                renderArrow(start, tip, width, 45.0f, color)
            | None ->
                []
            |> MapIcons.FromIcons
        let mkRailTravelArrow(startRegion : RegionId, endRegion : RegionId, color, qty) =
            match world.Rails |> List.tryPick (fun path -> path.MatchesEndpoints(startRegion, endRegion)) with
            | Some x ->
                let start = x.Value.Head.Pos
                let tip = (List.last x.Value).Pos
                let width =
                    let x = (qty / Orders.ResupplyOrder.TrainCapacity) |> min 1.0f
                    computeArrowWidth x
                renderArrow(start, tip, width, 45.0f, color)
            | None ->
                []
            |> MapIcons.FromIcons
        let mkWaterTravelArrow(startRegion : RegionId, endRegion : RegionId, color, qty) =
            match world.SeaWays @ world.RiverWays |> List.tryPick (fun path -> path.MatchesEndpoints(startRegion, endRegion)) with
            | Some x ->
                let start = x.Value.Head.Pos
                let tip = (List.last x.Value).Pos
                let width =
                    let x = (qty / Orders.ResupplyOrder.ShipCapacity) |> min 1.0f
                    computeArrowWidth x
                renderArrow(start, tip, width, 45.0f, color)
            | None ->
                []
            |> MapIcons.FromIcons
        let mkAirTravelArrow(startAirfield : AirfieldId, endAirfield : AirfieldId, color, qty) =
            let af1, af2 = wg.GetAirfield(startAirfield), wg.GetAirfield(endAirfield)
            let start = af1.Pos
            let tip = af2.Pos
            let width =
                let x = (qty / (PlaneModel.Ju52.CargoCapacity * bombCost)) |> min 1.0f
                computeArrowWidth x
            renderArrow(start, tip, width, 45.0f, color)
            |> MapIcons.FromIcons
        let friendlyOrders, enemyOrders =
            match coalition with
            | Axis -> axisOrders, alliesOrders
            | Allies -> alliesOrders, axisOrders
        let arrowIcons =
            [
                let handleResupplyOrder color (order : Orders.ResupplyOrder) =
                    seq {
                        match order.Means with
                        | Orders.ByRoad ->
                            let iconId = Mcu.IconIdValue.CoverTransportColumn
                            yield order.OrderId, mkRoadTravelArrow(order.Convoy.Start, order.Convoy.Destination, color, int(order.Convoy.TransportedSupplies / Orders.ResupplyOrder.TruckCapacity)).AddShow(store)
                        | Orders.ByRail ->
                            let iconId = Mcu.IconIdValue.CoverTrains
                            yield order.OrderId, mkRailTravelArrow(order.Convoy.Start, order.Convoy.Destination, color, order.Convoy.TransportedSupplies).AddShow(store)
                        | Orders.ByRiverShip
                        | Orders.BySeaShip ->
                            let iconId = Mcu.IconIdValue.CoverShips
                            yield order.OrderId, mkWaterTravelArrow(order.Convoy.Start, order.Convoy.Destination, color, order.Convoy.TransportedSupplies).AddShow(store)
                        | Orders.ByAir(start, destination) ->
                            let iconId = Mcu.IconIdValue.CoverBombersFlight
                            yield order.OrderId, mkAirTravelArrow(start, destination, color, order.Convoy.TransportedSupplies).AddShow(store)
                    }
                let handleColumnOrder color (order : Orders.ColumnMovement) =
                    seq {
                        let iconId = Mcu.IconIdValue.CoverArmorColumn
                        match order.TransportType with
                        | Orders.ColByRoad ->
                            yield order.OrderId, mkRoadTravelArrow(order.Start, order.Destination, color, order.Composition.Length).AddShow(store)
                        | Orders.ColByTrain ->
                            yield order.OrderId, mkRailTravelArrow(order.Start, order.Destination, color, (float32 order.Composition.Length / 15.0f) * Orders.ResupplyOrder.TrainCapacity).AddShow(store)
                        | Orders.ColByRiverShip
                        | Orders.ColBySeaShip ->
                            yield order.OrderId, mkWaterTravelArrow(order.Start, order.Destination, color, (float32 order.Composition.Length / 15.0f) * Orders.ResupplyOrder.ShipCapacity).AddShow(store)
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
        arrowIcons

let clusteredBuildings getBuildings (world : World) (state : WorldState) =
    [
        for region, regState in List.zip world.Regions state.Regions do
            match regState.Owner with
            | Some owner ->
                let classes =
                    getBuildings(region, regState)
                    |> Algo.computePartition (fun (sto1, _) (sto2, _) -> (sto1.Pos.Pos - sto2.Pos.Pos).Length() < 3000.0f)
                    |> List.map (fun x -> x, owner)
                yield! classes
            | None ->
                ()
    ]

/// Create icons for storage areas and factories that appear after being spotted by a plane.
let createStorageIcons maxItems store lcStore missionBegin (world : World) (state : WorldState) =
    // Clusters of production and storage buildings
    let clusters =
        [
            yield!
                clusteredBuildings (fun (region : Region, regState : RegionState) -> List.zip region.Storage regState.StorageHealth) world state
                |> List.map (fun x -> "storage", x)
            yield!
                clusteredBuildings (fun (region : Region, regState : RegionState) -> List.zip region.Production regState.ProductionHealth) world state
                |> List.map (fun x -> "production", x)
        ]
    // Retain the maxItems buildings with highest storage or production capacity in each kind (storage, production) and coalition
    let clusters =
        clusters
        |> Seq.groupBy (fun (kind, (_, coalition)) -> (kind, coalition))
        |> Seq.collect (fun ((kind, _), clusters) ->
            clusters
            |> Seq.sortByDescending (fun (_, (buildings, _)) ->
                let buildings, healths =
                    List.map fst buildings,
                    List.map snd buildings
                match kind with
                | "storage" ->
                    buildingsStorageCapacity world.SubBlockSpecs buildings healths
                    |> float32
                | "production" ->
                    buildingsProductionCapacity world.SubBlockSpecs 1.0f buildings healths
                    |> float32
                | _ -> failwith "Unknown building kind")
            |> Seq.truncate maxItems)
        |> Seq.map (fun (label, (buildings, owner)) -> label, owner, buildings |> List.map fst)
    // Build icons
    [
        for label, owner, group in clusters do
            match group with
            | sto :: _ ->
                let iconA, iconB = IconDisplay.CreatePair(store, lcStore, sto.Pos.Pos, label, owner.ToCoalition, Mcu.IconIdValue.CoverBuildings)
                iconA.Show.Time <- 300.0 // Delay icon by 5 minutes
                iconB.Show.Time <- 300.0
                let wec = Proximity.Create(store, owner.Other.ToCoalition, 2500, sto.Pos.Pos)
                Mcu.addTargetLink wec.Out iconA.Show.Index
                Mcu.addTargetLink wec.Out iconB.Show.Index
                Mcu.addTargetLink missionBegin wec.Start.Index
                yield iconA.All
                yield iconB.All
                yield wec.All
            | [] ->
                ()
    ]


/// Create icons for tank parks.
let createTankParkIcons store lcStore missionBegin (world : World) (state : WorldState) =
    // Build icons
    [
        for reg, regState in List.zip world.Regions state.Regions do
            if not regState.NumExposedVehicles.IsEmpty && not reg.Parking.IsEmpty && not regState.HasInvaders then
                match regState.Owner with
                | Some owner ->
                    let pos = reg.Parking.[0]
                    let label = "tank park"
                    let iconA, iconB = IconDisplay.CreatePair(store, lcStore, pos, label, owner.ToCoalition, Mcu.IconIdValue.CoverArmorColumn)
                    iconA.Show.Time <- 300.0 // Delay icon by 5 minutes
                    iconB.Show.Time <- 300.0
                    let wec = Proximity.Create(store, owner.Other.ToCoalition, 2500, pos)
                    Mcu.addTargetLink wec.Out iconA.Show.Index
                    Mcu.addTargetLink wec.Out iconB.Show.Index
                    Mcu.addTargetLink missionBegin wec.Start.Index
                    yield iconA.All
                    yield iconB.All
                    yield wec.All
                | None ->
                    ()
    ]
