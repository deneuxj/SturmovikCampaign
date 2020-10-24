// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
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

module Campaign.MissionGen.MapGraphics

open System.Numerics
open System.Collections.Generic

open SturmovikMission.DataProvider
open SturmovikMission.Cached
open SturmovikMission.Blocks.MapGraphics

open Util
open Campaign.Common.BasicTypes
open Campaign.Common.Algo


/// Merge vertices of region boundaries that are close to each other.
let getRepresentative (regions : IRegion list) =
    let dist2 =
        let dist = 1000.0f
        dist * dist
    let areSimilar (v1 : Vector2) (v2 : Vector2) =
        (v1 - v2).LengthSquared() < dist2
    let allVecs =
        regions
        |> Seq.map (fun region -> region.Boundary)
        |> List.concat
    let equivClasses = computePartition areSimilar allVecs
    getEquivalent equivClasses


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
    static member CreateSegments(regions : IRegion list, getRegion : RegionId -> IRegion) =
        let segments =
            [
                let dist2 =
                    let dist = 1000.0f
                    dist * dist
                for region in regions do
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
        segments

    /// Turn a list of segments to lits of loops, i.e. list of connected segments.
    static member MakeLoops(getOwner : RegionId -> CoalitionId option, segments : Segment list) =
        let rec pullString (bit : Segment) (working : Segment list) loop =
            match working with
            | [] -> bit :: loop, []
            | _ :: _ ->
                let owner = getOwner(bit.Region)
                let next =
                    working
                    |> List.tryFind (fun next ->
                        let nextOwner = getOwner(next.Region)
                        let isBorder =
                            match next.Kind with
                            | OuterBorder _ -> true
                            | InnerBorder(_, other) -> getOwner(other) <> owner
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
                        getOwner(r1) <> getOwner(r2)
                if proceed then
                    let loop, working = pullString start working []
                    let loop = List.rev loop
                    work working (loop :: loops)
                else
                    work working loops
        work segments []

type IRegionQuery =
    abstract Regions : IRegion list
    abstract GetRegion : RegionId -> IRegion
    abstract GetOwner : RegionId -> CoalitionId option
    abstract GetRegionAntiAirCapacity : RegionId * CoalitionId -> float32

type SturmovikMission.Blocks.MapGraphics.MapIcons with
    /// Render region boundaries
    static member CreateRegions(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, regions : IRegionQuery) =
        let segments =
            [
                let dist2 =
                    let dist = 1000.0f
                    dist * dist
                for region in regions.Regions do
                    let owner = regions.GetOwner(region.RegionId)
                    let getCycled vertices =
                        match vertices with
                        | [] -> []
                        | x :: _ -> vertices @ [x]
                    let boundary = getCycled region.Boundary
                    for (u1, u2) in Seq.pairwise boundary do
                        let sharedEdges =
                            seq {
                                for next in region.Neighbours do
                                    let nextRegion = regions.GetRegion next
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
            let getIcon = getRepresentative regions.Regions >> getIcon
            let mkSegment = mkSegmentIcons getIcon
            let icons =
                [
                    for segment in segments do
                        match segment.Kind with
                        | OuterBorder _ -> ()
                        | InnerBorder(home, other) ->
                            let homeOwner = regions.GetOwner(home)
                            let otherOwner = regions.GetOwner(other)
                            match homeOwner, otherOwner with
                            | Some Allies, Some Axis ->
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
                let getIcon = getRepresentative regions.Regions >> (cached cache mkIcon)
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
                    let owner = regions.GetOwner(region)
                    match owner with
                    // Outer border of non-neutral coalition rendered using the coalition's color.
                    | Some coalition -> mkCoalitionSegment coalition segment.Edge
                    // Outer border of neutral coalition not rendered at all.
                    | None -> ()
                | InnerBorder(home, other) ->
                    let homeOwner = regions.GetOwner(home)
                    let otherOwner = regions.GetOwner(other)
                    match homeOwner, otherOwner with
                    | Some coalition, None ->
                        // Inner border of non-neutral with neutral rendered using coalition's color
                        mkCoalitionSegment coalition segment.Edge
                    | Some coalition1, Some coalition2 when coalition1 = coalition2 ->
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
