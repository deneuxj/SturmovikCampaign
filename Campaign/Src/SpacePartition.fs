// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2019 Johann Deneux <johann.deneux@gmail.com>
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

namespace Campaign.SpacePartition

open System.Numerics
open VectorExtension

/// A node in a quad tree
type QuadNode<'T> =
    {
        Min : Vector2
        Max : Vector2
        Children : QuadNode<'T>[]
        Content : 'T[]
    }

[<RequireQualifiedAccess>]
module QuadNode =
    open FSharp.Collections.ParallelSeq

    /// Get the lower corner of the bounding box containing two points
    let min (v1 : Vector2) (v2 : Vector2) =
        Vector2(min v1.X v2.X, min v1.Y v2.Y)

    /// Get the upper corner of the bounding box containing two points
    let max (v1 : Vector2) (v2 : Vector2) =
        Vector2(max v1.X v2.X, max v1.Y v2.Y)

    /// Create the root of a quad tree
    let newRoot (getBounds : 'T -> (Vector2 * Vector2)) (items : 'T seq) =
        let ninf = System.Single.NegativeInfinity
        let pinf = System.Single.PositiveInfinity
        let lower, upper =
            items
            |> Seq.fold (fun (lower, upper) item ->
                let l, u = getBounds item
                (min l lower, max u upper)) (Vector2(pinf, pinf), Vector2(ninf, ninf))
        {
            Min = lower
            Max = upper
            Children = [||]
            Content = Array.ofSeq items
        }

    /// Split or merge the leaves of a quad tree to meet new constraints on maximum depth and maximum number of items in leaves
    let rec split (intersects : 'T -> Vector2 * Vector2 -> bool) (maxDepth : int) (maxItems : int) (contentInInnerNodes : bool) (node : QuadNode<'T>) =
        if maxDepth <= 0 || node.Content.Length <= maxItems then
            { node with Children = [||] }
        elif node.Children.Length = 0 then
            let subs =
                let hx = (node.Min.X + node.Max.X) / 2.0f
                let hy = (node.Min.Y + node.Max.Y) / 2.0f
                [|
                    for x1, x2 in [(node.Min.X, hx); (hx, node.Max.X)] do
                        for y1, y2 in [(node.Min.Y, hy); (hy, node.Max.Y)] do
                            let lower = Vector2(x1, y1)
                            let upper = Vector2(x2, y2)
                            let content =
                                node.Content
                                |> PSeq.filter (fun item -> intersects item (lower, upper))
                                |> PSeq.toArray
                            let child = {
                                Min = lower
                                Max = upper
                                Children = [||]
                                Content = content
                            }
                            let child = split intersects (maxDepth - 1) maxItems contentInInnerNodes child
                            yield child
                |]
            { node with
                Children = subs
                Content = if contentInInnerNodes then node.Content else [||]
            }
        else
            let newChildren =
                node.Children
                |> Array.map (split intersects (maxDepth - 1) maxItems contentInInnerNodes)
            { node with
                Children = newChildren
                Content = if contentInInnerNodes then node.Content else [||]
            }

    /// Create the root of a quad tree and split it
    let create (getBounds : 'T -> (Vector2 * Vector2)) (intersects : 'T -> Vector2 * Vector2 -> bool) (maxDepth : int) (maxItems : int) (contentInInnerNodes : bool) =
        newRoot getBounds >> split intersects maxDepth maxItems contentInInnerNodes

    /// Find items in a quad tree that intersect an external item
    /// Note that the sequence of found items may contain duplicates
    let rec find (mayIntersect : Vector2 * Vector2 -> bool) (intersect: 'T -> bool) (node : QuadNode<'T>) =
        seq {
            if mayIntersect(node.Min, node.Max) then
                match node.Children with
                | [||] ->
                    yield!
                        node.Content
                        |> Seq.filter intersect
                | _ ->
                    yield!
                        node.Children
                        |> Seq.map (find mayIntersect intersect)
                        |> Seq.concat
        }

/// A quad tree, composed of a root, constraints on the content and depth of the tree, and a function to grow or curtail the tree.
type QuadTree<'T> =
    {
        Intersects : 'T -> Vector2 * Vector2 -> bool
        MaxDepth : int
        MaxItems : int
        ContentInInnerNodes : bool
        Root : QuadNode<'T>
    }
with
    /// Split or merge leaves to meet new constraints
    member this.Split(?maxDepth, ?maxItems) =
        let maxDepth = defaultArg maxDepth this.MaxDepth
        let maxItems = defaultArg maxItems this.MaxItems
        if maxDepth = this.MaxDepth && maxItems = this.MaxItems then
            this
        else
            { this with
                MaxDepth = maxDepth
                MaxItems = maxItems
                Root = QuadNode.split this.Intersects maxDepth maxItems this.ContentInInnerNodes this.Root 
            }

/// Helper functions to check for intersections based on convex hulls
module private Functions =
    let getSeparatingAxes (poly1 : Vector2 list) (poly2 : Vector2 list) =
        match poly1 with
        | v0 :: _ ->
            seq {
                for (v1, v2) in poly1 @ [v0] |> Seq.pairwise do
                    let allOnOuterSide =
                        seq {
                            for w in poly2 do
                                let s = Vector2.Cross(v2 - v1, w - v1)
                                if s >= 0.0f then
                                    yield ()
                        }
                        |> Seq.isEmpty
                    if allOnOuterSide then
                        yield (v1, v2)
            }
        | [] ->
            Seq.empty

    let tryGetSeparatingAxis poly1 poly2 =
        [
            getSeparatingAxes poly1 poly2
            getSeparatingAxes poly2 poly1
        ]
        |> Seq.concat
        |> Seq.tryHead

    let getBoundingBox (getBoundary : 'T -> Vector2 list) item =
        let boundary = getBoundary item
        let minx =
            boundary
            |> Seq.map (fun v -> v.X)
            |> Seq.min
        let maxx =
            boundary
            |> Seq.map (fun v -> v.X)
            |> Seq.max
        let miny =
            boundary
            |> Seq.map (fun v -> v.Y)
            |> Seq.min
        let maxy =
            boundary
            |> Seq.map (fun v -> v.Y)
            |> Seq.max
        Vector2(minx, miny), Vector2(maxx, maxy)

    let intersectWithBoundingBox (getBoundary : 'T -> Vector2 list) item (lower : Vector2, upper : Vector2) =
        let poly1 =
            getBoundary item
        let poly2 =
            [
                lower
                Vector2(upper.X, lower.Y)
                upper
                Vector2(lower.X, upper.Y)
            ]
        tryGetSeparatingAxis poly1 poly2
        |> Option.isNone

[<RequireQualifiedAccess>]
module QuadTree =
    /// Create a quad tree from items that have convex polygons as boundaries
    let fromBoundaryOjects (getBoundary : 'T -> Vector2 list) maxDepth maxItems contentInInnerNodes (items : 'T seq) =
        let getBoundingBox = Functions.getBoundingBox getBoundary
        let intersectWithBox = Functions.intersectWithBoundingBox getBoundary
        let root = QuadNode.create getBoundingBox intersectWithBox maxDepth maxItems contentInInnerNodes items
        {
            Intersects = intersectWithBox
            MaxDepth = maxDepth
            MaxItems = maxItems
            ContentInInnerNodes = contentInInnerNodes
            Root = root
        }

/// An object to find items in a quad tree that intersect a provided type of external objects
type QuadTreeItemFinder<'T, 'U> =
    {
        Tree : QuadTree<'T>
        MayMatch : 'U -> Vector2 * Vector2 -> bool
        IsMatch : 'U -> 'T -> bool
    }
with
    /// Find items in the quad tree that intersect a given external item
    member this.FindIntersectingItems (uitem : 'U) =
        QuadNode.find (this.MayMatch uitem) (this.IsMatch uitem) this.Tree.Root

module QuadTreeItemFinder =
    /// Create a QuadTreeItemFinder that extracts convex hulls from internal and external items, and use that to check for intersection.
    let create getTreeItemBoundary getExternalItemBoundary tree =
        let mayMatch = Functions.intersectWithBoundingBox getExternalItemBoundary
        let isMatch uitem =
            let boundary2 =
                getExternalItemBoundary uitem
            fun item ->
                let boundary1 =  
                    getTreeItemBoundary item
                Functions.tryGetSeparatingAxis boundary1 boundary2
                |> Option.isNone
        {
            Tree = tree
            MayMatch = mayMatch
            IsMatch = isMatch
        }

/// Extract free areas from a QuadTree
module FreeAreas =
    type FreeAreasNode =
        {
            Min : Vector2
            Max : Vector2
            Children : FreeAreasNode[]
        }

    /// Translate a quad tree node to a free areas node
    let rec translate (quad : QuadNode<Vector2>) =
        if Array.isEmpty quad.Children then
            if Array.isEmpty quad.Content then
                Some {
                    Min = quad.Min
                    Max = quad.Max
                    Children = [||]
                }
            else
                None
        else
            let subs = quad.Children |> Array.choose translate
            match subs with
            | [||] -> None
            | _ ->
                Some {
                    Min = quad.Min
                    Max = quad.Max
                    Children = subs
                }

    /// Compute area statistics, returning total area and number of leaf nodes
    let rec sumArea (free : FreeAreasNode) =
        if Array.isEmpty free.Children then
            let area = (free.Max.X - free.Min.X) * (free.Max.Y - free.Min.Y)
            (area, 1)
        else
            free.Children
            |> Seq.map sumArea
            |> Seq.fold (fun (area, num) (acc0, acc1) -> (area + acc0, num + acc1)) (0.0f, 0)