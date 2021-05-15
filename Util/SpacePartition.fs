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
        ContentLen : int // Only used in internal nodes when content is not stored there
    }

[<RequireQualifiedAccess>]
module QuadNode =
    open FSharp.Collections.ParallelSeq

    let rec allItems (node : QuadNode<'T>) =
        seq {
            if node.Children.Length = 0 then
                yield! node.Content
            else
                for child in node.Children do
                    yield! allItems child
        }

    let rec allLeaves (node : QuadNode<'T>) =
        seq {
            if node.Children.Length = 0 then
                yield node
            else
                for child in node.Children do
                    yield! allLeaves child
        }

    /// Get the lower corner of the bounding box containing two points
    let vmin (v1 : Vector2) (v2 : Vector2) =
        Vector2(min v1.X v2.X, min v1.Y v2.Y)

    /// Get the upper corner of the bounding box containing two points
    let vmax (v1 : Vector2) (v2 : Vector2) =
        Vector2(max v1.X v2.X, max v1.Y v2.Y)

    /// Create the root of a quad tree
    let newRoot (getBounds : 'T -> (Vector2 * Vector2)) (items : 'T seq) =
        let ninf = System.Single.NegativeInfinity
        let pinf = System.Single.PositiveInfinity
        let lower, upper =
            items
            |> Seq.fold (fun (lower, upper) item ->
                let l, u = getBounds item
                (vmin l lower, vmax u upper)) (Vector2(pinf, pinf), Vector2(ninf, ninf))
        let content = Array.ofSeq items
        {
            Min = lower
            Max = upper
            Children = [||]
            Content = content
            ContentLen = content.Length
        }

    let divideBounds (minV : Vector2, maxV : Vector2) =
        let hx = (minV.X + maxV.X) / 2.0f
        let hy = (minV.Y + maxV.Y) / 2.0f
        seq {
            for x1, x2 in [(minV.X, hx); (hx, maxV.X)] do
                for y1, y2 in [(minV.Y, hy); (hy, maxV.Y)] do
                    let lower = Vector2(x1, y1)
                    let upper = Vector2(x2, y2)
                    yield (lower, upper)
        }

    /// Split or merge the leaves of a quad tree to meet new constraints on maximum depth and minimum number of items in internal nodes
    let rec split (intersects : 'T -> Vector2 * Vector2 -> bool) (maxDepth : int) (minItems : int) (contentInInnerNodes : bool) (node : QuadNode<'T>) =
        if maxDepth <= 0 || node.ContentLen <= minItems then
            if node.Children.Length = 0 then
                node
            else
                let content =
                    if contentInInnerNodes then
                        node.Content
                    else
                        allItems node
                        |> Seq.distinct
                        |> Array.ofSeq
                { node with
                    Children = [||]
                    Content = content
                }
        elif node.Children.Length = 0 then
            let subs =
                [|
                    for lower, upper in divideBounds(node.Min, node.Max) do
                        let content =
                            node.Content
                            |> PSeq.filter (fun item -> intersects item (lower, upper))
                            |> PSeq.toArray
                        let child = {
                            Min = lower
                            Max = upper
                            Children = [||]
                            Content = content
                            ContentLen = content.Length
                        }
                        let child = split intersects (maxDepth - 1) minItems contentInInnerNodes child
                        yield child
                |]
            { node with
                Children = subs
                Content = if contentInInnerNodes then node.Content else [||]
                ContentLen = if contentInInnerNodes then node.Content.Length else subs |> Array.sumBy (fun sub -> sub.ContentLen)
            }
        else
            let newChildren =
                node.Children
                |> Array.map (split intersects (maxDepth - 1) minItems contentInInnerNodes)
            if newChildren.Length = node.Children.Length && Seq.forall2 (fun n1 n2 -> obj.ReferenceEquals(n1, n2)) newChildren node.Children then
                node
            else
                { node with
                    Children = newChildren
                    Content = if contentInInnerNodes then node.Content else [||]
                    ContentLen = if contentInInnerNodes then node.Content.Length else newChildren |> Array.sumBy (fun sub -> sub.ContentLen)
                }

    /// Create the root of a quad tree and split it
    let create (getBounds : 'T -> (Vector2 * Vector2)) (intersects : 'T -> Vector2 * Vector2 -> bool) (maxDepth : int) (minItems : int) (contentInInnerNodes : bool) =
        newRoot getBounds >> split intersects maxDepth minItems contentInInnerNodes

    /// Insert an item in the node and its children, if it intersects with the bounds
    let insert (intersects : 'T -> Vector2 * Vector2 -> bool) (maxDepth : int) (minItems : int) (contentInInnerNodes : bool) (item : 'T) (node : QuadNode<'T>) =
        let rec pushDown (item : 'T) (node : QuadNode<'T>) =
            if not(intersects item (node.Min, node.Max)) then
                node
            else
            let node =
                { node with
                    ContentLen = node.ContentLen + 1
                    Content = if contentInInnerNodes || node.Children.Length = 0 then Array.append node.Content [| item |] else node.Content
                }
            if node.Children.Length = 0 && maxDepth > 0 && node.ContentLen > minItems then
                split intersects maxDepth minItems contentInInnerNodes node
            else
                let children =
                    node.Children
                    |> Array.map (pushDown item)
                { node with
                    Children = children
                }
        pushDown item node

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
type QuadTree<'T when 'T : equality> =
    {
        Intersects : 'T -> Vector2 * Vector2 -> bool
        MaxDepth : int
        MinItems : int
        ContentInInnerNodes : bool
        Root : QuadNode<'T>
    }
with
    /// Split or merge leaves to meet new constraints
    member this.Split(?maxDepth, ?minItems) =
        let maxDepth = defaultArg maxDepth this.MaxDepth
        let minItems = defaultArg minItems this.MinItems
        if maxDepth = this.MaxDepth && minItems = this.MinItems then
            this
        else
            { this with
                MaxDepth = maxDepth
                MinItems = minItems
                Root = QuadNode.split this.Intersects maxDepth minItems this.ContentInInnerNodes this.Root
            }

/// Helper functions to check for intersections based on convex hulls
module Functions =
    let getSeparatingAxes (poly1 : Vector2 list) (poly2 : Vector2 list) =
        match poly1 with
        | v0 :: _ ->
            seq {
                for (v1, v2) in poly1 @ [v0] |> Seq.pairwise do
                    let allOnOuterSide =
                        seq {
                            if v1 <> v2 then
                                let dv = v2 - v1
                                for w in poly2 do
                                    let s = Vector2.Cross(dv, w - v1)
                                    yield s < 0.0f
                        }
                        |> Seq.forall id
                    if allOnOuterSide && v1 <> v2 then
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
    let fromBoundaryOjects (getBoundary : 'T -> Vector2 list) maxDepth minItems contentInInnerNodes (items : 'T seq) =
        let getBoundingBox = Functions.getBoundingBox getBoundary
        let intersectWithBox = Functions.intersectWithBoundingBox getBoundary
        let root = QuadNode.create getBoundingBox intersectWithBox maxDepth minItems contentInInnerNodes items
        {
            Intersects = intersectWithBox
            MaxDepth = maxDepth
            MinItems = minItems
            ContentInInnerNodes = contentInInnerNodes
            Root = root
        }

/// An object to find items in a quad tree that intersect a provided type of external objects
type QuadTreeItemFinder<'T, 'U when 'T : equality> =
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
    /// Find candidates for the reference point of a shape (0, 0) that must fit within free areas and within the boundaries of a convex region
    let findPositionCandidates (random : System.Random) (finder : QuadTreeItemFinder<_, _>) (shape : Vector2 list) (region : Vector2 list) =
        let candidates =
            let xs = region |> List.map (fun v -> v.X)
            let ys = region |> List.map (fun v -> v.Y)
            let b0 = Vector2(List.min xs, List.min ys)
            let b1 = Vector2(List.max xs, List.max ys)
            Seq.initInfinite (fun _ ->
                let r0 = random.NextDouble() |> float32
                let r1 = random.NextDouble() |> float32
                Vector2(r0 * b0.X + (1.0f - r0) * b1.X, r1 * b0.Y + (1.0f - r1) * b1.Y))
            |> Seq.cache
            |> Seq.filter (fun v -> v.IsInConvexPolygon region)

        let validate (candidate : Vector2) =
            let shape = shape |> List.map ((+) candidate)
            let obstacles = finder.FindIntersectingItems shape
            Seq.isEmpty obstacles

        candidates
        |> Seq.filter validate
