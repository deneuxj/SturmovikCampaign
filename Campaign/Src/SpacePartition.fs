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

type QuadNode<'T> =
    {
        Min : Vector2
        Max : Vector2
        Children : QuadNode<'T>[]
        Content : 'T[]
    }

[<RequireQualifiedAccess>]
module QuadNode =
    let min (v1 : Vector2) (v2 : Vector2) =
        Vector2(min v1.X v2.X, min v1.Y v2.Y)

    let max (v1 : Vector2) (v2 : Vector2) =
        Vector2(max v1.X v2.X, max v1.Y v2.Y)

    let newRoot (getBounds : 'T -> (Vector2 * Vector2)) (items : 'T seq) =
        let ninf = System.Single.NegativeInfinity
        let pinf = System.Single.PositiveInfinity
        let lower, upper =
            items
            |> Seq.fold (fun (lower, upper) item ->
                let l, u = getBounds item
                (min l lower, max u upper)) (Vector2(ninf, ninf), Vector2(pinf, pinf))
        {
            Min = lower
            Max = upper
            Children = [||]
            Content = Array.ofSeq items
        }

    let rec split (intersects : 'T * Vector2 * Vector2 -> bool) (maxDepth : int) (maxItems : int) (node : QuadNode<'T>) =
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
                                |> Array.filter (fun item -> intersects(item, lower, upper))
                            let child = {
                                Min = lower
                                Max = upper
                                Children = [||]
                                Content = content
                            }
                            let child = split intersects (maxDepth - 1) maxItems child
                            yield child
                |]
            { node with Children = subs }
        else
            let newChildren =
                node.Children
                |> Array.map (split intersects (maxDepth - 1) maxItems)
            { node with Children = newChildren }

    let create (getBounds : 'T -> (Vector2 * Vector2)) (intersects : 'T * Vector2 * Vector2 -> bool) (maxDepth : int) (maxItems : int) =
        newRoot getBounds >> split intersects maxDepth maxItems

    let rec find (mayMatch : Vector2 * Vector2 -> bool) (isMatch: 'T -> bool) (node : QuadNode<'T>) =
        seq {
            if mayMatch(node.Min, node.Max) then
                match node.Children with
                | [||] ->
                    yield!
                        node.Content
                        |> Seq.filter isMatch
                | _ ->
                    yield!
                        node.Children
                        |> Seq.map (find mayMatch isMatch)
                        |> Seq.concat
        }

type QuadTree<'T> =
    {
        GetBounds : 'T -> (Vector2 * Vector2)
        Intersects : 'T * Vector2 * Vector2 -> bool
        MaxDepth : int
        MaxItems : int
        Root : QuadNode<'T>
    }
with
    member this.Split(?maxDepth, ?maxItems) =
        let maxDepth = defaultArg maxDepth this.MaxDepth
        let maxItems = defaultArg maxItems this.MaxItems
        if maxDepth = this.MaxDepth && maxItems = this.MaxItems then
            this
        else
            { this with
                MaxDepth = maxDepth
                MaxItems = maxItems
                Root = QuadNode.split this.Intersects maxDepth maxItems this.Root 
            }