﻿module QuadNodeTests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open System.Numerics
open Campaign.SpacePartition
open VectorExtension

open ClusterTests

let boxAround(_, x, y) =
    let v = Vector2(x, y)
    let ll = v - Vector2(1.0f, 1.0f)
    let ur = v + Vector2(1.0f, 1.0f)
    [
        ll
        Vector2(ur.X, ll.Y)
        ur
        Vector2(ll.X, ur.Y)
    ]

[<Property>]
let ``Content of quadtree is identical to input content``() =
    Prop.forAll arbIntItems (fun items ->
        let qt =
            items
            |> QuadTree.fromBoundaryOjects boxAround 10 10 true
        let allItems =
            QuadNode.allItems qt.Root
            |> Set.ofSeq
        let original =
            Set items
        allItems = original
    )

[<Property>]
let ``Splitting a quad tree maintains the content``() =
    let genPars = Gen.zip (Gen.choose(1, 5)) (Gen.choose(10, 20))
    let genAll = Gen.zip genIntItems genPars
    let arbAll = Arb.fromGen genAll
    Prop.forAll arbAll (fun (items, (m, n)) ->
        let qt =
            items
            |> QuadTree.fromBoundaryOjects boxAround m n true
        let qt = qt.Split(m * 2, n / 2)
        let allItems =
            QuadNode.allItems qt.Root
            |> Set.ofSeq
        let original =
            Set items
        allItems = original
    )

[<Property>]
let ``If an item is within the bounds of a node, then it is listed in the content of that node``() =
    let genPars = Gen.zip (Gen.choose(1, 5)) (Gen.choose(10, 20))
    let genAll = Gen.zip genIntItems genPars
    let arbAll = Arb.fromGen genAll
    Prop.forAll arbAll (fun (items, (m, n)) ->
        let qt =
            items
            |> QuadTree.fromBoundaryOjects boxAround m n true
        let qt = qt.Split(m * 2, n / 2)
        let rec allNodesOf (node : QuadNode<_>) =
            seq {
                if node.Children.Length = 0 then
                    yield node
                else
                for child in node.Children do
                    yield! allNodesOf child
            }
        qt.Root
        |> allNodesOf
        |> Seq.forall (fun node ->
            items
            |> List.forall (fun ((_, x, y) as item) ->
                let isInside =
                    node.Min.X <= x && x <= node.Max.X &&
                    node.Min.Y <= y && y <= node.Max.Y
                let isListed = node.Content |> Array.exists ((=) item)
                (not isInside || isListed)
            )
        )
    )

let genFloat100 = Gen.choose(0, 1000) |> Gen.map float32 |> Gen.map (fun x -> x / 10.0f)

let genVector2 =
    Gen.two genFloat100
    |> Gen.map (fun (x, y) -> Vector2(x, y))

let genConvexPoly =
    gen {
        let! v = genVector2
        let! numVertices = Gen.choose(5, 8)
        let! vertices =
            Gen.two genFloat100
            |> Gen.map (fun (dx, dy) -> v + Vector2(0.1f * dx, 0.1f * dy))
            |> Gen.listOfLength numVertices
        return VectorExtension.convexHull vertices
    }

[<Property>]
let ``Testing for presence inside a set of polygons is correct``() =
    let genPolys = Gen.listOf genConvexPoly
    let genAll = Gen.zip genPolys genVector2
    let arbAll = Arb.fromGen genAll
    Prop.forAll arbAll (fun (polys, v) ->
        let qt =
            polys
            |> QuadTree.fromBoundaryOjects id 5 5 false
        let finder = QuadTreeItemFinder.create id List.singleton qt
        let bruteForce =
            polys
            |> List.filter (fun poly -> v.IsInConvexPolygon poly)
            |> List.map (List.map (fun v -> v.X, v.Y))
            |> Set.ofList
        let fromFinder =
            finder.FindIntersectingItems v
            |> Seq.map (List.map (fun v -> v.X, v.Y))
            |> Set.ofSeq
        bruteForce = fromFinder
        |> Prop.trivial polys.IsEmpty
        |> Prop.classify bruteForce.IsEmpty "Outside all polygons"
        |> Prop.classify (not bruteForce.IsEmpty) "Inside a polygon"
    )
