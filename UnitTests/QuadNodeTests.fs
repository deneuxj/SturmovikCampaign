module QuadNodeTests

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
            |> Gen.map (fun (dx, dy) -> v + Vector2(0.3f * dx, 0.3f * dy))
            |> Gen.listOfLength numVertices
        return VectorExtension.convexHull vertices
    }

[<Property(MaxTest=1000)>]
let ``Existence of separating axes is consistent with vertices checks between polygons``() =
    let genAll = Gen.zip genConvexPoly genConvexPoly
    let arbAll = Arb.fromGen genAll
    Prop.forAll arbAll (fun (poly1, poly2) ->
        let vertex1In2 =
            poly1
            |> Seq.exists (fun v -> v.IsInConvexPolygon poly2)
        let vertex2In1 =
            poly2
            |> Seq.exists (fun v -> v.IsInConvexPolygon poly1)
        let sat = Functions.tryGetSeparatingAxis(poly1, poly2) |> Option.isSome
        ("Vertex 1 not in 2" @| not vertex1In2 .&.
         "Vectex 2 not in 1" @| not vertex2In1) .|.
        "no SAT exists" @| not sat
        |> Prop.classify sat "SAT"
    )

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

let shrinkSkip (polys : _ list) =
    polys
    |> List.mapi (fun i _ ->
        polys
        |> List.indexed
        |> List.choose (fun (j, poly) -> if i = j then None else Some poly)
    )

[<Property(MaxTest=100,StartSize=1,EndSize=10)>]
let ``Candidates from free areas do not intersect with the occupied areas``() =
    let genPolys = Gen.sized (fun s -> Gen.listOfLength (5 + s) genConvexPoly)
    let genSeed = Gen.choose(0, 1 <<< 30)
    let arbAll =
        Arb.fromGenShrink(
            Gen.zip3 genPolys genSeed genConvexPoly,
            fun (polys, seed, shape) ->
                polys
                |> shrinkSkip
                |> Seq.map (fun polys -> (polys, seed, shape))
            )
    Prop.forAll arbAll (fun (polys, seed, shape) ->
        let random = System.Random(seed)
        let qt =
            polys
            |> QuadTree.fromBoundaryOjects id 5 0 false
        let region =
            List.concat polys
            |> function
                | [] -> []
                | vs -> VectorExtension.convexHull vs
        let finder = QuadTreeItemFinder.create id id qt
        let candidates =
            match shape, region with
            | [], _ | _, [] ->
                []
            | _ ->
                FreeAreas.findPositionCandidates random finder shape region
                |> Seq.truncate 100
                |> List.ofSeq
        let noneIntersect =
            candidates
            |> Seq.forall (fun offset ->
                let shape = shape |> List.map ((+) offset)
                polys
                |> Seq.forall (fun poly -> Functions.tryGetSeparatingAxis(poly, shape) |> Option.isSome)
            )
        noneIntersect
        |> Prop.trivial polys.IsEmpty
        |> Prop.classify candidates.IsEmpty "No candidates"
    )

let getCandidatesAfterSubtraction(polys, seed, subShape) =
    let random = System.Random(seed)
    let qt =
        polys
        |> QuadTree.fromBoundaryOjects id 5 0 false
    let region =
        List.concat polys
        |> function
            | [] -> []
            | vs -> VectorExtension.convexHull vs
    let qt2 =
        { qt with
            Root = QuadNode.insert qt.Intersects qt.MaxDepth qt.MinItems qt.ContentInInnerNodes subShape qt.Root
        }
    let finder = QuadTreeItemFinder.create id id qt2
    let shape = [
        for deg in 0.0f .. 45.0f .. 359.0f do
            let rad = deg / 180.0f * float32 System.Math.PI
            yield 5.0f * Vector2(cos rad, sin rad)
    ]
    let candidates =
        match region with
        | [] ->
            []
        | _ ->
            FreeAreas.findPositionCandidates random finder shape region
            |> Seq.truncate 10
            |> List.ofSeq
    let candidates =
        candidates
        |> List.map (fun offset -> shape |> List.map ((+) offset))
    qt, qt2, candidates

let arbSubtraction =
    let genPolys = Gen.sized (fun s -> Gen.listOfLength (5 + s) genConvexPoly)
    let genSeed = Gen.choose(0, 1 <<< 30)
    let genAll =
        gen {
            let! polys = genPolys
            let! seed = genSeed
            let! subShape = genConvexPoly

            return (polys, seed, subShape)
        }
    let arbAll =
        Arb.fromGenShrink(
            genAll,
            fun (polys, seed, subShape) ->
                let shrunkPolys = shrinkSkip polys
                seq {
                    for polys in shrunkPolys do
                        yield (polys, seed, subShape)
                }
            )
    let arbAll = Arb.fromGen genAll
    arbAll

//[<Property(MaxTest=50,StartSize=1,EndSize=10,Replay="861517953, 296890303")>]
[<Property(MaxTest=100,StartSize=1,EndSize=10)>]
let ``Subtracting from free areas eliminates candidates from the subtracted areas``() =
    let bb (v1 : Vector2, v2 : Vector2) =
        [v1; Vector2(v2.X, v1.Y); v2; Vector2(v1.X, v2.Y)]
    let ss =
        List.ofSeq
        >> List.map (List.map string >> String.concat ";" >> sprintf "[%s]")
        >> List.chunkBySize 10
        >> List.map (String.concat ";")
        >> String.concat ";\n"
        >> sprintf "[%s]"
    let bbofqt =
        QuadNode.allLeaves
        >> Seq.map (fun n -> n.Min, n.Max)
        >> Seq.map bb
        >> ss

    let arbAll = arbSubtraction
    Prop.forAll arbAll (fun (polys, seed, subShape) ->
        let subShapeIntersects =
            polys
            |> Seq.exists (fun poly ->
                Functions.tryGetSeparatingAxis(poly, subShape)
                |> Option.isNone)
        let qt, qt2, candidates = getCandidatesAfterSubtraction(polys, seed, subShape)
        let noneIntersect =
            candidates
            |> Seq.forall (fun shape ->
                subShape :: polys
                |> Seq.forall (fun poly -> Functions.tryGetSeparatingAxis(poly, shape) |> Option.isSome)
            )
        noneIntersect |@ sprintf "%A" (candidates, bbofqt qt.Root, bbofqt qt2.Root)
        |> Prop.trivial (polys.IsEmpty || not subShapeIntersects)
        |> Prop.classify candidates.IsEmpty "No candidates"
    )