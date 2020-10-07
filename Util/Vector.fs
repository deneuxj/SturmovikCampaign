module VectorExtension

open System.Numerics

let convexHull (points : Vector2 list) =
    let ccw(p1 : Vector2, p2 : Vector2, p3 : Vector2) =
        (p2.X - p1.X) * (p3.Y - p1.Y) - (p2.Y - p1.Y) * (p3.X - p1.X) >= 0.0f
    let lowest, points =
        let rec work (lowest : Vector2) points (working : Vector2 list) =
            match working with
            | [] -> lowest, points
            | pt :: working ->
                if pt.Y < lowest.Y then
                    work pt (lowest :: points) working
                else
                    work lowest (pt :: points) working
        match points with
        | [] -> invalidArg "points" "Must not be empty"
        | lowest :: working -> work lowest [] working
    let points =
        points
        |> List.sortByDescending (fun v ->
            let l = (v - lowest).Length()
            if l = 0.0f then
                0.0f
            else
                Vector2.Dot(v - lowest, Vector2.UnitX) / l)
    let rec backoff pt outerPoints =
        match outerPoints with
        | pt2 :: ((pt1 :: _) as tail) ->
            if ccw(pt1, pt2, pt) then
                pt :: outerPoints
            else
                backoff pt tail
        | [_] ->
            pt :: outerPoints
        | [] ->
            failwith "outerPoints should never become empty"
    let rec work outerPoints working =
//        printfn "outerPoints: %A working: %A" outerPoints working
        match working with
        | [] -> outerPoints
        | pt :: working ->
            let outerPoints = backoff pt outerPoints
            work outerPoints working
    work [lowest] points
    |> List.rev

type Vector2
with
    static member inline FromPos(pos : ^T) =
        let x = (^T : (member GetXPos : unit -> ^F) pos)
        let y = (^T : (member GetZPos : unit -> ^F) pos)
        let x = (^F : (member Value : float) x)
        let y = (^F : (member Value : float) y)
        Vector2(float32 x, float32 y)

    static member FromPair((x, y) : float * float) =
        Vector2(float32 x, float32 y)

    static member inline FromPair(p : ^T) =
        let value = (^T : (member Value : float * float) p)
        Vector2.FromPair(value)

    static member inline FromYOri(ori : ^T) =
        let angle = (^T : (member GetYOri : unit -> ^F) ori)
        let angle = (^F : (member Value : float) angle)
        let alpha = System.Math.PI * angle / 180.0
        Vector2(float32 <| cos alpha, float32 <| sin alpha)

    static member FromYOri(ori : float) =
        let alpha = System.Math.PI * ori / 180.0
        Vector2(float32 <| cos alpha, float32 <| sin alpha)

    static member Cross(u : Vector2, v : Vector2) =
        u.X * v.Y - u.Y * v.X

    member this.Rotate(degrees : float32) =
        let angle = float32 (System.Math.PI * float degrees / 180.0)
        let cos = cos angle
        let sin = sin angle
        let x = this.X
        let y = this.Y
        Vector2(
            x * cos - y * sin,
            y * cos + x * sin
        )

    member this.IsInConvexPolygon(poly : Vector2 list) =
        match poly with
        | [] -> false
        | first :: _ ->
            let cycled = poly @ [first]
            cycled
            |> List.pairwise
            |> List.forall(fun (v1, v2) ->
                let v = v2 - v1
                let w = this - v1
                let c = Vector2.Cross(v, w)
                c >= 0.0f
            )

    static member ConvexPolygonArea(poly : Vector2 list) =
        match poly with
        | [] -> 0.0f
        | p0 :: rest ->
            rest
            |> Seq.pairwise
            |> Seq.sumBy (fun (p1, p2) ->
                let v1 = p1 - p0
                let v2 = p2 - p0
                0.5f * abs(Vector2.Cross(v1, v2)))

    member this.DistanceFromSegment(v1 : Vector2, v2 : Vector2) =
        let w = v2 - v1
        let wl = w.Length()
        let v = this - v1
        if wl <= 1000.0f * System.Single.Epsilon then
            v.Length()
        else
            let w = w / wl
            let dot = Vector2.Dot(v, w)
            if dot < 0.0f then
                v.Length()
            elif dot > wl then
                (this - v2).Length()
            else
                abs(Vector2.Cross(v, w))

    member this.DistanceFromPath(path : Vector2 seq) =
        path
        |> Seq.pairwise
        |> Seq.map (fun (v1, v2) -> this.DistanceFromSegment(v1, v2))
        |> Seq.min

    member this.YOri =
        let angle = atan2 this.Y this.X
        180.0f * angle / float32 System.Math.PI
        |> fun degrees ->
            if degrees < 0.0f then
                degrees + 360.0f
            else
                degrees

    static member Center(vs : Vector2 list) =
        match List.length vs with
        | 0 -> invalidArg "vs" "Must have at least one vertex"
        | n ->
            vs
            |> List.sum
            |> fun s -> s * (1.0f / (float32 n))

    /// Compute intersecion point of two segments, if any
    static member Intersection ((v1, v2), (w1, w2)) =
        let r = v2 - v1
        let s = w2 - w1
        let rs = Vector2.Cross(r, s)
        let qp = w1 - v1
        let qpr = Vector2.Cross(qp, r)
        if rs = 0.0f && qpr = 0.0f then
            // Colinear segments, return endpoint that is between the other vector's endpoints, if any
            seq {
                for (v1, v2), ws in [((v1, v2), [w1; w2]); ((w1, w2), [v1; v2])] do
                    for w in ws do
                        if Vector2.Dot(w - v1, w - v2) <= 0.0f then
                            Some w
                        else
                            None
                }
            |> Seq.tryPick id
        elif rs = 0.0f then
            // Parallel
            None
        else
            // Non parallel
            let t = Vector2.Cross(qp, s) / rs
            let u = qpr / rs
            if 0.0f <= t && t <= 1.0f && 0.0f <= u && u <= 1.0f then
                // intersection within both segments
                Some (v1 + t * r)
            else
                // intersection outside of one of the segments
                None

[<ReferenceEquality>]
[<NoComparison>]
/// Looping linked list of vertices
type PolygonIntersectionNode =
    {
        Pos : Vector2
        mutable Next : PolygonIntersectionNode
    }
with
    member this.Edge = (this.Pos, this.Next.Pos)

    member this.Contains(v : Vector2) =
        let start = this
        let rec work (sign, node) =
            Vector2.Cross(node.Next.Pos - node.Pos, v - node.Pos) * sign >= 0.0f && (node.Next = start || work(sign, node.Next))
        work(1.0f, this) || work(-1.0f, this)

    member this.Reverse() =
        let rec work(start, node) =
            let next = node.Next
            next.Next <- node
            if not(next = start) then
                work(start, next)
        work(this, this)

    member this.Vertices =
        seq {
            let mutable node = this
            yield node.Pos
            while node.Next <> this do
                node <- node.Next
                yield node.Pos
        }

let intersectConvexPolygons(poly1 : Vector2 list, poly2 : Vector2 list) =
    let toPolyNode (poly : Vector2 list) =
        let nodes =
            poly
            |> List.distinct
            |> List.map (fun v -> { Pos = v; Next = Unchecked.defaultof<PolygonIntersectionNode>})
        match nodes with
        | first :: _ ->
            for prev, curr in Seq.pairwise (nodes @ [first]) do
                prev.Next <- curr
            first
        | [] ->
            failwith "Cannot get node from empty polygon"

    let loop1 = toPolyNode poly1
    let loop2 = toPolyNode poly2


    // Find the first edge in poly1 that has an intersection with some edge in poly2. Return the
    // start vertex of the first such edge in poly2, if any.
    let findIntersection(poly1, poly2) =

        let rec loop2 (node1 : PolygonIntersectionNode, node2 : PolygonIntersectionNode) =
            match Vector2.Intersection(node1.Edge, node2.Edge) with
            | Some posI ->
                Some(node2, posI)
            | None ->
                if node2.Next = poly2 then
                    None
                else
                    loop2(node1, node2.Next)

        let rec loop1(start1, start2) =
            match loop2 (start1, start2) with
            | Some(node2, posI) ->
                Some(start1, node2, posI)
            | None ->
                if start1.Next = poly1 then
                    None
                else
                    loop1(start1.Next, start2)

        loop1(poly1, poly2)

    // Walk over the outlines of two polygons, yielding the intersections and vertices inside the other polygon
    let intersectionOutline =
        match findIntersection(loop1, loop2) with
        | None ->
            if loop1.Vertices |> Seq.forall loop2.Contains then
                // poly1 inside poly2
                Some poly1
            elif loop2.Vertices |> Seq.forall loop1.Contains then
                // poly2 inside poly2
                Some poly2
            else
                // No overlap
                None
        | Some(start1, start2, pos) ->
            // Decide what to do at an intersection point
            let rec decide(node1 : PolygonIntersectionNode, node2 : PolygonIntersectionNode, pos) =
                seq {
                    // All intersection points are always part of the outline
                    yield pos
                    if node2.Contains(node1.Next.Pos) then
                        // Vertex after node1 is in poly2, yield all successors in poly1 until next intersection point
                        yield! direct(node1.Next, node2)
                    elif node1.Contains(node2.Next.Pos) then
                        // Vertex after node2 is in poly1, yield all successors in poly2 until next intersection point
                        yield! direct(node2.Next, node1)
                    else
                        // Neither successors are part of the outline.
                        // Look for next intersection and pick up from there
                        match findIntersection(node1, node2.Next) with
                        | Some(node1, node2, pos) ->
                            yield! decide(node2, node1, pos)
                        | None ->
                            ()
                }
            // Yield node1 and its successors that are in the intersection
            and direct(node1, node2) =
                seq {
                    yield node1.Pos
                    if node2.Contains(node1.Next.Pos) then
                        yield! direct(node1.Next, node2)
                    else
                        match findIntersection(node1, node2) with
                        | Some(node1, node2, pos) ->
                            yield! decide(node1, node2, pos)
                        | None ->
                            ()
                }
            // The sequence of vertices in the outline, repeating.
            let looping = decide(start1, start2, pos)
            let p1 = looping |> Seq.head
            // The first occurrence of the repeating loop.
            seq {
                yield p1
                let len1 = p1.Length()
                yield!
                    looping
                    |> Seq.skip 1
                    |> Seq.takeWhile (fun p -> (p1 - p).Length() / len1 > 0.00001f)
            }
            |> List.ofSeq
            |> List.distinct
            |> Some

    intersectionOutline

let mkCircle(center, radius : float32) =
    [
        for alpha in 0.0f..30.0f..359.0f do
            yield Vector2.FromYOri(float alpha) * radius + center
    ]

let mkSquare(center, halfSide : float32) =
    [
        Vector2(halfSide, halfSide)
        Vector2(-halfSide, halfSide)
        Vector2(-halfSide, -halfSide)
        Vector2(halfSide, -halfSide)
    ]
    |> List.map ((+) center)

let testConvexHull (random : System.Random) N =
    let nextFloat() =
        random.NextDouble()
        |> fun x -> x * 10.0
        |> floor
        |> fun x -> x / 10.0
        |> float32
    let points =
        List.init N (fun _ -> Vector2(nextFloat(), nextFloat()))
    let hull = convexHull points
    let ok =
        points
        |> List.forall (fun p ->
            let ok = p.IsInConvexPolygon(hull)
            if not ok then
                printfn "%A not in polygon" p
            ok)
    if not ok then
        Some(points, hull)
    else
        None

let repeat rnd =
    Seq.initInfinite (fun _ -> testConvexHull rnd 4)
    |> Seq.tryPick id