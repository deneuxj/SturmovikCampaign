module VectorExtension

open System.Numerics
open SturmovikMission.DataProvider

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

    static member FromMcu(mcuPos : Mcu.Vec3) =
        Vector2(float32 mcuPos.X, float32 mcuPos.Z) 

    member this.AssignTo(dst : Mcu.Vec3) =
        dst.X <- float this.X
        dst.Z <- float this.Y

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

    let poly1 = toPolyNode poly1
    let poly2 = toPolyNode poly2

    // Find intersection between edge starting at node1 and the segments between node2 and start2
    let rec findIntersection (start2, node1 : PolygonIntersectionNode, node2 : PolygonIntersectionNode) =
        match Vector2.Intersection(node1.Edge, node2.Edge) with
        | Some posI ->
            Some(node2, posI)
        | None ->
            if node2.Next = start2 then
                None
            else
                findIntersection(start2, node1, node2.Next)

    // Enumerate all intersections between the outline of two polygons
    let allIntersections(poly1, poly2) =
        let rec work(start1, start2) =
            seq {
                match findIntersection (start2, start1, start2) with
                | None ->
                    ()
                | Some(node2, posI) ->
                    yield start1, node2, posI
                    match findIntersection (start2, start1, node2.Next) with
                    | Some(node2, posI) ->
                        yield start1, node2, posI
                    | None -> ()
                if start1.Next <> poly1 then
                    yield! work(start1.Next, start2)
            }
        work(poly1, poly2)

    let intersectionOutline =
        if Seq.isEmpty (allIntersections(poly1, poly2)) then
            if poly1.Vertices |> Seq.forall poly2.Contains then
                Some poly1.Vertices
            elif poly2.Vertices |> Seq.forall poly1.Contains then
                Some poly2.Vertices
            else
                None
        else
            let node1, node2, posI =
                allIntersections(poly1, poly2)
                |> Seq.head
            let rec decide(node1, node2, pos) =
                seq {
                    yield pos
                    if poly2.Contains(node1.Next.Pos) then
                        yield! direct(node1.Next, poly2)
                    elif poly1.Contains(node2.Next.Pos) then
                        yield! direct(node2.Next, poly1)
                    elif poly2.Contains(node1.Pos) then
                        poly1.Reverse()
                        yield! direct(node1, poly2)
                    elif poly1.Contains(node2.Pos) then
                        poly2.Reverse()
                        yield! direct(node2, poly1)
                    else
                        let nextIntersection =
                            allIntersections(node1, node2)
                            |> Seq.tryFind (fun (n1, n2, p) -> p <> pos && n1 = node1)
                        match nextIntersection with
                        | Some (_, node2, pos) ->
                            yield! decide(node1, node2, pos)
                        | None ->
                            ()
                }
            and direct(node1, poly2) =
                seq {
                    yield node1.Pos
                    if poly2.Contains(node1.Next.Pos) then
                        yield! direct(node1.Next, poly2)
                    else
                        let closestIntersection =
                            allIntersections(node1, poly2)
                            |> Seq.filter (fun (n, _, _) -> n = node1)
                            |> Seq.sortBy (fun (_, _, p) -> (p - node1.Pos).LengthSquared())
                            |> Seq.tryHead
                        match closestIntersection with
                        | None ->
                            ()
                        | Some (_, node2, pos) ->
                            yield! decide(node1, node2, pos)
                }
            let looping = decide(node1, node2, posI)
            let p1 = looping |> Seq.head
            seq {
                yield p1
                yield!
                    looping
                    |> Seq.skip 1
                    |> Seq.takeWhile (fun p -> (p1 - p).Length() > 0.01f)
            }
            |> Some

    intersectionOutline


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