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