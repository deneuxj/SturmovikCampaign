module Vector

open System.Numerics

type T = SturmovikMissionTypes.Provider<"C:\Users\johann\Documents\SturmovikMission-git\data\Sample.mission", "">

type Vector2
with
    static member inline FromPos(pos : ^T) =
        let x = (^T : (member XPos : T.Float) pos).Value
        let y = (^T : (member ZPos : T.Float) pos).Value
        Vector2(float32 x, float32 y)

    static member FromPair((x, y) : float * float) =
        Vector2(float32 x, float32 y)

    static member FromPair(p : T.FloatPair) =
        Vector2.FromPair(p.Value)

    static member inline FromYOri(ori : ^T) =
        let angle = (^T : (member YOri : T.Float) ori).Value
        let alpha = System.Math.PI * angle / 180.0
        Vector2(float32 <| cos alpha, float32 <| sin alpha)

    static member Cross(u : Vector2, v : Vector2) =
        u.X * v.Y - u.Y * v.X

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
