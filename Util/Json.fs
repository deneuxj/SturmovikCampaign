namespace Util.Json

open FSharp.Json
open System.Numerics

/// Add support for JSON (de)serialization of TimeSpan, using attribute FSharp.Json.JsonField(Transform = typeof<TimeSpanTransform>)
type TimeSpanTransform() =
    interface ITypeTransform with
        member this.targetType(): System.Type =
            typeof<int64>

        member this.fromTargetType(arg1: obj): obj =
            let ticks : int64 = unbox arg1
            System.TimeSpan(ticks) :> obj

        member this.toTargetType(arg1: obj): obj =
            match arg1 with
            | :? System.TimeSpan as span -> box span.Ticks
            | _ -> failwith "Not a TimeSpan"

type TimeSpanJsonField() =
    inherit JsonField(Transform = typeof<TimeSpanTransform>)

/// Add support for JSON (de)serialization of Vector2, using attribute FSharp.Json.JsonField(Transform = typeof<Vector2Transform>)
type Vector2Transform() =
    interface ITypeTransform with
        member this.targetType(): System.Type =
            typeof<float32 * float32>

        member this.fromTargetType(arg1: obj): obj =
            let xy : float32 * float32 = unbox arg1
            let x, y = xy
            Vector2(x, y) :> obj

        member this.toTargetType(arg1: obj): obj =
            match arg1 with
            | :? Vector2 as v -> box(v.X, v.Y)
            | _ -> failwith "Not a TimeSpan"

type Vector2JsonField() =
    inherit JsonField(Transform = typeof<Vector2Transform>)