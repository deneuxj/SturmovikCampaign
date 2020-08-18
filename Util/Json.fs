namespace Util.Json

open FSharp.Json

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
