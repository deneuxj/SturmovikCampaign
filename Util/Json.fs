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
            | _ -> failwith "Not a Vector2"

type Vector2ListTransform() =
    interface ITypeTransform with
        member this.targetType(): System.Type =
            typeof<(float32 * float32) list>

        member this.fromTargetType(arg1: obj): obj =
            let xys : (float32 * float32) list = unbox arg1
            xys
            |> List.map (fun (x, y) -> Vector2(x, y))
            :> obj

        member this.toTargetType(arg1: obj): obj =
            match arg1 with
            | :? (Vector2 list) as vs -> vs |> List.map (fun v -> (v.X, v.Y)) |> box
            | _ -> failwith "Not a Vector2 list"

type Vector2JsonField() =
    inherit JsonField(Transform = typeof<Vector2Transform>)

type Vector2ListJsonField() =
    inherit JsonField(Transform = typeof<Vector2ListTransform>)

type AsArrayTransform<'K, 'V when 'K : comparison>() =
    interface ITypeTransform with
        member this.targetType(): System.Type =
            typedefof<_ list>.MakeGenericType(typedefof<_ * _>.MakeGenericType(typeof<'K>, typeof<'V>))

        member this.fromTargetType(arg1: obj): obj =
            let xs : ('K * 'V) list = unbox arg1
            Map.ofList xs
            :> obj

        member this.toTargetType(arg1: obj): obj =
            let argType = arg1.GetType()
            if argType.IsGenericType then
                if argType.GetGenericTypeDefinition() = typedefof<Map<_, _>> then
                    let keyType = argType.GetGenericArguments().[0]
                    let argType = argType.GetGenericArguments().[1]
                    let kvpType = typedefof<System.Collections.Generic.KeyValuePair<_, _>>.MakeGenericType(keyType, argType)
                    let getKeyM = kvpType.GetProperty("Key")
                    let getValueM = kvpType.GetProperty("Value")
                    let getKey x = getKeyM.GetMethod.Invoke(x, [||])
                    let getValue x = getValueM.GetMethod.Invoke(x, [||])
                    let kvps = arg1 :?> System.Collections.IEnumerable
                    let arr =
                        [
                            for kvp in kvps do
                                yield getKey kvp, getValue kvp
                        ]
                    arr :> obj
                else
                    failwith "Not a map"
            else
                failwith "Not a map"

/// Informs FSharp.Json that a map is to be serialized as an array. Useful when the keys cannot be converted to/from strings
type AsArrayJsonField(keyType, valueType) =
    inherit JsonField(Transform = typedefof<AsArrayTransform<_, _>>.MakeGenericType(keyType, valueType))

[<AutoOpen>]
module Extensions =
    type JsonConfig with
        static member IL2Default =
            { JsonConfig.Default with
                serializeNone = SerializeNone.Omit
                deserializeOption = DeserializeOption.AllowOmit
                allowUntyped = true
            }
