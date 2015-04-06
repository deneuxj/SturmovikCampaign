module SturmovikMission.DataProvider.Ast


type ValueType =
    | Integer
    | String
    | Float
    | Composite of (string * ValueType) list
    | Mapping of ValueType // { 1 = XXX1; 2 = XXX2 }
    | Set of ValueType // { entries }
    | IntVector // [1, 2, 3]
    | Pair of ValueType * ValueType
    | Triplet of ValueType * ValueType * ValueType
    | Time
    | Date


type Value =
    | Integer of int
    | String of string
    | Float of float
    | Composite of (string * Value) list
    | Mapping of (string * Value) list
    | Set of Value list
    | IntVector of int list
    | Pair of Value * Value
    | Triplet of Value * Value * Value
    | Time of int * int * int
    | Date of int * int * int


let rec dump (value : Value) : string =
    match value with
    | Integer i -> sprintf "%d" i
    | String s -> sprintf "\"%s\"" s
    | Float f -> sprintf "%f" f
    | Composite content ->
        seq {
            yield sprintf "{\n"
            for (k, v) in content do
                match v with
                | Set _
                | Composite _ ->
                    yield sprintf "%s\n%s" k (dump v)
                | _ ->
                    yield sprintf "%s = %s;\n" k (dump v)
            yield "}\n"
        }
        |> String.concat ""
    | Date (day, month, year) ->
        sprintf "%d.%d.%d" day month year
    | Time (hour, minute, second) ->
        sprintf "%d:%d:%d" hour minute second
    | Pair (x1, x2) ->
        sprintf "%s : %s" (dump x1) (dump x2)
    | Triplet (x1, x2, x3) ->
        sprintf "%s : %s : %s" (dump x1) (dump x2) (dump x3)
    | IntVector xs ->
        let content = 
            seq {
                for x in xs do
                    yield sprintf "%d" x
            }
            |> String.concat ", "
        sprintf "[%s]" content
    | Set xs ->
        seq {
            yield "{\n"
            for x in xs do
                yield dump x
                yield ";\n"
            yield "}\n"
        }
        |> String.concat ""
    | Mapping content ->
        seq {
            yield sprintf "{\n"
            for (k, v) in content do
                yield sprintf "%s = %s;\n" k (dump v)
            yield "}\n"
        }
        |> String.concat ""

exception UnificationFailure of string

let mkFailedUnification kind1 kind2 msg =
    sprintf "Failed to unify %A and %A (%s)" kind1 kind2 msg

let rec tryUnify =
    function
    | kind1, kind2 when kind1 = kind2 ->
        Choice1Of2(kind1)
    | ValueType.Integer, ValueType.Float
    | ValueType.Float, ValueType.Integer ->
        Choice1Of2(ValueType.Float)
    | ValueType.Composite kinds1, ValueType.Composite kinds2 ->
        let unifyInternally kinds initial =
            kinds
            |> List.fold (fun map (kw, kind) ->
                match Map.tryFind kw map with
                | Some kind2 ->
                    match tryUnify(kind, kind2) with
                    | Choice1Of2 kind ->
                        Map.add kw kind map
                    | Choice2Of2 msg ->
                        raise(UnificationFailure(
                                mkFailedUnification kind kind2 msg))
                | None ->
                    Map.add kw kind map
                ) initial
        try
            let unified =
                Map.empty
                |> unifyInternally kinds1
                |> unifyInternally kinds2
                |> Seq.map (fun kvp -> (kvp.Key, kvp.Value))
                |> List.ofSeq
            Choice1Of2(ValueType.Composite unified)
        with
        | :? UnificationFailure as e ->
            Choice2Of2(e.Data0)
    | ValueType.Mapping kind1, ValueType.Mapping kind2 ->
        match tryUnify(kind1, kind2) with
        | Choice1Of2 kind ->
            Choice1Of2(ValueType.Mapping kind)
        | Choice2Of2 msg ->
            Choice2Of2 (mkFailedUnification kind1 kind2 msg)
    | ValueType.Set kind1, ValueType.Set kind2 ->
        match tryUnify(kind1, kind2) with
        | Choice1Of2 kind ->
            Choice1Of2(ValueType.Set kind)
        | Choice2Of2 msg ->
            Choice2Of2 (mkFailedUnification kind1 kind2 msg)
    | ValueType.Pair(kindA1, kindA2) as p1, (ValueType.Pair(kindB1, kindB2) as p2) ->
        match tryUnify(kindA1, kindB1), tryUnify(kindA2, kindB2) with
        | Choice1Of2 kind1, Choice1Of2 kind2 ->
            Choice1Of2(ValueType.Pair(kind1, kind2))
        | Choice2Of2 msg, _
        | _, Choice2Of2 msg ->
            Choice2Of2 (mkFailedUnification p1 p2 msg)
    | ValueType.Triplet(kindA1, kindA2, kindA3) as p1, (ValueType.Triplet(kindB1, kindB2, kindB3) as p2) ->
        match tryUnify(kindA1, kindB1), tryUnify(kindA2, kindB2), tryUnify(kindA3, kindB3) with
        | Choice1Of2 kind1, Choice1Of2 kind2, Choice1Of2 kind3 ->
            Choice1Of2(ValueType.Triplet(kind1, kind2, kind3))
        | Choice2Of2 msg, _, _
        | _, Choice2Of2 msg, _
        | _, _, Choice2Of2 msg ->
            Choice2Of2 (mkFailedUnification p1 p2 msg)
    | kind1, kind2 ->
        Choice2Of2 (mkFailedUnification kind1 kind2 "Incompatible value types")
