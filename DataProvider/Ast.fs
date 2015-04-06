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
