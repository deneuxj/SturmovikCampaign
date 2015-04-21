module SturmovikMission.DataProvider.Ast


type MinMultiplicity = Zero | MinOne

type MaxMultiplicity = MaxOne | Multiple

let least =
    function
    | Zero, _
    | _, Zero -> Zero
    | MinOne, MinOne -> MinOne

let most =
    function
    | Multiple, _
    | _, Multiple -> Multiple
    | MaxOne, MaxOne -> MaxOne

type ValueType =
    | Boolean
    | Integer
    | String
    | Float
    | Composite of Map<string, ValueType * MinMultiplicity * MaxMultiplicity>
    | Mapping of ValueType // { 1 = XXX1; 2 = XXX2 }
    | Set of ValueType // { entries }
    | IntVector // [1, 2, 3]
    | Pair of ValueType * ValueType
    | Triplet of ValueType * ValueType * ValueType
    | Date


type Value =
    | Boolean of bool
    | Integer of int
    | String of string
    | Float of float
    | Composite of (string * Value) list
    | Mapping of (string * Value) list
    | Set of Value list
    | IntVector of int list
    | Pair of Value * Value
    | Triplet of Value * Value * Value
    | Date of int * int * int


let rec dump (value : Value) : string =
    match value with
    | Boolean b -> if b then "1" else "0"
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


type Data =
    | Leaf of string * Value
    | Group of GroupData

and GroupData =
    { Name : string
      Index : int
      Description : string
      Data : Data list
    }


type Mission =
    { Version : int
      Content : Data list
    }



