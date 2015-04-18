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

exception UnificationFailure of string

let mkFailedUnification kind1 kind2 msg =
    sprintf "Failed to unify %A and %A (%s)" kind1 kind2 msg

let rec tryUnify =
    function
    | kind1, kind2 when kind1 = kind2 ->
        Choice1Of2(kind1)
    | ValueType.Boolean, ValueType.Integer
    | ValueType.Integer, ValueType.Boolean ->
        Choice1Of2(ValueType.Integer)
    | ValueType.Boolean, ValueType.Float
    | ValueType.Float, ValueType.Boolean ->
        Choice1Of2(ValueType.Float)
    | ValueType.Integer, ValueType.Float
    | ValueType.Float, ValueType.Integer ->
        Choice1Of2(ValueType.Float)
    | ValueType.Composite comp, ValueType.Mapping kind
    | ValueType.Mapping kind, ValueType.Composite comp ->
        tryUnifyMappingAndComposite kind comp
    | ValueType.Composite kinds1, ValueType.Composite kinds2 ->
        tryUnifyComposites kinds1 kinds2
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

and tryUnifyMappingAndComposite kind comp =
    let unified =
        comp
        |> Map.fold (fun m k (v, multMin, multMax) ->
            match m, tryUnify(v, kind) with
            | Choice2Of2 _ as err, _ ->
                err
            | Choice1Of2 m, Choice1Of2 kind ->
                Map.add k (kind, multMin, multMax) m
                |> Choice1Of2
            | Choice1Of2 m, Choice2Of2 msg ->
                Choice2Of2(mkFailedUnification v kind msg)) (Choice1Of2 Map.empty)
    match unified with
    | Choice1Of2 m -> Choice1Of2(ValueType.Composite m)
    | Choice2Of2 msg -> Choice2Of2 msg

and tryUnifyComposites kinds1 kinds2 =
    let inOne = kinds1 |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq
    let inTwo = kinds2 |> Seq.map (fun kvp -> kvp.Key) |> Set.ofSeq 
    let merged = ref Map.empty
    for label1 in Set.difference inOne inTwo do
        let (kind, _, maxMult) = kinds1.[label1]
        merged := Map.add label1 (kind, Zero, maxMult) !merged
    for label2 in Set.difference inTwo inOne do
        let (kind, _, maxMult) = kinds2.[label2]
        merged := Map.add label2 (kind, Zero, maxMult) !merged
    let rec work labels =
        match labels with
        | [] ->
            Choice1Of2(ValueType.Composite !merged)
        | label :: rest ->
            let (kind1, minMult1, maxMult1) = kinds1.[label]
            let (kind2, minMult2, maxMult2) = kinds2.[label]
            match tryUnify(kind1, kind2) with
            | Choice1Of2 kind ->
                let minMult = least(minMult1, minMult2)
                let maxMult = most(maxMult1, maxMult2)
                merged := Map.add label (kind, minMult, maxMult) !merged
                work rest
            | Choice2Of2 msg ->
                Choice2Of2(mkFailedUnification kind1 kind2 msg)
    work (Set.intersect inOne inTwo |> List.ofSeq)

let tryUnifyMap (n : string) kind oldKinds =
    match Map.tryFind n oldKinds with
    | Some oldKind ->
        match tryUnify(oldKind, kind) with
        | Choice1Of2 kind ->
            Choice1Of2(Map.add n kind oldKinds)
        | Choice2Of2 msg ->
            Choice2Of2(mkFailedUnification oldKind kind msg)
    | None ->
        Choice1Of2(Map.add n kind oldKinds)

let unifyMap n kind oldKinds =
    match tryUnifyMap n kind oldKinds with
    | Choice1Of2 d -> d
    | Choice2Of2 msg -> raise(UnificationFailure(msg))

let tryUnifyMultMap (n : string) (kind, minMult, maxMult) oldKinds =
    match Map.tryFind n oldKinds with
    | Some (oldKind, oldMin, oldMax) ->
        match tryUnify(oldKind, kind) with
        | Choice1Of2 kind ->
            Choice1Of2(Map.add n (kind, least(minMult, oldMin), most(maxMult, oldMax)) oldKinds)
        | Choice2Of2 msg ->
            Choice2Of2(mkFailedUnification oldKind kind msg)
    | None ->
        Choice1Of2(Map.add n (kind, MinOne, MaxOne) oldKinds)

let unifyMultMap n kind oldKinds =
    match tryUnifyMultMap n kind oldKinds with
    | Choice1Of2 d -> d
    | Choice2Of2 msg -> raise(UnificationFailure(msg))
