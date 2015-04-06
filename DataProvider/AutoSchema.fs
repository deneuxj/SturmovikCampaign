module SturmovikMission.DataProvider.AutoSchema

open SturmovikMission.DataProvider.Ast
open SturmovikMission.DataProvider.Parsing

let rec tryParseAsComposite (s : Stream) =
    match s with
    | ReLit "{" s ->
        let rec work s =
            match s with
            | ReLit "}" s ->
                Some([], s)
            | ReId(n, ((ReLit "{" _) as s))
            | ReId(n, ReLit "=" s) ->
                match tryParse s with
                | Some (kind, s) ->
                    match s with
                    | ReLit ";" s
                    | s ->
                        match work s with
                        | Some (kinds, s) ->
                            Some((n, kind) :: kinds, s)
                        | None ->
                            None
                | None ->
                    None
            | _ ->
                None
        match work s with
        | Some (kinds, s) ->
            Some(ValueType.Composite kinds, s)
        | None ->
            None
    | _ ->
        None

and tryParseAsMapping (s : Stream) =
    match s with
    | ReLit "{" s ->
        let rec work tryParseValue s =
            match s with
            | ReLit "}" s ->
                Some(s)
            | ReId(n, ReLit "=" s) ->
                match tryParseValue s with
                | Some (kind, s) ->
                    match s with
                    | ReLit ";" s
                    | s ->
                        match work tryParseValue s with
                        | Some (s) ->
                            Some(s)
                        | None ->
                            None
                | None ->
                    None
            | _ ->
                None
        match s with
        | ReId(_, ReLit "=" s2) ->
            match tryGetParser s2 with
            | Some f ->
                match work f s with
                | Some (s) ->
                    match f s2 with
                    | Some(kind, _) ->
                        Some(ValueType.Mapping (kind), s)
                    | None ->
                        // tryGetParser lied to us! Can't parse s2.
                        failwith "Inconsistency with tryGetParser"
                | None ->
                    None
            | None ->
                None
        | _ ->
            None
    | _ ->
        None

and tryParseAsSet (s : Stream) =
    match s with
    | ReLit "{" s ->
        let rec work tryParseValue s =
            match s with
            | ReLit "}" s ->
                Some(s)
            | s ->
                match tryParseValue s with
                | Some (kind, s) ->
                    match s with
                    | ReLit ";" s
                    | s ->
                        match work tryParseValue s with
                        | Some (s) ->
                            Some(s)
                        | None ->
                            None
                | None ->
                    None

        match tryGetParser s with
        | Some f ->
            match work f s with
            | Some (s2) ->
                match f s with
                | Some(kind, _) ->
                    Some(ValueType.Set (kind), s2)
                | None ->
                    // tryGetParser lied to us! Can't parse s.
                    failwith "Inconsistency with tryGetParser"
            | None ->
                None
        | None ->
            None
    | _ ->
        None

and tryParseAsPair s =
    match tryParseGround s with
    | Some (kind, s) ->
        match s with
        | ReLit ":" s ->
            match tryParseGround s with
            | Some (kind2, s) ->
                Some(ValueType.Pair(kind, kind2), s)
            | None ->
                None
        | _ ->
            None
    | None ->
        None

and tryParseAsTriplet s =
    match tryParseGround s with
    | Some (kind, s) ->
        match s with
        | ReLit ":" s ->
            match tryParseGround s with
            | Some (kind2, s) ->
                match s with
                | ReLit ":" s ->
                    match tryParseGround s with
                    | Some (kind3, s) ->
                        Some(ValueType.Triplet(kind, kind2, kind3), s)
                    | None ->
                        None
                | _ ->
                    None
            | None ->
                None
        | _ ->
            None
    | None ->
        None

and tryParseAsInt =
    function
    | ReInt(n, s) -> Some(ValueType.Integer, s)
    | _ -> None

and tryParseAsString =
    function
    | ReString(n, s) -> Some(ValueType.String, s)
    | _ -> None

and tryParseAsFloat =
    function
    | ReFloat(n, s) -> Some(ValueType.Float, s)
    | _ -> None

and tryParseAsTime =
    function
    | ReTime(n, s) -> Some(ValueType.Time, s)
    | _ -> None

and tryParseAsDate =
    function
    | ReDate(n, s) -> Some(ValueType.Date, s)
    | _ -> None

and tryGetGroundParser s =
    let funs = [
        tryParseAsDate
        tryParseAsTime
        tryParseAsFloat
        tryParseAsInt
        tryParseAsString
    ]
    funs
    |> Seq.tryPick (fun f -> f s |> Option.map (fun _ -> f))

and tryParseGround s =
    tryGetGroundParser s
    |> Option.bind (fun f -> f s)

and tryGetParser s =
    let funs = [
        tryParseAsComposite
        tryParseAsMapping
        tryParseAsSet
        tryParseAsTriplet
        tryParseAsPair
        tryParseAsDate
        tryParseAsTime
        tryParseAsFloat
        tryParseAsInt
        tryParseAsString
    ]
    funs
    |> Seq.tryPick (fun f -> f s |> Option.map (fun _ -> f))
 
and tryParse s =
    tryGetParser s
    |> Option.bind (fun f -> f s)

let tryGetTopType =
    function
    | ReId(n, s) ->
        match tryParse s with
        | Some (kind, s) ->
            Some((n, kind), s)
        | None ->
            None
    | _ ->
        None

let getTopTypes s =
    let rec work s =
        match tryGetTopType s with
        | Some((n, k), s) ->
            let xs, s = work s 
            (n, k) :: xs, s
        | None ->
            [], s
    work s

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
