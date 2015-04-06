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
