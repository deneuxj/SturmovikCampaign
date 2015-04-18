﻿module SturmovikMission.DataProvider.AutoSchema

open SturmovikMission.DataProvider.Ast
open SturmovikMission.DataProvider.Parsing

#if VERBOSE_TRY_PARSE
let wrap desc f x =
    printfn "TRY %s" desc
    match f x with
    | Some _ as v ->
        printfn "OK"
        v
    | v ->
        printfn "KO"
        v
#else
let inline wrap _ f x = f x
#endif

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
//                    printfn "Failed to parse RHS %s" (getContext s)
                    None
            | _ ->
//                printfn "Failed to parse LHS %s" (getContext s)
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
//                        printfn "Could not identify 3rd ground type: %s" (getContext s)
                        None
                | _ ->
//                    printfn "Could not 2nd colon: %s" (getContext s)
                    None
            | None ->
//                printfn "Could not identify 2nd ground type: %s" (getContext s)
                None
        | _ ->
//            printfn "Could not 1st colon: %s" (getContext s)
            None
    | None ->
//        printfn "Could not identify 1st ground type: %s" (getContext s)
        None

and tryParseAsVector =
    function
    | ReLit "[" s ->
        let rec work s =
            match s with
            | ReInt(_, ReLit "]" s)
            | ReLit "]" s ->
                Some s
            | ReInt(n, ReLit "," s) ->
                work s
            | _ ->
                None
        match work s with
        | Some s ->
            Some(ValueType.IntVector, s)
        | None ->
            None
    | _ ->
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

and tryParseAsDate =
    function
    | ReDate(n, s) -> Some(ValueType.Date, s)
    | _ -> None

and tryGetGroundParser s =
    let funs = [
        wrap "INT" tryParseAsInt
        wrap "DATE" tryParseAsDate
        wrap "FLOAT" tryParseAsFloat
        wrap "STR" tryParseAsString
    ]
    funs
    |> Seq.tryPick (fun f -> f s |> Option.map (fun _ -> f))

and tryParseGround s =
    tryGetGroundParser s
    |> Option.bind (fun f -> f s)

and tryGetParser s =
    let funs = [
        wrap "TRIP" tryParseAsTriplet
        wrap "PAIR" tryParseAsPair
        wrap "STR" tryParseAsString
        wrap "DATE" tryParseAsDate
        wrap "INT" tryParseAsInt
        wrap "FLOAT" tryParseAsFloat
        wrap "VEC" tryParseAsVector
        wrap "SET" tryParseAsSet
        wrap "MAP" tryParseAsMapping
        wrap "COMP" tryParseAsComposite
    ]
    funs
    |> Seq.tryPick (fun f -> f s |> Option.map (fun _ -> f))
 
and tryParse s =
    tryGetParser s
    |> Option.bind (fun f -> f s)

let rec getTopType types s =
    match s with
    | EOF s ->
        [], s
    | ReId("Group", s) ->
        // Try to parse as a composite, then lift its content
        match tryParseAsComposite s with
        | Some (ValueType.Composite comps, s) ->
            let composites =
                comps
                |> List.filter (function (_, ValueType.Composite _) -> true | _ -> false)
            composites, s
        | Some _ ->
            failwith "tryParseAsComposite returned a non-composite"
        | None ->
            parseError("Failed to parse Group", s)
    | ReId(n, s) ->
        match Map.tryFind n types with
        | Some kind ->
            let (ParserFun f) = makeParser kind
            try
                let x, s = f s
                [(n, kind)], s
            with
            | _ ->
                match tryParse s with
                | Some (kind2, s) ->
                    match tryUnify(kind, kind2) with
                    | Choice1Of2 kind -> 
                        [(n, kind)], s
                    | Choice2Of2 msg ->
                        failwith "Unification failure: %s" msg
                | None ->
                    parseError("Failed to extend seen structure", s)
        | None ->
            match (wrap "COMP" tryParseAsComposite) s with
            | Some (kind, s) ->
                [(n, kind)], s
            | None ->
                parseError("Failed to guess unseen structure", s)
    | _ ->
        parseError("No structure identifier found", s)

let getTopTypes s : Map<string, ValueType> * Stream =
    let rec work types s =
        match s with
        | EOF s ->
            types, s
        | _ ->
            let newTypes, s =
                try
                    getTopType types s
                with
                | :? ParseError as e ->
                    printParseError e
                    |> String.concat "\n"
                    |> printfn "%s"
                    [], s
                | e ->
                    printfn "FAILED: %s" e.Message
                    [], s
            let types =
                newTypes
                |> List.fold (fun types (kw, kind) -> Map.add kw kind types) types
//            for (n, k) in newTypes do
//                printfn "FOUND %s: %A" n k
            let types, s = work types s 
            types, s

    work Map.empty s
