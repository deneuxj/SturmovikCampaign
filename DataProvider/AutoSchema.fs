module SturmovikMission.DataProvider.AutoSchema

open SturmovikMission.DataProvider.Ast
open SturmovikMission.DataProvider.Unification
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
        let rec work subTypes s =
            match s with
            | ReLit "}" s ->
                Some(subTypes, s)
            | ReId(n, ((ReLit "{" _) as s)) ->
                match tryParseCurly s with
                | Some (kind, s) ->
                    let subTypes = unifyMultMap n (kind, MinOne, MaxOne) subTypes
                    work subTypes s
                | None ->
//                    printfn "Failed to parse RHS %s" (getContext s)
                    None
            | ReId(n, ReLit "=" s) ->
                match tryParse s with
                | Some (kind, s) ->
                    let subTypes = unifyMultMap n (kind, MinOne, MaxOne) subTypes
                    match s with
                    | ReLit ";" s ->
                        work subTypes s
                    | _ ->
                        None
                | None ->
//                    printfn "Failed to parse RHS %s" (getContext s)
                    None
            | _ ->
//                printfn "Failed to parse LHS %s" (getContext s)
                None
        match work Map.empty s with
        | Some (kinds, s) ->
            Some(ValueType.Composite kinds, s)
        | None ->
            None
    | _ ->
        None

and tryParseAsMapping (s : Stream) =
    match s with
    | ReLit "{" s ->
        let rec work s =
            match s with
            | ReLit "}" s ->
                Some([], s)
            | ReInt(_, ReLit "=" s) ->
                match tryParseGround s with
                | Some (kind, s) ->
                    match s with
                    | ReLit ";" s ->
                        match work s with
                        | Some (kinds, s) ->
                            Some(kind :: kinds, s)
                        | None ->
                            None
                    | _ ->
                        None
                | None ->
                    None
            | _ ->
                None
        match s with
        | ReInt(_, ReLit "=" s2) ->
            match work s with
            | Some (kind :: kinds, s) ->
                let unified =
                    kinds
                    |> List.fold (fun unified kind ->
                        match unified with
                        | Choice2Of2 err as v -> v
                        | Choice1Of2 unified ->
                            Unification.tryUnify(unified, kind)) (Choice1Of2 kind)
                match unified with
                | Choice2Of2 _ -> None
                | Choice1Of2 unified -> Some(ValueType.Mapping unified, s)
            | Some ([], s2) ->
                // We got an empty mapping... No way to guess what kind is its content
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
        let rec work s =
            match s with
            | ReLit "}" s ->
                Some([], s)
            | s ->
                match tryParseGroundExt s with
                | Some (kind, s) ->
                    match s with
                    | ReLit ";" s
                    | s ->
                        match work s with
                        | Some (kinds, s) ->
                            Some(kind :: kinds, s)
                        | None ->
                            None
                | None ->
                    None

        match work s with
        | Some (kind :: kinds, s2) ->
            let unified =
                kinds
                |> List.fold (fun unified kind ->
                    match unified with
                    | Choice2Of2 err as v -> v
                    | Choice1Of2 unified ->
                        Unification.tryUnify(unified, kind)) (Choice1Of2 kind)
            match unified with
            | Choice2Of2 _ -> None
            | Choice1Of2 unified -> Some(ValueType.Set unified, s2)
        | Some ([], s2) ->
            // We got an empty set... No way to guess what kind is its content
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

and tryParseAsBool =
    function
    | ReBool(n, s) -> Some(ValueType.Boolean, s)
    | _ -> None

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

and groundParserFuns =
    [
        wrap "BOOL" tryParseAsBool
        wrap "INT" tryParseAsInt
        wrap "DATE" tryParseAsDate
        wrap "FLOAT" tryParseAsFloat
        wrap "STR" tryParseAsString
    ]

and tryGetGroundParser s =
    groundParserFuns
    |> Seq.tryPick (fun f -> f s |> Option.map (fun _ -> f))

and tryParseGround s =
    groundParserFuns
    |> Seq.tryPick (fun f -> f s)

and groundExtParserFuns =
    [
        wrap "TRIP" tryParseAsTriplet
        wrap "PAIR" tryParseAsPair
        wrap "BOOL" tryParseAsBool
        wrap "INT" tryParseAsInt
        wrap "DATE" tryParseAsDate
        wrap "FLOAT" tryParseAsFloat
        wrap "STR" tryParseAsString
    ]

and tryGetGroundExtParser s =
    groundExtParserFuns
    |> Seq.tryPick (fun f -> f s |> Option.map (fun _ -> f))

and tryParseGroundExt s =
    groundExtParserFuns
    |> Seq.tryPick (fun f -> f s)

and curlyParserFuns =
    [
        wrap "SET" tryParseAsSet
        wrap "MAP" tryParseAsMapping
        wrap "COMP" tryParseAsComposite
    ]

and tryGetCurlyParser s =
    curlyParserFuns
    |> Seq.tryPick (fun f -> f s |> Option.map (fun _ -> f))

and tryParseCurly s =
    curlyParserFuns
    |> Seq.tryPick (fun f -> f s)

and allParserFuns =
    [
        wrap "TRIP" tryParseAsTriplet
        wrap "PAIR" tryParseAsPair
        wrap "STR" tryParseAsString
        wrap "DATE" tryParseAsDate
        wrap "BOOL" tryParseAsBool
        wrap "INT" tryParseAsInt
        wrap "FLOAT" tryParseAsFloat
        wrap "VEC" tryParseAsVector
        wrap "SET" tryParseAsSet
        wrap "MAP" tryParseAsMapping
        wrap "COMP" tryParseAsComposite
    ]

and tryGetParser s =
    allParserFuns
    |> Seq.tryPick (fun f -> f s |> Option.map (fun _ -> f))
 
and tryParse s =
    allParserFuns
    |> Seq.tryPick (fun f -> f s)

let rec parseGroup types s =
    match s with
    | ReId("Group", ReLit "{" s) ->
        let rec work types s =
            match s with
            | ReLit "}" s ->
                types, s
            | ReId("Name", ReLit "=" (ReString(_, ReLit ";" s)))
            | ReId("Index", ReLit "=" (ReInt(_, ReLit ";" s)))
            | ReId("Desc", ReLit "=" (ReString(_, ReLit ";" s))) ->
                work types s
            | ReId("Group", _) ->
                let types, s = parseGroup types s
                work types s
            | ReId(n, ((ReLit "{" _) as s)) ->
                match tryParseAsComposite s with
                | Some (kind, s) ->
                    let types = unifyMap n kind types
                    work types s
                | None ->
                    parseError("Expected group or composite", s)
            | s ->
                parseError("In Group, unexpected LHS", s)
        work types s
    | s ->
        parseError("Expected 'Group'", s)

let rec getTopType types s =
    match s with
    | EOF s ->
        types, s
    | ReId("Group", _) ->
        parseGroup types s
    | ReId(n, s) ->
        match Map.tryFind n types with
        | Some kind ->
            let (ParserFun f) = makeParser kind
            try
                let x, s = f s
                types, s
            with
            | _ ->
                match tryParse s with
                | Some (kind2, s) ->
                    unifyMap n kind2 types, s
                | None ->
                    parseError("Failed to extend seen structure", s)
        | None ->
            match (wrap "COMP" tryParseAsComposite) s with
            | Some (kind, s) ->
                Map.add n kind types, s
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
            let types, s =
                try
                    getTopType types s
                with
                | :? ParseError as e ->
                    printParseError e
                    |> String.concat "\n"
                    |> printfn "%s"
                    raise e
                | :? UnificationFailure as e ->
                    printfn "Unification failure: %s" e.Data0
                    raise e
                | e ->
                    printfn "FAILED: %s" e.Message
                    raise e
            work types s

    work Map.empty s
