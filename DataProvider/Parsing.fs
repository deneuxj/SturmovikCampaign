﻿module SturmovikMission.DataProvider.Parsing

open System.Text.RegularExpressions

open SturmovikMission.DataProvider.Ast

let reInt = Regex(@"\G\s*([+-]?\d+)")
let reId = Regex(@"\G\s*([a-zA-Z0-9]+)")
let reString = Regex("\G\\s*\"([^\"]*)\"")
let reFloat = Regex(@"\G\s*([+-]?\d+[.]\d+)")
let reTime = Regex(@"\G\s*(\d+):(\d+):(\d+)")
let reDate = Regex(@"\G\s*(\d+)\.(\d+)\.(\d+)")

type Stream = SubString of Data : string * Offset : int
with
    static member FromFile(path) =
        let lines = System.IO.File.ReadAllLines(path)
        let lines =
            lines
            |> Array.map (fun line ->
                if line.StartsWith("#") then
                    ""
                else
                    line.TrimEnd())
        let data = String.concat "\n" lines
        SubString(data, 0)

let (|ReInt|_|) (SubString(data, offset)) =
    let m = reInt.Match(data, offset)
    if m.Success then
        Some (System.Int32.Parse m.Value, SubString(data, m.Index + m.Length))
    else
        None

let (|ReId|_|) (SubString(data, offset)) =
    let m = reId.Match(data, offset)
    if m.Success then
        Some (m.Groups.[1].Value, SubString(data, m.Index + m.Length))
    else
        None

let (|ReString|_|) (SubString(data, offset)) =
    let m = reString.Match(data, offset)
    if m.Success then
        Some (m.Groups.[1].Value, SubString(data, m.Index + m.Length))
    else
        None

let (|ReLit|_|) lit (SubString(data, offset)) =
    let r = Regex(@"\G\s*")
    let m = r.Match(data, offset)
    if data.[m.Index + m.Length..].StartsWith(lit) then
        Some (SubString(data, m.Index + m.Length + lit.Length))
    else
        None

let (|ReFloat|_|) (SubString(data, offset)) =
    let m = reFloat.Match(data, offset)
    if m.Success then
        try
            Some (System.Double.Parse m.Value, SubString(data, m.Index + m.Length))
        with
        | :? System.FormatException ->
            failwithf "Not a float: %s" m.Value
    else
        None

let (|ReTime|_|) (SubString(data, offset)) =
    let m = reTime.Match(data, offset)
    if m.Success then
        let hour = System.Int32.Parse m.Groups.[1].Value
        let minute = System.Int32.Parse m.Groups.[2].Value
        let second = System.Int32.Parse m.Groups.[3].Value
        Some (Value.Time(hour, minute, second), SubString(data, m.Index + m.Length))
    else
        None

let (|ReDate|_|) (SubString(data, offset)) =
    let m = reDate.Match(data, offset)
    if m.Success then
        let day = System.Int32.Parse m.Groups.[1].Value
        let month = System.Int32.Parse m.Groups.[2].Value
        let year = System.Int32.Parse m.Groups.[3].Value
        Some (Value.Date(day, month, year), SubString(data, m.Index + m.Length))
    else
        None

type ParserFun =
    ParserFun of (Stream -> Value * Stream)

exception ParseError of string * Stream

let parseError (txt, s) =
    raise(ParseError(txt, s))

let printParseError (e : ParseError) =
    let msg, (SubString(txt, offset)) = e.Data0, e.Data1
    let prefix = txt.[0..offset]
    let lines = prefix.Split('\n')
    let lineno = lines.Length
    [
        sprintf "Parse error: %s" msg
        sprintf "(%d): '%s...'" lineno (lines.[lines.Length - 1]) 
    ]

let rec makeParser (format : ValueType) : ParserFun =
    match format with
    | ValueType.Integer ->
        function
        | ReInt (x, s) -> (Value.Integer x, s)
        | s -> parseError("Not an integer", s)
    | ValueType.String ->
        function
        | ReString (x, s) -> (Value.String x, s)
        | s -> parseError("Not a string", s)
    | ValueType.Float ->
        function
        | ReFloat (x, s) -> (Value.Float x, s)
        | s -> parseError("Not a float", s)
    | ValueType.Composite content ->
        let typeMap = Map.ofList content
        let rec parse (s : Stream) =
            match s with
            | ReId(kw, (ReLit "{" _ as s))
            | ReId(kw, ReLit "=" s) ->
                match typeMap.TryFind kw with
                | Some valueType ->
                    let (ParserFun f) = makeParser valueType
                    let (v, s) = f s
                    match s with
                    | ReLit ";" s
                    | s ->
                        let xs, s = parse s 
                        (kw, v) :: xs, s
                | None ->
                    parseError(sprintf "Not a known key: %s" kw, s)
            | ReLit "}" s ->
                [], s
            | _ ->
                parseError("Not } or id = data", s)
        function
        | ReLit "{" s ->
            let vs, s = parse s
            (Composite vs, s)
        | s -> parseError("Not {", s)
    | ValueType.Mapping itemType ->
        let rec parse (s : Stream) =
            match s with
            | ReId(kw, ReLit "=" s) ->
                let (ParserFun f) = makeParser itemType
                let (v, s) = f s
                match s with
                | ReLit ";" s
                | s ->
                    let xs, s = parse s 
                    (kw, v) :: xs, s
            | ReLit "}" s ->
                [], s
            | _ ->
                parseError("Not } or id = data", s)
        function
        | ReLit "{" s ->
            let vs, s = parse s
            (Mapping vs, s)
        | s -> parseError("Not {", s)
    | ValueType.Pair(itemType1, itemType2) ->
        let (ParserFun f1) = makeParser itemType1
        let (ParserFun f2) = makeParser itemType2
        fun (s : Stream) ->
            let x1, s = f1 s
            match s with
            | ReLit ":" s ->
                let x2, s = f2 s
                (Value.Pair(x1, x2), s)
            | _ ->
                parseError("Not :", s)
    | ValueType.Triplet(itemType1, itemType2, itemType3) ->
        let (ParserFun f1) = makeParser itemType1
        let (ParserFun f2) = makeParser itemType2
        let (ParserFun f3) = makeParser itemType3
        fun (s : Stream) ->
            let x1, s = f1 s
            match s with
            | ReLit ":" s ->
                let x2, s = f2 s
                match s with
                | ReLit ":" s ->
                    let x3, s = f3 s
                    (Value.Triplet(x1, x2, x3), s)
                | _ ->
                    parseError("Not :", s)
            | _ ->
                parseError("Not :", s)
    | ValueType.Time ->
        function
        | ReTime(t, s) -> (t, s)
        | _ -> failwith "Not a time"
    | ValueType.Date ->
        function
        | ReDate(d, s) -> (d, s)
        | _ -> failwith "Not a date"
    | ValueType.IntVector ->
        function
        | ReLit "[" s ->
            let rec parse =
                function
                | ReInt(n, s) ->
                    let xs, s = parse s
                    n :: xs, s
                | ReLit "]" s ->
                    [], s
                | s ->
                    parseError("Not an int or ]", s)
            let xs, s = parse s
            Value.IntVector xs, s
        | s ->
            parseError("Not [", s)
    | ValueType.Set itemType ->
        let (ParserFun f) = makeParser itemType
        function
        | ReLit "{" s ->
            let rec parse =
                function
                | ReLit "}" s ->
                    [], s
                | s ->
                    let x, s = f s
                    match s with
                    | ReLit ";" s
                    | s ->
                        let xs, s = parse s
                        x :: xs, s
            let xs, s = parse s
            (Set xs, s)
        | s ->
            parseError("Not {", s)
    |> ParserFun
