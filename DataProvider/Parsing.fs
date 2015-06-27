module SturmovikMission.DataProvider.Parsing

open System
open System.Text.RegularExpressions

open SturmovikMission.DataProvider.Ast
open Microsoft.FSharp.Quotations

let regex pat =
    Regex(pat, RegexOptions.Compiled)

let reBool = regex(@"\G\s*(0|1)[^.\d]")
let reInt = regex(@"\G\s*([+-]?\d+)\s*[^.\d]")
let reId = regex(@"\G\s*([a-zA-Z0-9_]+)")
let reString = regex("\G\\s*\"([^\"]*)\"")
let reFloat = regex(@"\G\s*([+-]?\d+[.]\d+)")
let reDate = regex(@"\G\s*(\d+)\.(\d+)\.(\d+)")
let reWs = regex(@"\G\s*")

type Stream = SubString of Data : string * Offset : int
with
    static member FromFile(path) =
        let lines = IO.File.ReadAllLines(path)
        let lines =
            lines
            |> Array.map (fun line ->
                if line.StartsWith("#") then
                    ""
                else
                    line.TrimEnd())
        let data = String.concat "\n" lines
        SubString(data, 0)

    static member FromString(s) =
        SubString(s, 0)

let (|EOF|_|) (SubString(data, offset)) =
    let m = reWs.Match(data, offset)
    if m.Index + m.Length >= data.Length then
        Some(SubString(data, data.Length))
    else
        None

let (|ReBool|_|) (SubString(data, offset)) =
    let m = reBool.Match(data, offset)
    let g = m.Groups.[1]
    if m.Success then
        Some (g.Value = "1", SubString(data, g.Index + g.Length))
    else
        None

let (|ReInt|_|) (SubString(data, offset)) =
    let m = reInt.Match(data, offset)
    let g = m.Groups.[1]
    if m.Success then
        Some (Int32.Parse g.Value, SubString(data, g.Index + g.Length))
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

let (|ReLit|_|) (lit : string) (SubString(data, offset)) =
    let m = reWs.Match(data, offset)
    let i0 = m.Index + m.Length
    let rec matches i =
        if i >= lit.Length then
            true
        elif i0 + i >= data.Length then
            false
        else
            data.[i0 + i] = lit.[i] && matches (i + 1) 
    if matches 0 then
        Some (SubString(data, m.Index + m.Length + lit.Length))
    else
        None

let (|ReFloat|_|) (SubString(data, offset)) =
    let m = reFloat.Match(data, offset)
    if m.Success then
        try
            Some (
                Double.Parse(m.Value, Globalization.CultureInfo.InvariantCulture),
                SubString(data, m.Index + m.Length))
        with
        | :? FormatException ->
            failwithf "Not a float: %s" m.Value
    else
        None

let (|ReDate|_|) (SubString(data, offset)) =
    let m = reDate.Match(data, offset)
    if m.Success then
        let day = Int32.Parse m.Groups.[1].Value
        let month = Int32.Parse m.Groups.[2].Value
        let year = Int32.Parse m.Groups.[3].Value
        Some (Value.Date(day, month, year), SubString(data, m.Index + m.Length))
    else
        None

type ParserFun =
    ParserFun of (Stream -> Value * Stream)
with
    member this.Run(s) =
        let (ParserFun p) = this
        p s

exception ParseError of string * Stream

let parseError (txt, s) =
    raise(ParseError(txt, s))

let getContext (s : Stream) =
    let (SubString(txt, offset)) = s
    if txt.Length = 0 then
        "Empty string"
    else
        let start = max 0 (offset - 20)
        let fin = min (offset + 20) (txt.Length - 1)
        let ch =
            if offset >= 0 && offset < txt.Length then
                txt.[offset]
            else
                '!'
        sprintf "%c in %s" ch (txt.[start..fin].Replace("\r\n", "  ").Replace("\n", " "))

let printParseError (e : ParseError) =
    let msg, (SubString(txt, offset) as s) = e.Data0, e.Data1
    let prefix = txt.[0..offset]
    let lines = prefix.Split('\n')
    let lineno = lines.Length
    [
        sprintf "Parse error: %s" msg
        sprintf "(%d): '%s...'" lineno (getContext s)
    ]

let parseBool =
    function
    | ReBool (x, s) -> (Value.Boolean x, s)
    | s -> parseError("Not a Boolean", s)

let parseInteger =
    function
    | ReInt (x, s) -> (Value.Integer x, s)
    | s -> parseError("Not an integer", s)

let parseString =
    function
    | ReString (x, s) -> (Value.String x, s)
    | s -> parseError("Not a string", s)

let parseFloat =
    function
    | ReFloat (x, s) -> (Value.Float x, s)
    | ReInt (x, s) -> (Value.Float(float x), s)
    | s -> parseError("Not a float", s)

let parseDate =
    function
    | ReDate(d, s) -> (d, s)
    | _ -> failwith "Not a date"

let parseIntVector =
    function
    | ReLit "[" s ->
        let rec parse s =
            match s with
            | ReInt(n, ReLit "]" s) ->
                [n], s
            | ReLit "]" s ->
                [], s
            | ReInt(n, ReLit "," s) ->
                let xs, s = parse s
                n :: xs, s
            | _ ->
                parseError("Not an int or ]", s)
        let xs, s = parse s
        Value.IntVector xs, s
    | s ->
        parseError("Not [", s)

let rec parsePair (itemType1, itemType2) s =
    let (ParserFun f1) = makeParser itemType1
    let (ParserFun f2) = makeParser itemType2
    let x1, s = f1 s
    match s with
    | ReLit ":" s ->
        let x2, s = f2 s
        (Value.Pair(x1, x2), s)
    | _ ->
        parseError("Not :", s)

and makeParser (format : ValueType) : ParserFun =
    match format with
    | ValueType.Boolean -> parseBool
    | ValueType.Integer -> parseInteger
    | ValueType.String -> parseString
    | ValueType.Float -> parseFloat
    | ValueType.Composite content ->
        let typeMap = content
        let rec parse (s : Stream) =
            match s with
            | ReId(kw, ((ReLit "{" _) as s)) ->
                match typeMap.TryFind kw with
                | Some (valueType, _, _) ->
                    let (ParserFun f) = makeParser valueType
                    let (v, s) = f s
                    let xs, s = parse s 
                    (kw, v) :: xs, s
                | None ->
                    parseError(sprintf "Not a known key: %s" kw, s)
            | ReId(kw, ReLit "=" s) ->
                match typeMap.TryFind kw with
                | Some (valueType, _, _) ->
                    let (ParserFun f) = makeParser valueType
                    let (v, s) = f s
                    match s with
                    | ReLit ";" s ->
                        let xs, s = parse s 
                        (kw, v) :: xs, s
                    | _ ->
                        parseError("Not ;", s)
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
            | ReInt(n, ReLit "=" s) ->
                let (ParserFun f) = makeParser itemType
                let (v, s) = f s
                match s with
                | ReLit ";" s
                | s ->
                    let xs, s = parse s 
                    (n, v) :: xs, s
            | ReLit "}" s ->
                [], s
            | _ ->
                parseError("Not } or id = data", s)
        function
        | ReLit "{" s ->
            let vs, s = parse s
            (Mapping vs, s)
        | s -> parseError("Not {", s)
    | ValueType.Pair(itemType1, itemType2) -> parsePair (itemType1, itemType2)
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
    | ValueType.Date -> parseDate
    | ValueType.IntVector -> parseIntVector
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


let parseFile (getParser : string -> ParserFun) (s : Stream) =
    let defaultGroup = 
        { Name = ""; Index = 0; Description = ""; Data = [] }

    let rec parseGroup s data =
        match s with
        | ReLit "}" s ->
            data, s
        | ReId("Name", ReLit "=" (ReString(name, ReLit ";" s))) ->
            parseGroup s { data with Name = name }
        | ReId("Index", ReLit "=" (ReInt(index, ReLit ";" s))) ->
            parseGroup s { data with Index = index }
        | ReId("Desc", ReLit "=" (ReString(desc, ReLit ";" s))) ->
            parseGroup s { data with Description = desc }
        | ReId("Group", (ReLit "{" s)) ->
            let subData, s = parseGroup s defaultGroup
            parseGroup s { data with Data = (Group subData) :: data.Data }
        | ReId(name, ((ReLit "{" _) as s)) ->
            let parser = getParser name
            let subData, s = parser.Run(s)
            parseGroup s { data with Data = (Leaf(name, subData)) :: data.Data }
        | s ->
            parseError("In Group, unexpected LHS", s)

    let rec work (s : Stream) data =
        match s with
        | ReId("Group", (ReLit "{" s)) ->
            let group, s = parseGroup s defaultGroup
            work s (Group group :: data)
        | ReId(name, ((ReLit "{" _) as s)) ->
            let parser =
                try
                    getParser name
                with
                | e -> failwithf "Failed to get parser for %s. Error was '%s'" name e.Message
            let subData, s = parser.Run(s)
            work s (Leaf(name, subData) :: data)
        | EOF _ ->
            data
        | s ->
            parseError("Expected keyword", s)

    work s []
