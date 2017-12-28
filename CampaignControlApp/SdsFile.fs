module SdsFile

open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Ast

let parseKeyStringValue s =
    match s with
    | ReId(key, ReLit "=" (ReString(value, EOF _))) ->
        Some(key, value)
    | _ ->
        None

let parseKeyIntValue s =
    match s with
    | ReId(key, ReLit "=" (ReInt(value, EOF _))) ->
        Some(key, value)
    | _ ->
        None

let parseSds sdsFile =
    System.IO.File.ReadAllLines sdsFile
    |> Seq.choose (fun line ->
        let s = Stream.FromString line
        match parseKeyStringValue s with
        | Some (k, v) -> Some(k, Value.String v)
        | None ->
            match parseKeyIntValue s with
            | Some (k, v) -> Some(k, Value.Integer v)
            | None -> None)
    |> Map.ofSeq
