module Util.StringPatterns

open System

let (|AsInt32|_|) (s : string) =
    match Int32.TryParse(s) with
    | true, n -> Some n
    | false, _ -> None

/// Active pattern checking if a string contains a given substring.
let (|Contains|_|) (substring : string) (s : string) =
    if s.Contains(substring) then
        Some()
    else
        None
