module Util.StringPatterns

open System

let (|AsInt32|_|) s =
    match Int32.TryParse(s) with
    | true, n -> Some n
    | false, _ -> None