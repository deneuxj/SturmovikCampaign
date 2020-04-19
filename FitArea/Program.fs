// Learn more about F# at http://fsharp.org

open Campaign.SpacePartition

open System
open System.Numerics

let (|BinFilePath|_|) =
    function
    | path :: rest -> Some(path, rest)
    | [] -> None

let (|Num|_|) (arg : string) =
    match Single.TryParse(arg, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with
    | true, x -> Some x
    | _ -> None

let (|Coords|_|) args =
    let rec work args res =
        match args with
        | (Num x) :: (Num y) :: rest ->
            work rest (Vector2(x, y) :: res)
        | _ ->
            Some(res |> List.rev, args)
    work args []

let (|Integer|_|) (arg : string) =
    match Int32.TryParse(arg, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with
    | true, x -> Some x
    | _ -> None

let (|Seed|_|) =
    function
    | "-s" :: (Integer seed) :: [] -> Some seed
    | [] -> Some 0
    | _ -> None

[<EntryPoint>]
let main argv =
    match argv |> List.ofSeq with
    | BinFilePath(path, "-o" :: Coords(shape, "-r" :: Coords(region, Seed seed))) ->
        try
            use freeAreasFile =
                try
                    IO.File.OpenRead(path)
                with _ -> failwithf "Could not open free areas data file '%s'" path
            let serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
            let freeAreas : FreeAreas.FreeAreasNode option =
                try
                    serializer.Deserialize(freeAreasFile)
                with e -> failwithf "Failed to read free areas data file, error was: %s" e.Message
            match shape, region, freeAreas with
            | _ :: _, _ :: _, Some root ->
                let random = System.Random(seed)
                let rank _ = 
                    random.Next()
                let candidates =
                    FreeAreas.findPositionCandidates rank root shape region
                    |> Seq.cache
                if Seq.isEmpty candidates then
                    failwith "Failed to find a fit"
                candidates
                |> Seq.truncate 10
                |> Seq.iter (fun offset -> printfn "%s" (string offset))
            | [], _, _ ->
                failwith "Missing shape outline"
            | _, [], _ ->
                failwith "Missing region outline"
            | _, _, None ->
                failwith "No free space"
            0
        with e ->
            eprintfn "Error: %s" e.Message
            1
    | _ ->
        eprintfn "Invalid commandline."
        eprintfn "Usage: FitArea <free area bin file> -o <shape outline> -r <constraint region outline> -s seed"
        eprintfn "Example: FitArea rheinland.bin -o 100.0 50.0 150.0 50.0 100.0 75.0 -r 0.0 0.0 3.0e6 0.0 3.0e6 3.0e6 3.0e6 0.0 -s 1234"
        1
