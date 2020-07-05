﻿// Learn more about F# at http://fsharp.org

open Campaign.SpacePartition

open System
open System.Numerics

let (|BinFilePath|_|) =
    function
    | path :: rest ->
        Some(path, rest)
    | [] -> None

let (|Num|_|) (arg : string) =
    match Single.TryParse(arg, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with
    | true, x -> Some x
    | _ -> None

let (|Poly|_|) args =
    let rec work count args res =
        match args with
        | (Num x) :: (Num y) :: rest ->
            work (count + 1) rest (Vector2(x, y) :: res)
        | _ ->
            if count >= 3 then
                Some(res |> List.rev, args)
            else
                None
    work 0 args []

let (|Square|_|) args =
    match args with
    | (Num x) :: (Num y) :: (Num side) :: rest ->
        let corner = Vector2(x, y)
        let dx = Vector2.UnitX * side
        let dy = Vector2.UnitY * side
        Some ([corner; corner + dy; corner + dx + dy; corner + dx], rest)
    | _ -> None

let (|Integer|_|) (arg : string) =
    match Int32.TryParse(arg, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) with
    | true, x -> Some x
    | _ -> None

let (|Random|_|) =
    function
    | "-s" :: (Integer seed) :: rest -> Some(System.Random(seed), rest)
    | rest -> Some(System.Random(), rest)

let (|Candidates|_|) =
    function
    | "-n" :: (Integer n) :: rest -> Some(n, rest)
    | rest -> Some(10, rest)

let (|Rectangle|_|) args =
    match args with
    | Num(x0) :: Num(y0) :: Num(dx) :: Num(dy) :: rest ->
        Some((Vector2(x0, y0), dx, dy), rest)
    | _ ->
        None

let (|WithDebugGroup|_|) =
    function
    | "-d" :: path :: rest -> Some(path, rest)
    | _ -> None

module Debug =
    open SturmovikMission.Blocks.BlocksMissionData

    let mkDebugGroup (path : string, region : Vector2 list, shape : Vector2 list, node : FreeAreas.FreeAreasNode, candidates : Vector2 seq) =
        let nodesInRegion = FreeAreas.filterLeaves (FreeAreas.intersectsWithRegion region) node
        let shapeCenter =
            let x = shape |> Seq.sum
            x / (float32 shape.Length)
        let areas =
            let toFloatPair (v : Vector2) =
                T.FloatPair.N(float v.X, float v.Y)
            [
                for node in nodesInRegion do
                    let center =
                        Vector2(0.5f * (node.Min.X + node.Max.X), 0.5f * (node.Min.Y + node.Max.Y))
                    yield
                        center, [
                            Vector2(node.Min.X, node.Min.Y)
                            Vector2(node.Min.X, node.Max.Y)
                            Vector2(node.Max.X, node.Max.Y)
                            Vector2(node.Max.X, node.Min.Y)
                        ]
                for candidate in candidates do
                    let offset = candidate - shapeCenter
                    yield candidate, shape |> List.map ((+) offset)
            ]
            |> List.mapi (fun i (center, vertices) ->
                T.MCU_TR_InfluenceArea.Default
                    .SetIndex(T.Integer.N(i + 1))
                    .SetXPos(T.Float.N(float center.X))
                    .SetZPos(T.Float.N(float center.Y))
                    .SetBoundary(T.MCU_TR_InfluenceArea.Boundary.FromList <| List.map toFloatPair vertices)
                    .AsString()
            )
        use debugFile = IO.File.CreateText(path)
        for area in areas do
            debugFile.Write(area)

[<EntryPoint>]
let main argv =
    match argv |> List.ofSeq with
    | Candidates(numCandidates, BinFilePath(path, "-o" :: Poly(shape, "-r" :: (Poly(region, Random (random, rest)) | Square(region, Random(random, rest)))))) ->
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
            match freeAreas with
            | Some root ->
                let rank _ = 
                    random.Next()
                let candidates =
                    FreeAreas.findPositionCandidates rank root shape region
                    |> Seq.truncate numCandidates
                    |> Seq.cache
                match rest with
                | WithDebugGroup(path, _) ->
                    Debug.mkDebugGroup(path, region, shape, root, candidates)
                    printfn "wrote debug group to %s" path
                | _ -> ()
                if Seq.isEmpty candidates then
                    failwith "Failed to find a fit"
                candidates
                |> Seq.iter (fun candidate -> printfn "%s" (string candidate))
            | None ->
                failwith "No free space"
            0
        with e ->
            eprintfn "Error: %s" e.Message
            1
    | _ ->
        eprintfn "Invalid commandline."
        eprintfn "Usage: FitArea [-n <num candidates] <free area bin file> -o <shape outline> -r <constraint region outline> [-s <seed>] [-d <editor group file>]"
        eprintfn "Example: FitArea -n 10 rheinland.bin -o 100.0 50.0 150.0 50.0 100.0 75.0 -r 0.0 0.0 1.0e4 -s 1234"
        eprintfn " Tries to fit a triangle with vertices (100, 50), (150, 50), (100, 75) into the square that is 10000m wide and its south-west corner in (0, 0), using the map data from rheinland.bin."
        eprintfn " The coordinates are specified using the x and z components, using the mission editor's system (x goes north, z goes east)."
        eprintfn " Outputs 10 candidates, printing the position of the centers of the shape."
        1
