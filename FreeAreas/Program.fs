﻿// Learn more about F# at http://fsharp.org

open System
open System.Numerics
open System.IO
open Campaign.SpacePartition
open System.Diagnostics
open MBrace.FsPickler

let parseNum x = Single.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

let mkScaleCoords mapName =
    // lower corner and size, using the game's coordinates (X -> north, Y -> east).
    // Actually, in the game it's Z that goes east, but we skip elevation Y.
    // Uses the sizes from https://github.com/dpm314/il2_map_analysis/blob/master/workspace.py#L30 rather than the game's.
    let ll, ext =
        match mapName with
        | "kuban" -> let o = Vector2(35000.0f, 35000.0f) in o, Vector2(323148.0f, 450925.0f) - o
        | "moscow" -> Vector2.Zero, Vector2(281600.0f, 281600.0f)
        | "rheinland" -> let o = Vector2(30000.0f, 30000.0f) in o, Vector2(354042.73f, 430842.86f) - o
        | "stalingrad" -> Vector2.Zero, Vector2(230400.0f, 358400.0f)
        | _ -> failwithf "Unsupported map '%s'" mapName
    fun (x, y) ->
        ll + Vector2(y * ext.X, x * ext.Y)

let getPoints scaleCoords (path : string) =
    seq {
        use file = File.OpenText(path)
        while not file.EndOfStream do
            let line = file.ReadLine()
            let components = line.Split ','
            match components with
            | [| cx; cy |] -> yield scaleCoords(parseNum cx, parseNum cy)
            | [||] -> ()
            | _ -> failwithf "Ill-formed line '%s'" line
    }

let mkQuadTree (points : Vector2 seq) =
    QuadTree.fromBoundaryOjects (fun v -> [v]) 10 0 false points

[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | mapName :: paths ->
        use resultFile = File.Create("free-areas.bin")
        let watch = Stopwatch.StartNew()
        let tree =
            paths
            |> Seq.collect (getPoints (mkScaleCoords mapName))
            |> mkQuadTree
        printfn "Top node bounds: %A %A" tree.Root.Min tree.Root.Max
        let time = watch.ElapsedMilliseconds
        printfn "Computation took %f s" ((float time) / 1000.0)
        let watch = Stopwatch.StartNew()
        let free = FreeAreas.translate tree.Root
        let area, numNodes = free |> Option.map FreeAreas.sumArea |> Option.defaultValue (0.0f, 0)
        printfn "%3.0f km^2 free area in %d nodes" (area / 1.0e6f) numNodes
        printfn "Collecting..."
        GC.Collect()
        printfn "Done"
        let serializer = FsPickler.CreateBinarySerializer()
        serializer.Serialize(resultFile, free)
        let time = watch.ElapsedMilliseconds
        printfn "Translation and serialization took %f s" ((float time) / 1000.0)
        0
    | _ ->
        printfn "Missing map name and/or point lists"
        1
