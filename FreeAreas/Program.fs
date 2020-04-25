// Learn more about F# at http://fsharp.org

open System
open System.Numerics
open System.IO
open Campaign.SpacePartition
open System.Diagnostics
open MBrace.FsPickler

let parseNum x = Single.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

let getPoints (path : string) =
    seq {
        use file = File.OpenText(path)
        while not file.EndOfStream do
            let line = file.ReadLine()
            let components = line.Split ','
            match components with
            | [| cx; cy |] -> yield Vector2(parseNum cx, parseNum cy)
            | [||] -> ()
            | _ -> failwithf "Ill-formed line '%s'" line
    }

let mkQuadTree (points : Vector2 seq) =
    QuadTree.fromBoundaryOjects (fun v -> [v]) 10 0 false points

[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | (path :: _) as paths ->
        use resultFile = File.Create("free-areas.bin")
        let watch = Stopwatch.StartNew()
        let tree =
            paths
            |> Seq.collect getPoints
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
        printfn "Missing path to obstacle list file"
        1
