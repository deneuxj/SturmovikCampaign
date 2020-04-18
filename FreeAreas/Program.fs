// Learn more about F# at http://fsharp.org

open System
open System.Numerics
open System.IO
open Campaign.SpacePartition
open System.Diagnostics
open MBrace.FsPickler

let getPoints (path : string) =
    match File.ReadAllLines(path) |> List.ofArray with
    | xs :: ys :: _ ->
        let parseNum x = Single.Parse(x, System.Globalization.CultureInfo.InvariantCulture)
        let numbers (line : string) = line.Split "," |> Seq.map parseNum
        Seq.zip (numbers xs) (numbers ys)
        |> Seq.map (fun (x, y) -> Vector2(x, y))
    | _ -> failwith "Input file must have two lines"

let mkQuadTree (points : Vector2 seq) =
    QuadTree.fromBoundaryOjects (fun v -> [v]) 10 1 false points

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
