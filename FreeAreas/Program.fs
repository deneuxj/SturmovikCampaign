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
    QuadTree.fromBoundaryOjects (fun v -> [v]) 10 1 points

type FreeAreasNode =
    {
        Min : Vector2
        Max : Vector2
        Children : FreeAreasNode[]
    }

let rec translate (quad : QuadNode<Vector2>) =
    if Array.isEmpty quad.Content then
        Some {
            Min = quad.Min
            Max = quad.Max
            Children = [||]
        }
    elif Array.isEmpty quad.Children then
        None
    else
        let subs = quad.Children |> Array.choose translate
        match subs with
        | [||] -> None
        | _ ->
            Some {
                Min = quad.Min
                Max = quad.Max
                Children = subs
            }

let rec sumArea (free : FreeAreasNode) =
    if Array.isEmpty free.Children then
        let area = (free.Max.X - free.Min.X) * (free.Max.Y - free.Min.Y)
        (area, 1)
    else
        free.Children
        |> Seq.map sumArea
        |> Seq.fold (fun (area, num) (acc0, acc1) -> (area + acc0, num + acc1)) (0.0f, 0)

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
        let free = translate tree.Root
        let area, numNodes = free |> Option.map sumArea |> Option.defaultValue (0.0f, 0)
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
