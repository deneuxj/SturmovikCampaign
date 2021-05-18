// Learn more about F# at http://fsharp.org

open System
open System.Numerics
open System.IO
open Campaign.SpacePartition
open System.Diagnostics
open MBrace.FsPickler
open System.IO.Compression

let parseNum x = Single.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

let mkScaleCoords mapName =
    // lower corner and size, using the game's coordinates (X -> north, Y -> east).
    // Actually, in the game it's Z that goes east, but we skip elevation Y.
    let ll, ext =
        match mapName with
        | "kuban" -> let o = Vector2(35000.0f, 35000.0f) in o, Vector2(323148.0f, 450925.0f) - o
        | "moscow" -> Vector2.Zero, Vector2(281600.0f, 281600.0f)
        | "rheinland" -> let o = Vector2(30000.0f, 30000.0f) in o, Vector2(354042.73f, 430842.86f) - o
        | "stalingrad" -> Vector2.Zero, Vector2(230400.0f, 358400.0f)
        | _ -> failwithf "Unsupported map '%s'" mapName
    fun (x, y) ->
        ll + Vector2(y * ext.X, x * ext.Y)

// The max depth of the tree. Tweaked to produce files of reasonable size
let getTreeDepth =
    function
    | "stalingrad" -> 10
    | "moscow" -> 10
    | _ -> 9

let getPoints scaleCoords (path : string) =
    let files =
        if Directory.Exists(path) then
            Directory.EnumerateFiles(path, "*.csv")
        else
            Seq.singleton path
    seq {
        for path in files do
            yield
                File.ReadAllLines(path)
                |> Array.choose (fun line ->
                    let components = line.Split ','
                    match components with
                    | [| cx; cy |] ->
                        Some(scaleCoords(parseNum cx, parseNum cy))
                    | [||] ->
                        None
                    | _ ->
                        printfn "Ill-formed line '%s'" line
                        None)
    }
    |> Seq.cache

let mkQuadTree maxDepth (points : Vector2 seq) =
    let getBoundary (v : Vector2) =
        let s = 50.0f
        let x0, x1 = v.X - s, v.X + s
        let y0, y1 = v.Y - s, v.Y + s
        [ Vector2(x0, y0)
          Vector2(x1, y0)
          Vector2(x1, y1)
          Vector2(x0, y1)
        ]
    points
    |> QuadTree.fromBoundaryOjects getBoundary maxDepth 0 false

// Go into the leaves and wipe out the indvidual points. Replace them by the bounding box of the leaf.
let wipeContent (tree : QuadTree<Vector2>) =
    let rec work (node : QuadNode<Vector2>) =
        if node.Children.Length = 0 then
            {
                Min = node.Min
                Max = node.Max
                Children = [||]
                Content = if node.Content.Length > 0 then [| [ node.Min; Vector2(node.Max.X, node.Min.Y); node.Max; Vector2(node.Min.X, node.Max.Y) ] |] else [||]
                ContentLen = if node.Content.Length > 0 then 1 else 0
            }
        else
            let children = node.Children |> Array.map work
            {
                Min = node.Min
                Max = node.Max
                Children = children
                Content = [||]
                ContentLen = children |> Array.sumBy (fun child -> child.ContentLen)
            }
    {   MaxDepth = tree.MaxDepth
        MinItems = tree.MinItems
        Root = work tree.Root
        ContentInInnerNodes = false
        Intersects = Functions.intersectWithBoundingBox id
    }

[<EntryPoint>]
let main argv =
    match argv |> List.ofArray with
    | mapName :: paths ->
        let watch = Stopwatch.StartNew()
        let tree =
            paths
            |> Seq.collect (getPoints (mkScaleCoords mapName))
            |> Seq.concat
            |> mkQuadTree (getTreeDepth mapName)
            |> wipeContent
        printfn "Top node bounds: %A %A" tree.Root.Min tree.Root.Max
        let time = watch.ElapsedMilliseconds
        printfn "Computation took %f s" ((float time) / 1000.0)
        let watch = Stopwatch.StartNew()
        let serializer = FsPickler.CreateBinarySerializer()
        use resultFile = File.Create(mapName + ".qtree.gz")
        use zStream = new GZipStream(resultFile, CompressionLevel.Optimal)
        serializer.Serialize(zStream, (tree.Root, tree.MaxDepth, tree.MinItems, tree.ContentInInnerNodes))
        let time = watch.ElapsedMilliseconds
        printfn "Serialization took %f s" ((float time) / 1000.0)
        0
    | _ ->
        printfn "Missing map name and/or point lists"
        1
