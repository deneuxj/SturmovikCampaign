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
    | _ -> 9

let getPoints scaleCoords (path : string) =
    seq {
        let files =
            if Directory.Exists(path) then
                Directory.EnumerateFiles(path, "*.csv")
            else
                Seq.singleton path
        for path in files do
            use file = File.OpenText(path)
            while not file.EndOfStream do
                let line = file.ReadLine()
                let components = line.Split ','
                match components with
                | [| cx; cy |] -> yield scaleCoords(parseNum cx, parseNum cy)
                | [||] -> ()
                | _ -> failwithf "Ill-formed line '%s'" line
    }

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
    QuadTree.fromBoundaryOjects getBoundary maxDepth 0 false points

// Go into the leaves and wipe out the indvidual points. Replace them by the bounding box of the leaf.
let wipeContent (tree : QuadTree<Vector2>) =
    let rec work (node : QuadNode<Vector2>) =
        if node.Children.Length = 0 then
            {
                Min = node.Min
                Max = node.Max
                Children = [||]
                Content = [| [ node.Min; Vector2(node.Max.X, node.Min.Y); node.Max; Vector2(node.Min.X, node.Max.Y) ] |]
                ContentLen = 1
            }
        else
            {
                Min = node.Min
                Max = node.Max
                Children = node.Children |> Array.map work
                Content = [||]
                ContentLen = node.ContentLen
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
        use resultFile = File.Create(mapName + ".bin")
        let watch = Stopwatch.StartNew()
        let tree =
            paths
            |> Seq.collect (getPoints (mkScaleCoords mapName))
            |> mkQuadTree (getTreeDepth mapName)
            |> wipeContent
        printfn "Top node bounds: %A %A" tree.Root.Min tree.Root.Max
        let time = watch.ElapsedMilliseconds
        printfn "Computation took %f s" ((float time) / 1000.0)
        let watch = Stopwatch.StartNew()
        let serializer = FsPickler.CreateBinarySerializer()
        serializer.Serialize(resultFile, (tree.Root, tree.MaxDepth, tree.MinItems, tree.ContentInInnerNodes))
        let time = watch.ElapsedMilliseconds
        printfn "Serialization took %f s" ((float time) / 1000.0)
        0
    | _ ->
        printfn "Missing map name and/or point lists"
        1
