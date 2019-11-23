open System.IO
open Campaign.NewWorldDescription
open Campaign.BasicTypes
open System.Reflection

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv =
    let missionFile =
        match argv with
        | [| x |] ->
            if File.Exists x then
                x
            else
                failwithf "Can't open %s" x
        | _ -> failwith "Usage: WorldCheck <mission file>"

    let world =
        try
            Loading.loadWorld(missionFile, 10000.0f<E/H>, 100.0f<E/H>, 100.0f<E/H>, 1000.0f<E/H>)
        with
        | e ->
            eprintfn "Error in mission file: %s" e.Message
            System.Console.ReadKey(true) |> ignore
            failwith "Failed"

    printfn "Number of regions: %d" world.Regions.Length
    printfn "Number of airfields: %d" world.Airfields.Length
    printfn "Number of road bridges: %d" (world.Roads.Links |> List.sumBy (fun link -> link.Bridges.Length))
    printfn "Number of railroad bridges: %d" (world.Rails.Links |> List.sumBy (fun link -> link.Bridges.Length))
    printfn "Number of regions with terminals: %d" ([world.Roads.Nodes ; world.Rails.Nodes] |> Seq.concat |> Seq.filter (fun node -> node.HasTerminal) |> Seq.distinctBy (fun node -> node.Region) |> Seq.length)
    System.Console.WriteLine("Press a key to exit")
    System.Console.ReadKey(true) |> ignore
    0 // return an integer exit code
