// Learn more about F# at http://fsharp.org

open System
open GroundAttack
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Parsing

[<EntryPoint>]
let main argv =
    let mutable status = 0
    for file in argv do
        let filename = IO.Path.GetFileNameWithoutExtension(file).ToLowerInvariant()
        if filename.StartsWith("skel-groundattack-") then
            let skelGroup =
                try
                    printfn "Parsing skeleton ground attack file %s" file
                    T.GroupData.Parse(Stream.FromFile file)
                    |> Ok
                with
                | :? ParseError as e ->
                    Error ("Error while parsing skeleton: " + String.concat "\n" (printParseError e))

            let config =
                skelGroup
                |> Result.bind (fun skelGroup ->
                    try
                        printfn "Extracting template parameters"
                        mkConfigFromGroup skelGroup
                        |> Ok
                    with e -> Error ("Error while extracting template parameters: " + e.Message))

            let store = SturmovikMission.DataProvider.NumericalIdentifiers.IdStore()
            let group =
                config
                |> Result.bind(fun (config, plane) ->
                    try
                        printfn "Instantiating template"
                        let group = SturmovikMission.Blocks.GroundAttack.AttackerGroup.Create(store, config)
                        group.SetPlanes(plane)
                        Ok group
                    with
                    | e -> Error ("Error during instantiation: " + e.Message))
            match group with
            | Ok group ->
                let outFile =
                    IO.Path.Combine(
                        IO.Path.GetDirectoryName(file),
                        "inst" + IO.Path.GetFileNameWithoutExtension(file).Substring("skel".Length) + ".group"
                    )
                try
                    printfn "Writing result to %s" outFile
                    SturmovikMission.DataProvider.McuOutput.writeGroupFile outFile (McuUtil.deepContentOf group.All)
                with
                | e ->
                    printfn "Error while writing result to %s: %s" outFile e.Message
                    status <- 1
            | Error m ->
                printfn "%s" m
                status <- 1

    status // return an integer exit code
