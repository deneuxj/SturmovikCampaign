// Learn more about F# at http://fsharp.org

open System
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.Blocks.McuInstantiation

let iconAttrib = "Icons made by Smashicons from www.flaticon.com"
let urls = [ "https://www.flaticon.com/authors/smashicons"
             "https://www.flaticon.com/" ]



[<EntryPoint>]
let main argv =
    printfn "%s" iconAttrib
    for url in urls do
        printfn "%s" url

    let mutable status = 0

    let processSkeleton mkConfigFromGroup mkGroup getNodes label file =
        let skelGroup =
            try
                printfn "Parsing skeleton %s file %s" label file
                T.GroupData.Parse(Stream.FromFile file)
                |> Ok
            with
            | :? ParseError as e ->
                Error ("Error while parsing skeleton: " + String.concat "\n" (printParseError e))
    
        let configAndPlane =
            skelGroup
            |> Result.bind (fun skelGroup ->
                try
                    printfn "Extracting template parameters"
                    mkConfigFromGroup skelGroup
                    |> Ok
                with e -> Error ("Error while extracting template parameters: " + e.Message))
    
        let store = SturmovikMission.DataProvider.NumericalIdentifiers.IdStore()
        let group =
            configAndPlane
            |> Result.bind(fun x ->
                try
                    printfn "Instantiating template"
                    let group = mkGroup(store, x)
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
                SturmovikMission.DataProvider.McuOutput.writeGroupFile outFile (getNodes group)
            with
            | e ->
                printfn "Error while writing result to %s: %s" outFile e.Message
                status <- 1
        | Error m ->
            printfn "%s" m
            status <- 1

    for file in argv do
        let filename = IO.Path.GetFileNameWithoutExtension(file).ToLowerInvariant()

        if filename.StartsWith("skel-groundattack-") then
            let mkConfigFromGroup = GroundAttack.mkConfigFromGroup
            let mkGroup(store, configAndPlane) =
                let config, plane = configAndPlane
                let group = SturmovikMission.Blocks.GroundAttack.AttackerGroup.Create(store, config)
                setVehiclesAfterPlane plane group
                group.All.PushGroupName(store, "Instantiated Ground Attack")
                group
            let getNodes(group : SturmovikMission.Blocks.GroundAttack.AttackerGroup) =
                group.All
                |> McuUtil.deepContentOf
            processSkeleton mkConfigFromGroup mkGroup getNodes "ground attack" file

        if filename.StartsWith("skel-patrol-") then
            let mkConfigFromGroup = Patrol.mkConfigFromGroup
            let mkGroup(store, configAndPlane) =
                let config, plane = configAndPlane
                let group = SturmovikMission.Blocks.Patrol.PatrolGroup.Create(store, config)
                setVehiclesAfterPlane plane group
                group.All.PushGroupName(store, "Instantiated Patrol")
                group
            let getNodes(group : SturmovikMission.Blocks.Patrol.PatrolGroup) =
                group.All
                |> McuUtil.deepContentOf
            processSkeleton mkConfigFromGroup mkGroup getNodes "patrol" file

        if filename.StartsWith("skel-escort-") then
            let mkConfigFromGroup = Escort.mkConfigFromGroup
            let mkGroup(store, configAndPlane) =
                let config, plane = configAndPlane
                let group = SturmovikMission.Blocks.Escort.EscortGroup.Create(store, config)
                setVehiclesAfterPlane plane group
                group.All.PushGroupName(store, "Instantiated Escort")
                group
            let getNodes(group : SturmovikMission.Blocks.Escort.EscortGroup) =
                group.All
                |> McuUtil.deepContentOf
            processSkeleton mkConfigFromGroup mkGroup getNodes "patrol" file

    status // return an integer exit code
