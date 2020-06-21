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

    let processSkeleton mkConfigFromGroup mkGroups label file =
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
        
        match configAndPlane with
        | Ok(_, _, instructions) ->
            for instruction in instructions do
                printfn "%s" instruction
        | _ ->
            ()

        let store = SturmovikMission.DataProvider.NumericalIdentifiers.IdStore()
        let groups =
            configAndPlane
            |> Result.bind(fun x ->
                try
                    printfn "Instantiating template"
                    let groups = mkGroups(store, x)
                    Ok groups
                with
                | e -> Error ("Error during instantiation: " + e.Message))
        match groups with
        | Ok groups ->
            let outFile =
                IO.Path.Combine(
                    IO.Path.GetDirectoryName(file),
                    "inst" + IO.Path.GetFileNameWithoutExtension(file).Substring("skel".Length) + ".group"
                )
            try
                printfn "Writing result to %s" outFile
                let nodes =
                    groups
                    |> List.collect (McuUtil.deepContentOf)
                printfn "%d nodes" nodes.Length
                //for node in nodes do
                //    printfn "%s" (node.AsString())
                SturmovikMission.DataProvider.McuOutput.writeGroupFile outFile nodes
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
            let mkConfigFromGroup = GroundAttack.mkConfigFromGroup false
            let mkGroup(store, configAndPlane) =
                let config, plane, _ = configAndPlane
                let group = SturmovikMission.Blocks.GroundAttack.AttackerGroup(store, config)
                setVehiclesAfterPlane plane group
                group.All.PushGroupName(store, "Instantiated Ground Attack")
                [group.All]
            processSkeleton mkConfigFromGroup mkGroup "ground attack" file

        if filename.StartsWith("skel-patrol-") then
            let mkConfigFromGroup = Patrol.mkConfigFromGroup
            let mkGroup(store, configAndPlane) =
                let config, plane, _ = configAndPlane
                let group = SturmovikMission.Blocks.Patrol.PatrolGroup(store, config)
                setVehiclesAfterPlane plane group
                group.All.PushGroupName(store, "Instantiated Patrol")
                [group.All]
            processSkeleton mkConfigFromGroup mkGroup "patrol" file

        if filename.StartsWith("skel-escort-") then
            let mkConfigFromGroup group =
                let (escortGroup, escortPlane, escortInstructions), (attackersGroup, attackersPlane, attackersInstructions) = 
                    Escort.mkFullConfigFromGroup group
                let instructions =
                    [
                        for line in escortInstructions do
                            yield "Escort: " + line
                        for line in attackersInstructions do
                            yield "Attackers: " + line
                    ]
                (escortGroup, attackersGroup), (escortPlane, attackersPlane), instructions
            
            let mkGroup(store, configsAndPlanes) =
                let (escortConfig, attackersConfig), (escortPlane, attackersPlane), _ = configsAndPlanes
                
                let escortGroup = SturmovikMission.Blocks.Escort.EscortGroup(store, escortConfig)
                setVehiclesAfterPlane escortPlane escortGroup
                escortGroup.All.PushGroupName(store, "Instantiated Escort")

                let attackersGroup = SturmovikMission.Blocks.GroundAttack.AttackerGroup(store, attackersConfig)
                setVehiclesAfterPlane attackersPlane attackersGroup
                attackersGroup.All.PushGroupName(store, "Instantiated Ground Attack")

                SturmovikMission.Blocks.Escort.connectEscortWithPlanes escortGroup attackersGroup

                [escortGroup.All; attackersGroup.All]
            
            processSkeleton mkConfigFromGroup mkGroup "patrol" file

    status // return an integer exit code
