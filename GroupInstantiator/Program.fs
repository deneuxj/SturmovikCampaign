// Learn more about F# at http://fsharp.org

open System
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.Blocks.McuInstantiation

let iconAttrib = "Icons made by Smashicons from www.flaticon.com"
let urls = [ "https://www.flaticon.com/authors/smashicons"
             "https://www.flaticon.com/" ]

let usage = """
This program creates complex mission groups based on simple skelettons created
by users. This allows to create rich missions without advanced knowledge of the
mission editor and all its nodes.

The general process is as follows:
Create the scenery of the mission using the file editor. No logic needed,
only the static objects and targets. Note that using logic is allowed, e.g.
it is possible to use some of the pre-made groups available from
https://forum.il2sturmovik.com/topic/14803-the-groups-sharing-corner/
Static objects that are also target must have entities and a coalition.
Save the mission file, for instance in MyMission.Mission.

In a DIFFERENT MISSION FILE, create the skeletton of the complex group to
instiante. Create the planes, set their loadout and other attributes.
Trace their mission path using waypoints.

Select all objects in the mission editor and save selection to file.
The name of the file must conform to the kind of complex group to instantiate
(replace whatever with a string of your choice, without spaces):

- skel-groundattack-whatever.group for a ground attack without escorts.
- skel-escort-whatever.group for a ground attack with escort.
- skel-patrol-whatever.group for a fighter patrol.

For more detailed instructions, start by creating one plane, save to the group
and run this program (see below). You should get messages instructing you on
the next steps.

Once all errors have been resolved a new group file named after the skeletton
group, but with "skel" replaced with "inst" is produced. Additionally,
a TODO list is printed that indicates the manual steps needed to integrate
the instantiated group with the mission (MyMission.Mission in the example
at the top of these instructions).

To process one or more skelettons, simply call this program and pass the names
of the skeletton groups as arguments.
Alternatively, you may drag&drop the group file on the icon of
GroupInstantiator.exe.
"""

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

    if argv.Length = 0 then
        printfn "%s" usage

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

    System.Console.ReadKey(true) |> ignore
    status // return an integer exit code
