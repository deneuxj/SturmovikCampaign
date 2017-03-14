// Generate a mission given a world state
#I "../Campaign/bin/Debug"

#r "DataProvider.dll"
#r "SturmovikMission.Blocks.dll"
#r "Campaign.dll"
#r "System.Numerics.Vectors"
#r "FsPickler.dll"

#load "Configuration.fsx" 

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.MissionGeneration
open Campaign.AutoOrder
open Campaign.Orders
open SturmovikMission.Blocks.BlocksMissionData
open System.IO

try
    File.Delete("StrategySmall1.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission", "StrategySmall1.mission")
    File.Delete("Blocks.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Blocks.mission", "Blocks.mission")
    File.Delete("Vehicles.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Vehicles.mission", "Vehicles.mission")
with
    | exc -> printfn "Error copying files: '%s'" exc.Message

let random =
    match Configuration.Seed with
    | Some n ->
        System.Random(n)
    | None ->
        System.Random()

open MBrace.FsPickler
let serializer = FsPickler.CreateXmlSerializer(indent = true)
let world, blocks, bridges, options, state =
    try
        use worldFile = File.OpenText(Path.Combine(Configuration.OutputDir, "world.xml"))
        use stateFile = File.OpenText(Path.Combine(Configuration.OutputDir, "state.xml"))
        use blocksFile = File.OpenText(Path.Combine(Configuration.OutputDir, "blocks.xml"))
        use bridgesFile = File.OpenText(Path.Combine(Configuration.OutputDir, "bridges.xml"))
        use optionsFile = File.OpenText(Path.Combine(Configuration.OutputDir, "options.xml"))
        serializer.Deserialize<World>(worldFile),
        serializer.Deserialize<T.Block list>(blocksFile),
        serializer.Deserialize<T.Bridge list>(bridgesFile),
        serializer.Deserialize<T.Options>(optionsFile),
        serializer.Deserialize<WorldState>(stateFile)
    with
    | e -> failwithf "Failed to read world and state data. Did you run Init.fsx? Reason was: '%s'" e.Message

let dt = 60.0f<H> * float32 Configuration.MissionLength
let mkOrders coalition =
    let convoyOrders =
        createAllConvoyOrders coalition (world, state)
        |> prioritizeConvoys Configuration.MaxConvoys dt world state
    let invasions =
        createGroundInvasionOrders(coalition, world, state)
        |> prioritizeGroundInvasionOrders(world, state)
        |> List.truncate Configuration.MaxInvasionsInPlanning
    let reinforcements =
        prioritizedReinforcementOrders(world, state) coalition invasions
    let reinforcements, invasions = filterIncompatible(reinforcements, invasions)
    convoyOrders, reinforcements |> List.truncate Configuration.MaxReinforcements, invasions |> List.truncate Configuration.MaxInvasions |> List.map (fun (x, _, _) -> x)
let adjustIndexes(convoys : ResupplyOrder list, reinforcements : ColumnMovement list, invasions : ColumnMovement list) =
    let convoys =
        convoys
        |> List.mapi (fun i order -> { order with Index = i + 1 })
    let n = List.length convoys
    let reinforcements =
        reinforcements
        |> List.mapi (fun i order -> { order with Index = i + 1 + n })
    let n = n + List.length reinforcements
    let invasions =
        invasions
        |> List.mapi (fun i order -> { order with Index = i + 1 + n })
    convoys, reinforcements, invasions
let mkAllOrders coalition =
    let convoys, reinforcements, invasions =
        mkOrders coalition
        |> adjustIndexes
    { Resupply = convoys
      Reinforcements = reinforcements
      Invasions = invasions
    }
let allAxisOrders = mkAllOrders Axis
let allAlliesOrders = mkAllOrders Allies

let author = "coconut"
let briefing = "Work in progress<br><br>Test of dynamically generated missions<br><br>"

do
    let outputDir = Configuration.OutputDir
    use axisOrderFiles = File.CreateText(Path.Combine(outputDir, "axisOrders.xml"))
    serializer.Serialize(axisOrderFiles, allAxisOrders)
    use alliesOrderFiles = File.CreateText(Path.Combine(outputDir, "alliesOrders.xml"))
    serializer.Serialize(alliesOrderFiles, allAlliesOrders)

let missionName = Configuration.MissionName
writeMissionFile random author Configuration.MissionName briefing Configuration.MissionLength Configuration.ConvoyInterval options blocks bridges world state allAxisOrders allAlliesOrders (Path.Combine(Configuration.OutputDir, missionName + ".Mission"))

let mpDir = Path.Combine(Configuration.ServerDataDir, "Multiplayer")
let swallow f = try f() with | _ -> ()
swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".Mission")))
swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".eng")))
swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".msnbin")))
File.Copy(Path.Combine(Configuration.OutputDir, missionName + ".Mission"), Path.Combine(mpDir, missionName + ".Mission"))
File.Copy(Path.Combine(Configuration.OutputDir, missionName + ".eng"), Path.Combine(mpDir, missionName + ".eng"))
open System.Diagnostics
let p = ProcessStartInfo("MissionResaver.exe", sprintf "-d %s -f %s" Configuration.ServerDataDir (Path.Combine(mpDir, missionName + ".Mission")))
p.WorkingDirectory <- Path.Combine(Configuration.ServerBinDir, "resaver")
p.UseShellExecute <- true
let proc = Process.Start(p)
proc.WaitForExit()
swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".Mission")))
printfn "Resaver exited with code %d" proc.ExitCode
