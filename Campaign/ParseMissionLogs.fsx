// Parse the mission log and produce a new world state.

#I @"..\Campaign\bin\Debug"

#r "Campaign.dll"
#r "NLog.dll"
#r "FsPickler.dll"
#r "ploggy.dll"

#load "Configuration.fsx" 

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction
open Campaign.NewWorldState
open Campaign.Orders

open System.IO
open ploggy
open NLog

let serverDataDir = @"E:\dserver\data"
let missionLogsDir = Path.Combine(serverDataDir, "logs")

open MBrace.FsPickler
let serializer = FsPickler.CreateXmlSerializer(indent = true)
let world, state, axisOrders, alliesOrders =
    try
        use worldFile = File.OpenText(Path.Combine(Configuration.OutputDir, "world.xml"))
        use stateFile = File.OpenText(Path.Combine(Configuration.OutputDir, "state.xml"))
        use axisOrdersFile = File.OpenText(Path.Combine(Configuration.OutputDir, "axisOrders.xml"))
        use alliesOrdersFile = File.OpenText(Path.Combine(Configuration.OutputDir, "alliesOrders.xml"))
        serializer.Deserialize<World>(worldFile),
        serializer.Deserialize<WorldState>(stateFile),
        serializer.Deserialize<OrderPackage>(axisOrdersFile),
        serializer.Deserialize<OrderPackage>(alliesOrdersFile)
    with
    | e -> failwithf "Failed to read world and state data. Did you run Init.fsx? Reason was: '%s'" e.Message

let config = Config.LoggingConfiguration()
LogManager.Configuration <- config

Plogger.Init()

let entries =
    seq {
        for file in Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt") do
            for line in File.ReadAllLines(file) do
                yield LogEntry.Parse(line)
    }
    |> Seq.filter (
        function
        | :? MissionStartEntry
        | :? ObjectSpawnedEntry
        | :? TakeOffEntry
        | :? MissionObjectiveEntry
        | :? DamageEntry
        | :? KillEntry
        | :? LandingEntry
        | :? RoundEndEntry
        | :? MissionEndEntry -> true
        | _ -> false
    )
    |> Seq.skipWhile (
        function
        | :? MissionStartEntry as entry ->
            entry.MissionTime <> state.Date
        | _ -> true
    )
    |> Seq.takeWhile (
        function
        | :? MissionStartEntry as entry ->
            entry.MissionTime = state.Date
        | _ -> true
    )
    |> Seq.cache


let resups =
    let resupsAxis = extractResupplies world state axisOrders.Resupply entries |> List.ofSeq
    let resupsAllies = extractResupplies world state alliesOrders.Resupply entries |> List.ofSeq
    resupsAxis @ resupsAllies
let staticDamages = extractStaticDamages world state entries |> List.ofSeq
let takeOffs, landings =
    let both =
        extractTakeOffs world entries
        |> List.ofSeq
    both |> List.choose (function Choice1Of2 x -> Some x | _ -> None),
    both |> List.choose (function Choice2Of2 x -> Some x | _ -> None)

let dt = 60.0f<H> * float32 Configuration.MissionLength
let state2 = newState dt world state resups staticDamages takeOffs landings

do
    let outputDir = Configuration.OutputDir
    let backupName =
        sprintf "state-%s-%s.xml" (state.Date.ToShortDateString()) (state.Date.ToShortTimeString())
        |> fun x -> x.Replace(":", "-")
    File.Copy(Path.Combine(outputDir, "state.xml"), Path.Combine(outputDir, backupName))
    use stateFile = File.CreateText(Path.Combine(outputDir, "state.xml"))
    serializer.Serialize(stateFile, state2)
