#r "../../plog/ploggy/bin/Debug/ploggy.dll"
#r "../DataProvider/bin/Debug/DataProvider.dll"
#r "../Campaign/bin/Debug/Campaign.dll"
#r "../../plog/packages/NLog.4.0.1/lib/net45/NLog.dll"
#r "../Blocks/packages/FsPickler.3.2.0/lib/net45/FsPickler.dll"
#r "../Blocks/packages/System.Numerics.Vectors.4.3.0/lib/net46/System.Numerics.Vectors.dll"

#load "Configuration.fsx" 

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction
open Campaign.NewWorldState

open System.IO
open ploggy
open NLog

let serverDataDir = @"E:\dserver\data"
let missionLogsDir = Path.Combine(serverDataDir, "logs")

open MBrace.FsPickler
let serializer = FsPickler.CreateXmlSerializer(indent = true)
let world, state =
    try
        use worldFile = File.OpenText(Path.Combine(Configuration.OutputDir, "world.xml"))
        use stateFile = File.OpenText(Path.Combine(Configuration.OutputDir, "state.xml"))
        serializer.Deserialize<World>(worldFile),
        serializer.Deserialize<WorldState>(stateFile)
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
            entry.MissionTime = state.Date
        | _ -> true
    )
    |> Seq.takeWhile (
        function
        | :? MissionStartEntry as entry ->
            entry.MissionTime = state.Date
        | _ -> true
    )
    |> Seq.cache


let resups = extractResupplies world state entries |> List.ofSeq
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
    let backupName = sprintf "state-%s.xml" (state.Date.ToShortTimeString())
    File.Copy(Path.Combine(outputDir, "state.xml"), Path.Combine(outputDir, backupName))
    use stateFile = File.CreateText(Path.Combine(outputDir, "state.xml"))
    serializer.Serialize(stateFile, state)
