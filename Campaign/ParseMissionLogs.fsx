#r "../../plog/ploggy/bin/Debug/ploggy.dll"
#r "../DataProvider/bin/Debug/DataProvider.dll"
#r "../Campaign/bin/Debug/Campaign.dll"
#r "../../plog/packages/NLog.4.0.1/lib/net45/NLog.dll"
#r @"C:\Users\johann\Documents\SturmovikMission-git\Blocks\packages\System.Numerics.Vectors.4.3.0\lib\net46\System.Numerics.Vectors.dll"


open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction

open System.IO
open ploggy
open NLog

let serverDataDir = @"E:\dserver\data"
let missionLogsDir = Path.Combine(serverDataDir, "logs")

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
    |> Seq.cache


let random = System.Random(0)
let strategyFile = "StrategySmall1.mission"
let world0, blocks, bridges, options = World.Create(strategyFile)
let world = { world0 with WeatherDaysOffset = 15.0 * (random.NextDouble() - 0.5)}
let state = WorldState.Create(world, strategyFile)

let resups = extractResupplies world state entries
let buildingDamages = extractBuildingDamages world state entries