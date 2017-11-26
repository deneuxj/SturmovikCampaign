module Campaign.Configuration

open Campaign.BasicTypes
open PlaneModel
open FSharp.Configuration

type Configuration = {
    PlaneSet : PlaneModel.PlaneSet
    StrategyFile : string
    Seed : int option
    WeatherDayMaxOffset : int
    MaxConvoys : int
    MaxAttackers : int
    MaxPatrols : int
    MaxCapturedPlanes : int
    MissionName : string
    MissionLength : int
    ColumnSplitInterval : int
    MaxSimultaneousConvoys : int
    MaxSimultaneousFerryFlights : int
    MaxVehiclesInBattle : int
    OutputDir : string
    ServerDataDir : string
    ServerBinDir : string
    ServerSdsFile : string
    ScriptPath : string
    Briefing : string
    ThinkTime : int
    DesiredProduction : float32
    MaxTankNeeds : float32
    PlaneProduction : float32
    WebHook : string
}
with
    static member Default =
        {
            PlaneSet = PlaneSet.Moscow
            StrategyFile = "StrategySmall1.mission"
            Seed = None // Some 0
            WeatherDayMaxOffset = 15
            MaxConvoys = 10
            MaxSimultaneousConvoys = 2
            MaxSimultaneousFerryFlights = 3
            MaxVehiclesInBattle = 15
            MaxPatrols = 6
            MaxAttackers = 3
            MaxCapturedPlanes = 3
            MissionName = "AutoGenMission2"
            MissionLength = 180
            ColumnSplitInterval = 60
            OutputDir = @"nul"
            ServerDataDir = @"nul"
            ServerBinDir = @"nul"
            ServerSdsFile = @"nul"
            ScriptPath = @"nul"
            ThinkTime = 30
            DesiredProduction = 6000.0f
            MaxTankNeeds = 30.0f
            PlaneProduction = 1000.0f
            WebHook = ""
            Briefing = @"
    This mission is part of a dynamic campaign, where the events from one mission affect the following missions.

    Objectives: Truck convoys, tank columns, field camps (look for dugouts), factories, parked planes, anti-tank guns, anti-air cannons.

    Each region has a 'life bar' indicating the storage capacity (blue bar) and current supply level (cursor on the bar).
    If the cursor is red, the region is poorly defended.
    Some regions have numbers beside their name: these represent the number of tanks in that region at the start of the mission.

    Planes can be transferred from one airfield to another, but the result will only be visible in the next mission.

    "
        }

[<Literal>]
let sampleFile = __SOURCE_DIRECTORY__ + @"\SampleConfig.yaml"
type ConfigFile = YamlConfig<sampleFile>

let loadConfigFile (path : string) =
    let config = ConfigFile()
    config.Load(path)
    let values = config.Campaign
    {
        PlaneSet =
            match values.PlaneSet with
            | "Moscow" -> PlaneSet.Moscow
            | "VelikieLuki" -> PlaneSet.VelikieLuki
            | "EarlyAccess" -> PlaneSet.EarlyAccess
            | "Stalingrad" -> PlaneSet.Stalingrad
            | x -> failwithf "Invalid PlaneSet value: '%s'" x
        StrategyFile = values.StrategyFile
        Seed =
            match values.Seed with
            | -1 -> None
            | x -> Some x
        WeatherDayMaxOffset = values.WeatherDayMaxOffset
        MaxConvoys = values.MaxConvoys
        MaxSimultaneousConvoys = values.MaxSimultaneousConvoys
        MaxSimultaneousFerryFlights = values.MaxSimultaneousFerryFlights
        MaxVehiclesInBattle = values.MaxVehiclesInBattle
        MaxPatrols = values.MaxPatrols
        MaxAttackers = values.MaxAttackers
        MaxCapturedPlanes = values.MaxCapturedPlanes
        MissionName = values.MissionName
        MissionLength = values.MissionLength
        ColumnSplitInterval = values.ColumnSplitInterval
        OutputDir = values.OutputDir
        ServerDataDir = values.ServerDataDir
        ServerBinDir = values.ServerBinDir
        ServerSdsFile = values.ServerSdsFile
        ScriptPath = values.InstallPath
        ThinkTime = values.ThinkTime
        DesiredProduction = float32 values.DesiredProduction
        MaxTankNeeds = float32 values.MaxTankNeeds
        PlaneProduction = float32 values.PlaneProduction
        Briefing = values.Briefing
        WebHook = values.WebHook
    }
