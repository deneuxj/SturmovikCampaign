// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Campaign.Configuration

open Campaign.BasicTypes
open Campaign.PlaneSet

open FSharp.Configuration
open System.IO
open SturmovikMission.Blocks.Util.String
open NLog

let private logger = LogManager.GetCurrentClassLogger()

type Configuration = {
    PlaneSet : PlaneSet
    StrategyFile : string
    Seed : int option
    WeatherDayMaxOffset : int
    MaxFires : int
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
    BattleKillRatio: int
    MaxBattleKillsRatioByPlayers: float32
    MaxBattleKillsRatioByAI: float32
    OutputDir : string
    ServerDataDir : string
    ServerBinDir : string
    ServerSdsFile : string
    ScriptPath : string
    Briefing : string
    ThinkTime : int
    DesiredProduction : float32
    PlaneProduction : float32
    MaxFriendlyFireEvents : int
    FriendlyFireBanDuration : int
    MaxNoobScore : float32
    NoobBanDuration : int
    WebHook : string
}
with
    static member Default =
        {
            PlaneSet = PlaneSet.Default
            StrategyFile = "StrategySmall1.mission"
            Seed = None // Some 0
            WeatherDayMaxOffset = 15
            MaxFires = 20
            MaxConvoys = 10
            MaxSimultaneousConvoys = 2
            MaxSimultaneousFerryFlights = 3
            MaxVehiclesInBattle = 15
            MaxPatrols = 6
            MaxAttackers = 3
            MaxCapturedPlanes = 3
            BattleKillRatio = 5
            MaxBattleKillsRatioByPlayers = 1.0f
            MaxBattleKillsRatioByAI = 0.25f
            MissionName = "AutoGenMission2"
            MissionLength = 180
            ColumnSplitInterval = 60
            OutputDir = @"nul"
            ServerDataDir = @"nul"
            ServerBinDir = @"nul"
            ServerSdsFile = @"nul"
            ScriptPath = @"nul"
            ThinkTime = 30
            DesiredProduction = 300.0f
            PlaneProduction = 1000.0f
            WebHook = ""
            MaxFriendlyFireEvents = 2
            FriendlyFireBanDuration = 1
            MaxNoobScore = 3.0f
            NoobBanDuration = 1
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
    let planeSet =
        try
            let file = PlaneSetFile()
            file.Load(Path.Combine(values.InstallPath, "planeSet-" + values.PlaneSet + ".yaml"))
            PlaneSet.FromYaml(file.PlaneSet)
        with
        | e ->
            logger.Error(sprintf "Failed to load planeset '%s': %s" values.PlaneSet e.Message)
            PlaneSet.Default
    {
        PlaneSet = planeSet
        StrategyFile = values.StrategyFile
        Seed =
            match values.Seed with
            | -1 -> None
            | x -> Some x
        WeatherDayMaxOffset = values.WeatherDayMaxOffset
        MaxFires = values.MaxFires
        MaxConvoys = values.MaxConvoys
        MaxSimultaneousConvoys = values.MaxSimultaneousConvoys
        MaxSimultaneousFerryFlights = values.MaxSimultaneousFerryFlights
        MaxVehiclesInBattle = values.MaxVehiclesInBattle
        MaxPatrols = values.MaxPatrols
        MaxAttackers = values.MaxAttackers
        MaxCapturedPlanes = values.MaxCapturedPlanes
        BattleKillRatio = values.BattleKillRatio
        MaxBattleKillsRatioByPlayers = float32 values.MaxBattleKillsRatioByPlayers
        MaxBattleKillsRatioByAI = float32 values.MaxBattleKillsRatioByAI
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
        PlaneProduction = float32 values.PlaneProduction
        Briefing = values.Briefing
        WebHook = values.WebHook
        FriendlyFireBanDuration = values.FriendlyFireBanDuration
        MaxFriendlyFireEvents = values.MaxFriendlyFireEvents
        MaxNoobScore = float32 values.MaxNoobScore
        NoobBanDuration = values.NoobBanDuration
    }
