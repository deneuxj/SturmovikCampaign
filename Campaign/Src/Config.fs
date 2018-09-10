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


open FSharp.Configuration
open System.IO
open Util
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.Blocks.BlocksMissionData
open Campaign.PlaneSet

open NLog

let private logger = LogManager.GetCurrentClassLogger()

/// Get the region and date of a scenario
let extractRegionAndDate (strategyFile : string) =
    let s = Stream.FromFile strategyFile
    let data = T.GroupData(s)
    let options = data.ListOfOptions.Head
    let date = options.GetDate()
    let date = System.DateTime(date.Year, date.Month, date.Day)
    let region =
        match options.GetGuiMap().Value.ToLowerInvariant() with
        | Contains "stalingrad" -> Region.Stalingrad
        | Contains "moscow" -> Region.Moscow
        | Contains "vluki" -> Region.VelikieLuki
        | Contains "kuban" -> Region.Kuban
        | other -> failwithf "No region for '%s'" other
    region, date

type Configuration = {
    PlaneSetName : string
    UseTextMissionFile : bool
    Seed : int option
    WeatherDayMaxOffset : int
    MaxFires : int
    MaxConvoys : int
    MaxAttackers : int
    MaxPatrols : int
    MaxCapturedPlanes : int
    MissionName : string
    MissionLength : int
    LongWorkDay : bool
    ColumnSplitInterval : int
    MaxSimultaneousConvoys : int
    MaxSimultaneousFerryFlights : int
    MaxVehiclesInBattle : int
    BattleKillRatio: int
    MaxBattleKillsRatioByPlayers: float32
    MaxBattleKillsRatioByAI: float32
    MaxStaticPlanes : int
    MaxCash : int
    InitialCash : int
    MaxReservedPlanes : int // Per type and airfield
    MaxTotalReservedPlanes : int // All types, all airfields
    RearAirfieldCostFactor : float32
    SpawnsAreRestricted : bool
    FreshFighterSpawns : int
    FreshAttackerSpawns : int
    FreshBomberSpawns : int
    FreshTransportSpawns : int
    FreshFighterRefill : int
    FreshAttackerRefill : int
    FreshBomberRefill : int
    FreshTransportRefill : int
    PlaneRentalAllowed : bool
    MoneyBackFactor : float32
    EnablePlayerTanks : bool
    OutputDir : string
    ServerDataDir : string
    PurgeLogs : bool
    ServerBinDir : string
    ServerSdsFile : string
    ScriptPath : string
    Briefing : string
    ThinkTime : int
    DesiredProduction : float32
    PlaneProduction : float32
    RearAirfieldPlanes : int
    MaxFriendlyFireEvents : int
    FriendlyFireBanDuration : int
    MaxNoobScore : float32
    NoobBanDuration : int
    WebHook : string
    DiscordUserId : string
    MaxBuildingIcons : int
    ChatLogCommandsEnabled : bool
}
with
    static member Default =
        {
            PlaneSetName = ""
            UseTextMissionFile = false
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
            MaxStaticPlanes = 6
            InitialCash = 5000
            MaxCash = 10000
            MaxReservedPlanes = 2
            MaxTotalReservedPlanes = 6
            SpawnsAreRestricted = true
            FreshFighterSpawns = 6
            FreshAttackerSpawns = 3
            FreshBomberSpawns = 2
            FreshTransportSpawns = 1
            FreshFighterRefill = 2
            FreshAttackerRefill = 1
            FreshBomberRefill = 1
            FreshTransportRefill = 1
            PlaneRentalAllowed = true
            RearAirfieldCostFactor = 0.125f
            MoneyBackFactor = 0.75f
            EnablePlayerTanks = true
            MissionName = "AutoGenMission2"
            MissionLength = 180
            LongWorkDay = false
            ColumnSplitInterval = 60
            OutputDir = @"nul"
            ServerDataDir = @"nul"
            PurgeLogs = true
            ServerBinDir = @"nul"
            ServerSdsFile = @"nul"
            ScriptPath = @"nul"
            ThinkTime = 30
            DesiredProduction = 300.0f
            PlaneProduction = 1000.0f
            RearAirfieldPlanes = 5000
            WebHook = ""
            DiscordUserId = ""
            MaxFriendlyFireEvents = 2
            FriendlyFireBanDuration = 1
            MaxNoobScore = 3.0f
            NoobBanDuration = 1
            MaxBuildingIcons = 10
            ChatLogCommandsEnabled = false
            Briefing = @"
    This mission is part of a dynamic campaign, where the events from one mission affect the following missions.

    Objectives: Truck convoys, tank columns, field camps (look for dugouts), factories, parked planes, anti-tank guns, anti-air cannons.

    Each region has a 'life bar' indicating the storage capacity (blue bar) and current supply level (cursor on the bar).
    If the cursor is red, the region is poorly defended.
    Some regions have numbers beside their name: these represent the number of tanks in that region at the start of the mission.

    Planes can be transferred from one airfield to another, but the result will only be visible in the next mission.

    "
        }

    member this.PlaneSet(scenario : string) =
        match this.PlaneSetName with
        | null | "" | "auto" ->
            let region, date = extractRegionAndDate(Path.Combine(this.ScriptPath, scenario) + ".Mission")
            loadPlaneSets (Path.Combine(this.ScriptPath, "Config"))
            |> tryPickPlaneSet region date
            |> Option.defaultValue (PlaneSet.Default)
        | planeSetName ->
            try
                let file = PlaneSetFile()
                file.Load(Path.Combine(this.ScriptPath, "Config", "planeSet-" + planeSetName + ".yaml"))
                PlaneSet.FromYaml(file.PlaneSet)
            with
            | e ->
                logger.Error(sprintf "Failed to load planeset '%s': %s" planeSetName e.Message)
                PlaneSet.Default


[<Literal>]
let private sampleFile = __SOURCE_DIRECTORY__ + @"\..\Config\SampleConfig.yaml"
type ConfigFile = YamlConfig<sampleFile>

let loadConfigFile (path : string) =
    let config = ConfigFile()
    config.Load(path)
    let values = config.Campaign
    {
        PlaneSetName = values.PlaneSet
        UseTextMissionFile = values.UseTextMissionFile
        Seed =
            match values.Seed with
            | -1 -> None
            | x -> Some x
        WeatherDayMaxOffset = values.WeatherDayMaxOffset
        MaxFires = values.MaxFires
        MaxBuildingIcons = values.MaxBuildingIcons
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
        MaxStaticPlanes = values.MaxStaticPlanes
        SpawnsAreRestricted = values.SpawnRestrictions
        FreshFighterSpawns = values.FreshFighterSpawns
        FreshAttackerSpawns = values.FreshAttackerSpawns
        FreshBomberSpawns = values.FreshBomberSpawns
        FreshTransportSpawns = values.FreshTransportSpawns
        FreshFighterRefill = values.FreshFighterRefill
        FreshAttackerRefill = values.FreshAttackerRefill
        FreshBomberRefill = values.FreshBomberRefill
        FreshTransportRefill = values.FreshTransportRefill
        PlaneRentalAllowed = values.PlaneRentalAllowed
        MaxCash = values.MaxCash
        InitialCash = values.InitialCash
        MaxReservedPlanes = values.MaxReservedPlanes
        MaxTotalReservedPlanes = values.MaxTotalReservedPlanes
        MoneyBackFactor = float32 values.MoneyBackFactor
        RearAirfieldCostFactor = float32 values.RearAirfieldCostFactor
        EnablePlayerTanks = values.EnablePlayerTanks
        MissionName = values.MissionName
        MissionLength = values.MissionLength
        LongWorkDay = values.LongWorkDay
        ColumnSplitInterval = values.ColumnSplitInterval
        OutputDir = values.OutputDir
        ServerDataDir = values.ServerDataDir
        ServerBinDir = values.ServerBinDir
        ServerSdsFile = values.ServerSdsFile
        ScriptPath = values.InstallPath
        ThinkTime = values.ThinkTime
        DesiredProduction = float32 values.DesiredProduction
        PlaneProduction = float32 values.PlaneProduction
        RearAirfieldPlanes = values.RearAirfieldPlanes
        Briefing = values.Briefing
        WebHook = values.WebHook
        DiscordUserId = values.DiscordUserId
        FriendlyFireBanDuration = values.FriendlyFireBanDuration
        MaxFriendlyFireEvents = values.MaxFriendlyFireEvents
        MaxNoobScore = float32 values.MaxNoobScore
        NoobBanDuration = values.NoobBanDuration
        PurgeLogs = values.PurgeLogs
        ChatLogCommandsEnabled = values.ChatLogCommandsEnabled
    }
