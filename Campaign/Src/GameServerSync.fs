﻿// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
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

/// Control game server start and stop
namespace Campaign.GameServerSync

open System
open System.IO
open System.Numerics
open FSharp.Json
open FSharp.Control

open Util
open Util.Json
open Util.StringPatterns
open VectorExtension

open Campaign.Common.BasicTypes
open Campaign.Common.Targets
open Campaign.Common.Targets.ActivePatterns

open Campaign.MissionGen.MissionFileGeneration

open Campaign
open Campaign.NewWorldDescription
open Campaign.NewWorldDescription.IO
open Campaign.WarState
open Campaign.WarState.IO
open Campaign.GameServerControl
open Campaign.CampaignScenario
open Campaign.CampaignScenario.IO
open Campaign.WarStateUpdate.CommandExecution

type SyncState =
    | PreparingMission of IncludeBattles : bool
    | ResavingMission
    | RunningMission of StartTime: DateTime * EndTime: DateTime
    | ExtractingResults of Pattern: string
    | SkippingMission
    | AdvancingScenario
    | ErrorState of Message: string * InnerState: SyncState option
with
    static member FileName = "sync.json"

    member this.Description =
        match this with
        | PreparingMission true -> "Preparing mission"
        | PreparingMission false -> "Preparing mission (without battles)"
        | ResavingMission -> "Resaving mission"
        | ExtractingResults _ -> "Extracting results"
        | SkippingMission -> "Skipping mission"
        | AdvancingScenario -> "Advancing scenario"
        | RunningMission(_, endTime) -> sprintf "Running mission (%03d minutes left)" (int (endTime - System.DateTime.UtcNow).TotalMinutes)
        | ErrorState(msg, state) ->
            let inner =
                state
                |> Option.map (fun state -> state.Description)
                |> Option.defaultValue "None"
            sprintf "Error while in state %s: %s" inner msg

module IO =
    /// Wrap SyncState in a record so that fieldless cases, which are saved as strings, can be saved as valid json.
    type SyncStateVessel =
        {
            SyncState : SyncState
        }

    type SyncState with
        member this.Save(workDir : string) =
            let tmpFile = IO.Path.Combine(workDir, SyncState.FileName + ".tmp")
            use file = IO.File.CreateText(tmpFile)
            let json = FSharp.Json.Json.serialize { SyncState = this }
            file.Write(json)
            file.Close()
            IO.File.Copy(tmpFile, IO.Path.Combine(workDir, SyncState.FileName), true)
            IO.File.Delete(tmpFile)

        static member Delete(workDir : string) =
            IO.File.Delete(IO.Path.Combine(workDir, SyncState.FileName))

        static member TryLoad(workDir) =
            try
                use file = IO.File.OpenText(IO.Path.Combine(workDir, SyncState.FileName))
                FSharp.Json.Json.deserialize<SyncStateVessel>(file.ReadToEnd())
                |> Some
            with _ -> None
            |> Option.map (fun vessel -> vessel.SyncState)


/// Inform players of interesting events while a game is going on
type LiveNotifier(commands : AsyncSeq<WarStateUpdate.Commands>, war : WarState, notifier : IPlayerNotifier) =
    let mutable isMuted = true

    let logger = NLog.LogManager.GetCurrentClassLogger()

    member this.UnMute() =
        if isMuted then
            logger.Debug("LiveNotifier is unmuted")
            isMuted <- false

    member this.Run() =
        async {
            logger.Info("LiveNotifier is now running")
            return!
                commands
                |> AsyncSeq.iterAsync(fun command -> async {
                    if not isMuted then
                        try
                            match command with
                            | WarStateUpdate.Commands.RegisterPilotFlight(pid, flight, health) ->
                                let pilot = war.GetPilot(pid)
                                let rank =
                                    Pilots.tryComputeRank war.World.Ranks pilot
                                    |> Option.map (fun rank -> rank.RankAbbrev)
                                    |> Option.defaultValue ""
                                let eventDescription =
                                    match flight.Return with
                                    | CrashedInEnemyTerritory _ -> "crashed in enemy territory"
                                    | CrashedInFriendlyTerritory _ -> "crash-landed"
                                    | AtAirfield afId -> sprintf "landed at %s" afId.AirfieldName
                                let msg = sprintf "%s %s has %s" rank pilot.FullName eventDescription
                                let coalition = war.World.Countries.[pilot.Country]
                                let! s = notifier.MessageCoalition(coalition, msg)
                                match health with
                                | Pilots.Healthy -> ()
                                | Pilots.Dead ->
                                    let msg = sprintf "The career of %s %s has ended" rank pilot.PilotLastName
                                    let! s = notifier.MessageCoalition(coalition, msg)
                                    ()
                                | Pilots.Injured until ->
                                    let msg = sprintf "%s %s is injured until at least %s" rank pilot.PilotLastName (until.ToString(pilot.Country.CultureInfo))
                                    let! s = notifier.MessageCoalition(coalition, msg)
                                    ()
                            | WarStateUpdate.Commands.UpdatePilot(pilot) when pilot.Health = Pilots.PilotHealth.Healthy ->
                                let player =
                                    match war.TryGetPlayer(pilot.PlayerGuid) with
                                    | Some player -> player.Name
                                    | None -> "<incognito>"
                                let rank =
                                    Pilots.tryComputeRank war.World.Ranks pilot
                                    |> Option.map (fun rank -> rank.RankAbbrev)
                                    |> Option.defaultValue ""
                                let msg = sprintf "%s controls %s %s" player rank pilot.FullName
                                let! s = notifier.MessageAll(msg)
                                ()
                            | _ ->
                                ()
                        with exc ->
                            logger.Warn("Live notifier command-handling failed")
                            logger.Debug(exc)
                    try
                        logger.Debug("Execute command")
                        command.Execute(war) |> ignore
                    with exc ->
                        logger.Warn("Command execution in live notifier failed")
                        logger.Debug(exc)
                })
        }


module BaseFileNames =
    open System.Text.RegularExpressions
    open RegexActivePatterns

    /// Name of the world description file, one file for the entire campaign
    let worldFilename = "world.json"
    /// Name of the scenario parameters file, one file for the entire campaign
    let scenarioCtrlFilename = "scenarioParameters.json"
    /// Current state of the campaign
    let stateBaseFilename = "-state.json"
    /// Current campaign scenario step and its data
    let stepBaseFilename = "-step.json"
    /// Commands and results that lead to the current state
    let simulationBaseFilename = "-simulation.json"
    /// Commands extracted from the game logs, and the results of the commands, precedes simulation when advancing the campaign state.
    let effectsBaseFilename = "-effects.json"

    let getStateFilename idx = sprintf "%03d%s" idx stateBaseFilename
    let getStepFilename idx = sprintf "%03d%s" idx stepBaseFilename
    let getSimulationFilename idx = sprintf "%03d%s" idx simulationBaseFilename
    let getEffectsFilename idx = sprintf "%03d%s" idx effectsBaseFilename

    /// Get the highest N such that N-state.xml exists
    let getCurrentIndex (path : string) =
        let pattern = Regex(@"^(\d*)-state.json")
        try
            IO.Directory.EnumerateFiles(path, "*" + stateBaseFilename)
            |> Seq.choose (fun path ->
                let filename = Path.GetFileName(path)
                match filename with
                | MatchesRegex(pattern) (GroupList [AsInt index]) ->
                    Some index
                | _ -> None)
            |> Seq.max
        with _ -> 0

module WarStateExt =
    open Campaign.MissionGen.MissionFileGeneration

    let mkMissionBuilderDataAccess (war : WarState, duration) =
        let supplies = war.ComputeSupplyAvailability()
        let road = war.ComputeRoadCapacity()
        let rail = war.ComputeRailCapacity()
        let transport(r1, r2) =
            if war.GetOwner(r1) = war.GetOwner(r2) then
                road(r1, r2) + rail(r1, r2)
            else
                road(r1, r2)
        { new IMissionBuilderData with
              member this.GetRegionAntiAirCapacity(region, coalition) =
                  let supplies rId = supplies rId * war.World.ResourceVolume
                  (war.ComputeRegionAntiAirBudget(transport, supplies, region, coalition) * duration) / 1.0f<M^3>
              member this.Airfields: IAirfield list =
                  war.World.Airfields.Values |> Seq.map (fun af -> af :> IAirfield) |> List.ofSeq
              member this.BuildingDamages: (Common.Buildings.BuildingInstanceId * int * float32) list =
                  war.BuildingDamages |> List.ofSeq
              member this.GetAirfield(afId: AirfieldId): IAirfield = 
                  war.World.Airfields.[afId] :> IAirfield
              member this.GetBuildingInstance(bid: Common.Buildings.BuildingInstanceId): Common.Buildings.BuildingInstance = 
                  war.World.GetBuildingInstance(bid)
              member this.GetCountryCoalition(country: CountryId): CoalitionId = 
                  war.World.Countries.[country]
              member this.GetCoalitionMainCountry(coalition: CoalitionId): CountryId =
                  war.World.GetAnyCountryInCoalition(coalition)
              member this.GetOwner(rId: RegionId): CoalitionId option = 
                  war.GetOwner(rId)
              member this.GetOwner(afId: AirfieldId): CountryId option = 
                  war.GetOwner(war.World.Airfields.[afId].Region)
                  |> Option.map war.World.GetAnyCountryInCoalition
              member this.GetOwnerAt(pos : Vector2): CountryId option = 
                  war.World.Regions.Values
                  |> Seq.tryFind (fun region -> pos.IsInConvexPolygon(region.Boundary))
                  |> Option.bind (fun region -> war.GetOwner(region.RegionId))
                  |> Option.map war.World.GetAnyCountryInCoalition
              member this.GetPlaneModel(plane: Common.PlaneModel.PlaneModelId): Common.PlaneModel.PlaneModel = 
                  war.World.PlaneSet.[plane]
              member this.GetGroundUnit(id : Common.GroundUnit.GroundUnitId): Common.GroundUnit.GroundUnit =
                  war.World.GroundUnits.[id]
              member this.GetRegion(rId: RegionId): IRegion = 
                  war.World.Regions.[rId] :> IRegion
              member this.Regions: IRegion list = 
                  war.World.Regions.Values |> Seq.map (fun region -> region :> IRegion) |> List.ofSeq
              member this.GetBuildingDurability(script : string) =
                  match war.World.BuildingProperties.TryGetValue(script) with
                  | true, x -> Some x.Durability
                  | false, _ -> None
        }

    type WarState with
        member this.AsMissionBuilderData(fightDayFactor) = mkMissionBuilderDataAccess(this, fightDayFactor)

open BaseFileNames
open IO
open WarStateExt

/// Controls execution of DServer, depending on status of campaign scenario controller.
type Sync(settings : Settings, gameServer : IGameServerControl, ?logger) =
    let mutable logger = defaultArg logger (NLog.LogManager.GetCurrentClassLogger())
    let mutable controller : IScenarioController option = None
    let mutable war : WarState option = None
    let mutable step : ScenarioStep option = None

    let mutable serverProcess = None
    let mutable state : SyncState option = None

    let mutable stopAfterMission = false
    let mutable isRunning = false

    let cancellation = new System.Threading.CancellationTokenSource()

    // Number of DServer restart attempts
    let maxRetries = 3

    let terminated = Event<Sync>()
    let stateChanged = Event<IWarStateQuery>()
    let stalled = Event<unit>()

    let wkPath f = Path.Combine(settings.WorkDir, f)

    /// Get the current synchronization state
    member this.SyncState = state

    /// Campaign scenario controller subscribes to this event to know when sync is terminated
    member this.Terminated = terminated.Publish

    /// Campaign scenario controller subscribes to this event to know when war state has changed
    member this.StateChanged = stateChanged.Publish

    /// DServer has not produced new log files for 2 minutes
    member this.Stalled = stalled.Publish

    /// Controls whether the workflow should be terminated when the current mission (or the one being prepared) ends.
    member this.StopAfterMission
        with get() = stopAfterMission
        and set x = stopAfterMission <- x

    /// Get a seed computed from the state of the war
    member this.Seed =
        match war with
        | Some war -> war.Seed
        | None -> 0

    member this.SaveState() =
        match state with
        | Some state -> state.Save(settings.WorkDir)
        | None ->
            try
                SyncState.Delete(settings.WorkDir)
            with e ->
                logger.Warn("Failed to delete sync state file")
                logger.Warn e

    /// Stop synchonization, typically after an unrecoverable error.
    member this.Die(msg) =
        let msg =
            sprintf "Game server sync terminated: %s" msg
        logger.Info msg
        terminated.Trigger(this)
        isRunning <- false

    /// Cancel any ongoing task.
    member private this.Interrupt(msg, killServer) =
        let msg =
            sprintf "Game server sync interrupted: %s" msg
        logger.Info msg
        if killServer then
            gameServer.KillProcess(serverProcess)
            |> ignore
        cancellation.Cancel()
        isRunning <- false

    member this.Init() =
        async {
            try
                // Load world
                let path = wkPath worldFilename
                let world =
                    if File.Exists path then
                        try
                            let w = World.LoadFromFile path
                            Some w
                        with e ->
                            eprintfn "Failed to load '%s': %s" path e.Message
                            None
                    else
                        None

                // Load latest state
                let latestState =
                    Directory.EnumerateFiles(settings.WorkDir, "*.json")
                    |> Seq.filter (fun s -> s.EndsWith(stateBaseFilename))
                    |> Seq.sortDescending
                    |> Seq.tryHead

                let war0 =
                    match world, latestState with
                    | Some world, Some latestState ->
                        try
                            let war = WarState.LoadFromFile(latestState, world)
                            Some war
                        with e ->
                            eprintfn "Failed to load '%s': %s" latestState e.Message
                            None
                    | _ ->
                        None

                // Restore scenario controller
                let controller0 =
                    match war0 with
                    | Some war ->
                        let path = Path.Combine(settings.WorkDir, scenarioCtrlFilename)
                        try
                            WorldWar2.LoadFromFile(war.World, path)
                            :> IScenarioController
                            |> Some
                        with exc ->
                            logger.Error("Failed to restore scenario controller")
                            logger.Debug(exc)
                            None
                    | None ->
                        None

                // Load scenario state
                let latestStep =
                    Directory.EnumerateFiles(settings.WorkDir, "*.json")
                    |> Seq.filter (fun s -> s.EndsWith(stepBaseFilename))
                    |> Seq.sortDescending
                    |> Seq.tryHead
                let step0 =
                    try
                        match controller0 with
                        | Some sctrl ->
                            match latestStep with
                            | Some path ->
                                use reader = new StreamReader(path)
                                let step = ScenarioStep.Deserialize(reader, sctrl.DeserializeStepData)
                                Some step
                            | None ->
                                None
                        | _ ->
                            None
                    with e ->
                        logger.Error("Failed to load scenario state")
                        logger.Debug(e)
                        None

                let syncState =
                    SyncState.TryLoad(settings.WorkDir)
                    |> Option.defaultValue (PreparingMission true)

                controller <- controller0
                war <- war0
                step <- step0
                state <- Some syncState

                return Ok()
            with
            | exc ->
                logger.Warn("Failed to initialize:")
                logger.Debug(exc)
                return Error "Initialization failure"
        }

    /// Start DServer
    member this.StartServerAsync(?retriesLeft) =
        let retriesLeft = defaultArg retriesLeft maxRetries
        async {
            logger.Trace retriesLeft
            if retriesLeft < 0 then
                return this.Die("Server start aborted after max retries reached")
            else
            let! s = gameServer.StartProcessWithSds(settings.SdsFile)
            match s with
            | Ok proc ->
                serverProcess <- Some proc
                let now = DateTime.UtcNow
                state <- Some(RunningMission (now, now + TimeSpan.FromMinutes(float settings.MissionDuration)))
                logger.Debug state
                this.SaveState()
                return! this.ResumeAsync(retriesLeft - 1)
            | Error msg ->
                serverProcess <- None
                return this.Die(msg)
        }

    /// Archive current campaign, and create a new one
    member this.ResetCampaign(scenario : string) =
        async {
            // Keep player and pilots database
            if war.IsNone then
                do! this.Init() |> Async.Ignore

            let players, pilots =
                match war with
                | Some war -> war.Players, war.Pilots
                | None -> [], []

            // Stop any ongoing activity
            this.Interrupt("Reset campaign", true)

            let bakDir =
                let up = Path.GetDirectoryName(Path.GetFullPath(settings.WorkDir))
                Seq.initInfinite (fun i -> sprintf "Archived-%03d" i)
                |> Seq.map (fun dirname -> Path.Combine(up, dirname))
                |> Seq.find (fun dirname -> not(Directory.Exists(dirname)))

            let moveDir _ =
                if Directory.Exists(settings.WorkDir) then
                    try
                        Directory.Move(settings.WorkDir, bakDir)
                        Ok "Old working dir backed up"
                    with
                    | _ -> Error <| sprintf "Failed to back up working dir '%s'" settings.WorkDir
                else
                    Ok "No current campaign to backup"

            let recreateDir _ =
                try
                    Directory.CreateDirectory(settings.WorkDir) |> ignore
                    Ok "Fresh working dir created"
                with
                | _ -> Error <| sprintf "Failed to create fresh working dir '%s'" settings.WorkDir

            let prepareDir _ =
                if not(Directory.Exists(settings.WorkDir)) || not (Seq.isEmpty(Directory.EnumerateFileSystemEntries(settings.WorkDir))) then
                    moveDir()
                    |> Result.bind recreateDir
                else
                    Ok "No old campaign data to backup before reset"

            let initData _ =
                let world =
                    try
                        Init.mkWorld(scenario + ".Mission", settings.RoadsCapacity * 1.0f<M^3/H>, settings.RailsCapacity * 1.0f<M^3/H>)
                        |> Ok
                    with exc ->
                        Error (sprintf "Failed to init world: %s" exc.Message)
                // Randomize weather state
                let weatherDaysOffset = (System.Random().NextDouble() - 0.5) * 30.0
                let world =
                    world
                    |> Result.map (fun world -> { world with WeatherDaysOffset = weatherDaysOffset })
                // Load names
                let countrySuffix country =
                    match country with
                    | GreatBritain -> "Britain"
                    | Russia -> "Russia"
                    | UnitedStates -> "USA"
                    | Germany -> "Germany"
                    | Italy -> "Italy"
                let world =
                    world
                    |> Result.map (fun world ->
                        let names =
                            (PilotRanks.NameDatabase.Default, world.Countries.Keys)
                            ||> Seq.fold (fun names country ->
                                let countrySuffix = countrySuffix country
                                let firstNames = Path.Combine("Config", sprintf "FirstNames%s.txt" countrySuffix)
                                let lastNames = Path.Combine("Config", sprintf "LastNames%s.txt" countrySuffix)
                                let names =
                                    if File.Exists firstNames then
                                        names.AddFirstNamesFromFile(country, firstNames)
                                    else
                                        names
                                let names =
                                    if File.Exists lastNames then
                                        names.AddLastNamesFromFile(country, lastNames)
                                    else
                                        names
                                names
                            )
                        { world with Names = names }
                    )
                // Load ranks
                let world =
                    world
                    |> Result.map  (fun world ->
                        let ranks =
                            (Map.empty, world.Countries.Keys)
                            ||> Seq.fold (fun ranks country ->
                                let countrySuffix = countrySuffix country
                                let path = Path.Combine("Config", sprintf "Ranks%s.txt" countrySuffix)
                                if File.Exists path then
                                    let countryRanks = PilotRanks.Rank.FromFile path
                                    Map.add country countryRanks ranks
                                else
                                    ranks
                            )
                        { world with Ranks = { Ranks = ranks } }
                    )
                match world with
                | Error e ->
                    Error e
                | Ok world ->
                    let (world, sctrl : IScenarioController, saveScenarioControoler, axisPlanesFactor, alliesPlanesFactor) =
                        let planeSet =
                            match world.Map with
                            | Contains "kuban" -> WorldWar2Internal.PlaneAndUnitSet.KubanEarly
                            | Contains "moscow" -> WorldWar2Internal.PlaneAndUnitSet.Moscow
                            | Contains "rheinland" -> WorldWar2Internal.PlaneAndUnitSet.Bodenplatte
                            | Contains "stalingrad" -> WorldWar2Internal.PlaneAndUnitSet.StalingradEarly
                            | _ ->
                                logger.Warn(sprintf "No default planeset for %s" world.Map)
                                WorldWar2Internal.PlaneAndUnitSet.StalingradEarly
                        let world = planeSet.Setup world
                        let ww2 = WorldWar2(world, WorldWar2Internal.Constants.Default(world.StartDate))
                        world, upcast(ww2), (fun() -> ww2.SaveToFile(wkPath(scenarioCtrlFilename))), 1.0f, 1.0f
                    let pilots =
                        pilots
                        |> List.map (fun pilot -> Campaign.Pilots.clearFlights pilot)
                    let state0 = Init.mkWar(world, players, pilots)
                    sctrl.InitAirfields(axisPlanesFactor, Axis, state0)
                    sctrl.InitAirfields(alliesPlanesFactor, Allies, state0)
                    sctrl.InitGroundForces(1.0f, 1.0f, state0)
                    let step = sctrl.Start(state0, settings.SimulatedDuration * 1.0f<H>)
                    Ok(world, state0, step, sctrl, saveScenarioControoler)

            let writeData (world : World, state0 : WarState, step0 : ScenarioStep, sctrl : IScenarioController, saveScenarioController) =
                try
                    world.SaveToFile(wkPath worldFilename)
                    state0.SaveToFile(wkPath(getStateFilename 0))
                    step0.SaveToFile(wkPath(getStepFilename 0))
                    saveScenarioController()
                    controller <- Some sctrl
                    war <- Some state0
                    step <- Some step0
                    stateChanged.Trigger(state0)
                    Ok()
                with
                | _ ->
                    Error "Internal error: world, state or controller data not set"

            let res =
                prepareDir()
                |> Result.bind initData
                |> Result.bind writeData

            return res
        }

    /// Advance scenario to next step
    member this.Advance() =
        async {
            match state with
            | Some AdvancingScenario -> ()
            | None -> ()
            | Some _ when isRunning ->
                failwith "Cannot force advance while running"
            | _ ->
                // Force advancing
                logger.Info("State is not AdvancingScenario, forcing advance and clearing state.")
                state <- None
                this.SaveState()

            match war, controller, step with
            | Some war, Some sctrl, Some(Ongoing stepData) ->
                let random = System.Random(this.Seed)
                // Simulate missions
                let sim = Campaign.Missions.MissionSimulator(random, war, stepData.Missions, settings.SimulatedDuration * 1.0f<H>)
                let events =
                    seq {
                        yield! sim.DoAll()
                        for cmd in sctrl.NewDay(war) do
                            yield cmd
                    }
                let results =
                    events
                    |> Seq.map(fun (cmd, description) ->
                        let results =
                            cmd
                            |> Option.map (fun cmd -> cmd.Execute(war))
                            |> Option.defaultValue []
                        description, cmd, results)
                    |> List.ofSeq
                // Pilots that have recovered become healthy
                war.RefreshPilotHealths()
                // Update weather
                let weather =
                    let world = war.World
                    Campaign.Common.Weather.getWeather (System.Random(world.Seed)) (war.Date + System.TimeSpan.FromDays(world.WeatherDaysOffset))
                war.SetWeather(weather)
                // Plan next round
                let advance = sctrl.NextStep(stepData)
                let nextStep = advance(war, settings.SimulatedDuration * 1.0f<H>)
                // Write war state and campaign step files
                let index = getCurrentIndex settings.WorkDir + 1
                let stateFile = wkPath(getStateFilename index)
                let stepFile = wkPath(getStepFilename index)
                let simFile = wkPath(getSimulationFilename index)
                war.SaveToFile(stateFile)
                stateChanged.Trigger(war.Clone())
                nextStep.SaveToFile(stepFile)
                use writer = new StreamWriter(simFile)
                let json = Json.serializeEx JsonConfig.IL2Default results
                writer.Write(json)
                step <- Some nextStep
                return Ok(results)
            | _, _, Some _ ->
                return (Error "Cannot advance campaign, it has reached its final state")
            | _ ->
                return (Error "Campaign data missing")
        }

    /// Prepare mission file
    member this.PrepareMission(withBattles : bool) =
        async {
            match controller, step, war with
            | Some(ctrl), Some(Ongoing stepData), Some state ->
                let isPlayable =
                    [Axis; Allies]
                    |> Seq.forall(fun coalition ->
                        let totalFighters =
                            state.AirfieldsOfCoalition coalition
                            |> Seq.sumBy(fun af ->
                                state.GetNumPlanes(af.AirfieldId)
                                |> Map.toSeq
                                |> Seq.sumBy (fun (plane, num) -> if state.World.PlaneSet.[plane].Kind = Common.PlaneModel.PlaneType.Fighter then (max num 0.0f) else 0.0f)
                            )
                        logger.Debug(sprintf "Coalition %s has %3.0f fighters" (string coalition) totalFighters)
                        int totalFighters >= settings.MinFightersInPlayableMission)
                if not isPlayable then
                    return Ok(false)
                else
                try
                    let seed = this.Seed
                    let selection =
                        ctrl.TrySelectMissions(stepData, state, seed, 25)
                        |> Option.map (fun selection ->
                            if withBattles then
                                selection
                            else
                                selection.WithoutGroundBattles)
                    let missionPrepSettings : MissionFilePreparation.PreparationSettings =
                        {
                            MissionFilePreparation.MaxTrainsPerSide = settings.MaxTrainsPerCoalition
                            MissionFilePreparation.MaxTruckColumnsPerSide = settings.MaxTruckConvoysPerCoalition
                            MissionFilePreparation.MissionLength = TimeSpan.FromMinutes(float settings.MissionDuration)
                            MissionFilePreparation.GroundBattleLimits = settings.GroundBattleLimits
                        }
                    let missionGenSettings : MissionGenSettings =
                        {
                            MaxAiPatrolPlanes = settings.MaxActivePatrolsPerCoalition
                            MaxAntiAirCannons = settings.MaxAAGuns
                            MaxAttackPlanesCpuCost = settings.MaxAttackPlanesCpuCost
                            OutFilename = settings.MissionFilePath
                            Planes = state.World.PlaneSet.Values |> List.ofSeq
                        }
                    let mission = MissionFilePreparation.mkMultiplayerMissionContent (Random(seed)) missionPrepSettings stepData.Briefing state selection
                    mission.BuildMission(
                        Random(seed),
                        missionGenSettings,
                        state.World.Scenario,
                        state.Date,
                        state.Weather,
                        state.World.Bridges.ContainsKey,
                        state.AsMissionBuilderData(1.0f<H> * float32 missionPrepSettings.MissionLength.TotalHours))
                    return Ok(true)
                with
                e -> return (Error e.Message)
            | _ ->
                return (Error "Cannot select missions in the current state")
        }

    member this.RunMission(startTime, endTime, restartsLeft) =
        async {
            match gameServer.IsRunning serverProcess with
            | None ->
                do! this.StartServerAsync(restartsLeft)
            | Some proc ->
                serverProcess <- Some(upcast proc)

            // Find the set of log files to use. Pick the first file with suffix [0] that is newer than the mission start time.
            let tryGetLatestStartingMissionReport() =
                try
                    IO.Directory.GetFiles(settings.MissionLogs, "missionReport*.txt")
                    |> Seq.filter (fun file -> IO.Path.GetFileNameWithoutExtension(file).EndsWith("[0]") && IO.File.GetCreationTimeUtc(file) > startTime)
                    |> Seq.minBy (fun file -> IO.File.GetCreationTimeUtc(file))
                    |> fun filePath ->
                        let basename = Path.GetFileNameWithoutExtension(filePath)
                        let basename = basename.Substring(0, basename.IndexOf('['))
                        basename
                    |> Some
                with _ -> None

            let latestStartingMissionReport =
                let maxTries = 8
                let rec keepTrying(i) =
                    async {
                        match tryGetLatestStartingMissionReport() with
                        | Some x ->
                            logger.Info("Found logs " + x)
                            return Ok x
                        | None ->
                            // Warn every minute if logs have not been found yet
                            if i > 0 && i % 4 = 0 then
                                logger.Warn("Still no logs found. This can happen if the server is slow to load the mission, or if logging isn't enabled in startup.cfg.")
                            if i >= maxTries then
                                return Error "Failed to locate game logs"
                            else
                                do! Async.Sleep(15000)
                                return! keepTrying(i + 1)
                    }
                keepTrying(0)

            match! latestStartingMissionReport with
            | Error msg ->
                if restartsLeft > 0 then
                    logger.Warn("No logs found. Will attempt to restart DServer.")
                    let killed = gameServer.KillProcess(serverProcess)
                    match killed with
                    | Ok() ->
                        logger.Info("DServer successfully killed")
                    | _ ->
                        logger.Warn("Failed to kill DServer")
                    serverProcess <- None
                    let duration = endTime - startTime
                    let startTime = DateTime.UtcNow
                    let endTime = startTime + duration
                    state <- Some(RunningMission(startTime, endTime))
                    this.SaveState()
                    return! this.ResumeAsync(restartsLeft - 1)
                else
                    this.Interrupt(msg, true)
                    failwith "Failure to locate game logs"
            | Ok latestStartingMissionReport ->
            let mutable stalledTriggered = false
            let! cancelLiveReporting =
                async {
                    // Start live reporting
                    match war, gameServer with
                    | Some war, (:? IPlayerNotifier as messaging) ->
                        let war2 = war.Clone()
                        let commands =
                            let basename = latestStartingMissionReport
                            asyncSeq {
                                for logFile in WatchLogs.watchLogs settings.MissionLogs basename (TimeSpan.FromMinutes(2.0)) do
                                    match logFile with
                                    | WatchLogs.NewLogFile x ->
                                        let lines = IO.File.ReadAllLines(x)
                                        yield! AsyncSeq.ofSeq lines
                                    | WatchLogs.Stalled ->
                                        stalled.Trigger()
                                        stalledTriggered <- true
                            }
                            |> MissionResults.commandsFromLogs war2
                            |> AsyncSeq.choose snd
                        let liveReporter = LiveNotifier(commands, war2, messaging)
                        let cancellation = new Threading.CancellationTokenSource()
                        Async.Start(liveReporter.Run(), cancellation.Token)
                        // Give time to execute old commands
                        do! Async.Sleep(15000)
                        liveReporter.UnMute()
                        return
                            fun () ->
                                logger.Info("LiveNotifier terminated")
                                cancellation.Cancel()
                    | _ ->
                        logger.Warn("LiveNotifier NOT started")
                        return ignore
                }
            let! ourCancellation = Async.CancellationToken
            ourCancellation.Register(fun () -> cancelLiveReporting()) |> ignore

            // Check that DServer is running every 15s until the deadline has passed
            // If DServer dies before, restart it.
            // If DServer stalled, kill it, regenerate new mission and retry
            let rec monitor() =
                async {
                    let untilDeadLine = (endTime - DateTime.UtcNow).TotalMilliseconds
                    if untilDeadLine > 0.0 then
                        match stalledTriggered, gameServer.IsRunning serverProcess with
                        | false, None ->
                            return Error(this.StartServerAsync(restartsLeft))
                        | false, Some proc ->
                            serverProcess <- Some(upcast proc)
                            try
                                do! Async.Sleep(15000)
                                return! monitor()
                            with
                            | :? OperationCanceledException ->
                                this.Die("Interrupted")
                                return Error(async.Return())
                        | true, _ ->
                            gameServer.KillProcess(serverProcess) |> ignore
                            return Error(
                                async {
                                    // Regen mission, but without battles as they are suspected to cause stalls
                                    state <- Some(PreparingMission(false))
                                    logger.Info state
                                    this.SaveState()
                                    if not stopAfterMission then
                                        return! this.ResumeAsync(restartsLeft)
                                    else
                                        this.Die("Terminate sync after server stalled")
                                        return()
                                }
                            )
                    else
                        return Ok()
                }
            match! monitor() with
            | Error cont ->
                cancelLiveReporting()
                return! cont
            | Ok () ->
                let! _ = gameServer.SignalMissionEnd()
                cancelLiveReporting()
                state <- Some(ExtractingResults(latestStartingMissionReport + "[*].txt" ))
                logger.Info state
                this.SaveState()
                if not stopAfterMission then
                    return! this.ResumeAsync()
                else
                    this.Interrupt("Stop after mission", true)
                    this.Die("Terminate sync after mission")
                    return ()

        }

    /// Extract mission log
    member this.ExtractMissionLog(pattern : string) =
        async {
            match war with
            | Some war ->
                // Find initial log file created right after the mission was started
                let lines =
                    asyncSeq {
                        let files =
                            System.IO.Directory.EnumerateFiles(settings.MissionLogs, pattern)
                            |> Seq.sortBy (fun file -> System.IO.File.GetCreationTimeUtc(file))
                            |> List.ofSeq
                        for file in files do
                            use f = File.OpenText(file)
                            while not f.EndOfStream do
                                let! line = Async.AwaitTask(f.ReadLineAsync())
                                yield line
                    }
                let commands = MissionResults.commandsFromLogs war lines
                let! effects =
                    asyncSeq {
                        for description, command in commands do
                            logger.Info(description)
                            match command with
                            | Some command ->
                                try
                                    logger.Debug("Command from game logs: " + Json.serialize command)
                                with exc ->
                                    logger.Debug("Command from game logs.")
                                    logger.Debug(exc)
                                let effects = command.Execute(war)
                                yield (description, Some command, effects)
                            | None ->
                                yield (description, None, [])
                    }
                    |> AsyncSeq.toArrayAsync
                stateChanged.Trigger(war.Clone())
                // Write effects to file
                // Write war state and campaign step files
                let index = getCurrentIndex settings.WorkDir + 1
                let effectsFile = wkPath(getEffectsFilename index)
                use writer = new StreamWriter(effectsFile, false)
                let json = Json.serializeEx JsonConfig.IL2Default effects
                writer.Write(json)
                return Ok()
            | None ->
                return (Error "No war state")
        }

    /// Prepare to advance campaign without running a mission
    member this.SkipMission() =
        async {
            let index = getCurrentIndex settings.WorkDir + 1
            let effectsFile = wkPath(getEffectsFilename index)
            use writer = new StreamWriter(effectsFile, false)
            let effects : (string * WarStateUpdate.Commands option * WarStateUpdate.Results list)[] = [||]
            let json = Json.serializeEx JsonConfig.IL2Default effects
            writer.Write(json)
            return Ok()
        }

    /// Check current state and act accordingly
    member this.ResumeAsync(?restartsLeft, ?skipsLeft) =
        let restartsLeft = defaultArg restartsLeft maxRetries
        let skipsLeft = defaultArg skipsLeft 5
        async {
            try
                logger.Trace restartsLeft
                match state with
                | Some(ErrorState(msg, _)) ->
                    return this.Die("In error state: " + msg)
                | None
                | Some(PreparingMission _)->
                    let withBattles =
                        match state with
                        | Some(PreparingMission x) -> x
                        | _ -> true
                    let! status =
                        async {
                            try
                                let timeout = 5 * 60 * 1000 // 5 minutes
                                let! child = Async.StartChild (this.PrepareMission(withBattles), timeout)
                                return! child
                            with _ ->
                                return Error "Failed to prepare mission in time"
                        }
                    match status with
                    | Ok(true) ->
                        state <- Some ResavingMission
                        logger.Info state
                        this.SaveState()
                        return! this.ResumeAsync()
                    | Ok(false) ->
                        if skipsLeft > 0 then
                            state <- Some SkippingMission
                            logger.Info state
                            this.SaveState()
                            return! this.ResumeAsync(skipsLeft = skipsLeft - 1)
                        else
                            gameServer.KillProcess(serverProcess) |> ignore
                            return this.Die("Reached maximum number of missions skipped in a row")
                    | Error msg ->
                        gameServer.KillProcess(serverProcess) |> ignore
                        return this.Die(msg)

                | Some ResavingMission ->
                    match! gameServer.CopyRenameMission settings.MissionFile with
                    | Error msg ->
                        return this.Die(msg)
                    | Ok() ->
                    let! resavers =
                        Async.Sequential(
                            [ gameServer.ResaveMission settings.MissionFile
                              gameServer.ResaveMission settings.AltMissionFile ])
                    match resavers with
                    | [| Error _; Error _ |] ->
                        // It's not unusual for one of the alternatives to fail, e.g. because DServer is locking one of the files
                        // Fail if both resavings failed.
                        return this.Die("Resaving failed")
                    | _ ->
                    match gameServer.IsRunning serverProcess with
                    | Some proc ->
                        serverProcess <- Some(upcast proc)
                        let! s = gameServer.RotateMission()
                        logger.Debug s
                        match s with
                        | Ok() ->
                            let now = DateTime.UtcNow
                            state <- Some(RunningMission (now, now + TimeSpan.FromMinutes(float settings.MissionDuration)))
                            logger.Info state
                            this.SaveState()
                            return! this.ResumeAsync()
                        | Error msg ->
                            logger.Warn(msg)
                            gameServer.KillProcess(serverProcess) |> ignore
                            return! this.StartServerAsync(restartsLeft - 1)
                    | None ->
                        return! this.StartServerAsync()

                | Some(RunningMission(startTime, endTime)) ->
                    return! this.RunMission(startTime, endTime, restartsLeft)

                | Some(ExtractingResults(pattern)) ->
                    let! status = this.ExtractMissionLog(pattern)
                    match status with
                    | Ok() ->
                        state <- Some AdvancingScenario
                        logger.Info state
                        this.SaveState()
                        return! this.ResumeAsync()
                    | Error msg ->
                        // Result extraction failed, stop sync.
                        return this.Die(msg)

                | Some(SkippingMission) ->
                    let! status = this.SkipMission()
                    match status with
                    | Ok() ->
                        state <- Some AdvancingScenario
                        logger.Info state
                        this.SaveState()
                        return! this.ResumeAsync(skipsLeft = skipsLeft)
                    | Error msg ->
                        // Should not happen, as skipping is expected to succeed.
                        return this.Die(msg)

                | Some AdvancingScenario ->
                    let! status = this.Advance()
                    match status with
                    | Ok _ ->
                        state <- Some(PreparingMission true)
                        logger.Info state
                        this.SaveState()
                        return! this.ResumeAsync(skipsLeft = skipsLeft)
                    | Error msg ->
                        // Scenario failed, stop sync.
                        return this.Die(msg)
            with exc ->
                state <- Some(ErrorState(exc.Message, state))
                logger.Error("Sync interrupted because of uncaught exception")
                logger.Error(exc)
                this.SaveState()
                return this.Die("Error: " + exc.Message)
        }

    /// Token to use with ResumeAsync in order for Interrupt to work.
    member this.CancellationToken = cancellation.Token

    /// Start synchronization and resume flow, unless already running.
    member this.Resume() =
        if not isRunning then
            isRunning <- true
            Async.StartImmediate(this.ResumeAsync(), this.CancellationToken)

    /// Change the error state to a non-error one, going back in the workflow
    member this.ResolveError() =
        if not isRunning then
            let state2 =
                match state with
                | Some(ErrorState(_, None)) | Some(ErrorState(_, Some(ErrorState _))) ->
                    // Go back
                    Some(PreparingMission true)
                | Some(RunningMission _)
                | Some(ErrorState(_, Some (RunningMission _))) ->
                    // Retry
                    let startTime = DateTime.UtcNow
                    let endTime = startTime + TimeSpan(settings.MissionDuration / 60, settings.MissionDuration % 60, 0)
                    Some (RunningMission(startTime, endTime))
                | Some(ErrorState(_, _)) ->
                    // Go back
                    Some(PreparingMission true)
                | Some _ ->
                    // Not an error state, keep it
                    state
                | None ->
                    // No state, set to first state in the flow.
                    Some(PreparingMission true)
            logger.Info("Resolved error state to " + string state2)
            state <- state2
            this.SaveState()
            Ok ("New state is " + (state2 |> Option.map (fun s -> s.Description) |> Option.defaultValue ""))
        else
            Error "Cannot change state while sync is active"

    member this.ModifyPlayer(playerId : string, update) =
        match war with
        | Some war ->
            match war.TryGetPlayer(playerId) with
            | Some player ->
                async {
                    let player2 = update player
                    war.UpdatePlayer(player2)
                    return Ok player2
                }
            | None ->
                async.Return(Error "No such player")
        | None ->
            async.Return(Error "No campaign data")


    /// Create a game server controller and a Sync.
    static member Create(settings : Settings, ?logger) =
        let sync =
            logger
            |> Option.map(fun logger -> new Sync(settings, new RConGameServerControl(settings, logger), logger))
            |> Option.defaultWith (fun () -> new Sync(settings, new RConGameServerControl(settings)))
        sync

    member this.Dispose() =
        cancellation.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()