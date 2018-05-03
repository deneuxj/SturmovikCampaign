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

/// Campaign monitoring exposed to SturmovikServerControl
namespace Campaign.ServerControlPlugin

open CampaignServerControl.Api

open System
open System.Diagnostics
open System.IO
open MBrace.FsPickler
open Campaign.BasicTypes
open Campaign.Run
open Campaign.Configuration
open Util
open Campaign.WebHook
open Campaign.Commenting
open Campaign.WorldDescription
open Campaign.PlaneModel
open Campaign.WorldState
open Campaign
open Campaign.PlayerDiscipline

module Support =
    open ploggy
    open System.Numerics
    open Util.Async

    let private logger = NLog.LogManager.GetCurrentClassLogger()

    let findRunningServers(config) =
            let procs =
                Process.GetProcessesByName("DServer")
                |> Array.filter (fun proc -> Path.GetFullPath(Path.GetDirectoryName(proc.MainModule.FileName)).StartsWith(Path.GetFullPath(config.ServerBinDir)))
            procs

    let killServer(config, runningProc : Process option) =
        let procToKill = runningProc |> Option.filter (fun proc -> not proc.HasExited)
        let procsToKill =
            match procToKill with
            | None ->
                findRunningServers(config)
            | Some running ->
                [| running |]
        for running in procsToKill do
            logger.Info(sprintf "Killing DServer process [%d]..." running.Id)
            try
                running.Kill()
            with
            | e ->
                logger.Warn(sprintf "Failed to kill DServer.exe: %s" e.Message)

    let startServer(config) =
        // Start DServer with given SDS file.
        try
            let exePath = Path.Combine(config.ServerBinDir, "game", "DServer.exe")
            let sdsPath = Path.Combine(config.ServerDataDir, config.ServerSdsFile)
            logger.Info(sprintf "Will start server '%s' with arg '%s'" exePath sdsPath)
            let si = ProcessStartInfo(exePath, sdsPath)
            si.WorkingDirectory <- Path.GetDirectoryName(exePath)
            si.UseShellExecute <- false
            let proc = Process.Start(si)
            try
                proc.PriorityClass <- ProcessPriorityClass.High
            with
            | _ -> logger.Warn("Failed to raise priority of DServer to High")
            logger.Info(sprintf "%s [%d] started." proc.ProcessName proc.Id)
            Some proc
        with
        | e ->
            logger.Error(sprintf "Failed to start DServer.exe: %s" e.Message)
            None

    type ExecutionState =
        | DecideOrders
        | GenerateMission
        | KillOrSkip
        | KillServer
        | StartServer
        | WaitForMissionEnd of System.DateTime
        | ExtractResults
        | CampaignOver of victorious: CoalitionId
        | Failed of message:string * stackTrace:string option * ExecutionState
    with
        member this.Description =
            match this with
            | KillOrSkip -> "next mission"
            | KillServer -> "kill server"
            | DecideOrders -> "decide orders"
            | GenerateMission -> "generate mission"
            | StartServer -> "start server"
            | WaitForMissionEnd _ -> "wait for mission end"
            | ExtractResults -> "extract results"
            | CampaignOver _ -> "terminate campaign"
            | Failed(_, _, inner) -> sprintf "failed to %s" inner.Description
        member this.GetAsync(support : SupportApis, config, serverProc : Process option, announceResults, announceWeather, announceWorldState, updateMap) =
            let tryOrNotifyPlayers errorMessage onError action =
                async {
                    let! result = Async.tryTask action
                    match result with
                    | Result.Error e ->
                        do! support.ServerControl.MessageAll errorMessage
                        do! Async.Sleep(5000)
                        onError()
                        raise(Exception("See inner exception", e))
                    | _ ->
                        ()
                    return
                        match result with
                        | Result.Ok x -> x
                        | Result.Error _ -> failwith "Unreachable"
                }
            match this with
            | DecideOrders ->
                async {
                    support.Logging.LogInfo "Deciding orders..."
                    Campaign.Run.OrderDecision.run config
                    return serverProc, GenerateMission
                }
            | GenerateMission ->
                async {
                    support.Logging.LogInfo "Generate mission..."
                    do!
                        tryOrNotifyPlayers
                            ["Failed to generate next mission"]
                            (fun() -> killServer(config, serverProc))
                            (Campaign.Run.MissionFileGeneration.run config)
                    let! ok =
                        tryTask(
                            tryOrNotifyPlayers
                                ["Failed to publish next mission file"
                                 "Will retry after shutting down server"]
                                (fun() -> killServer(config, serverProc))
                                (Campaign.Run.MissionFileGeneration.publish config))
                    match ok with
                    | Error _ ->
                        killServer(config, serverProc)
                        do! Async.Sleep(5000)
                        do! Campaign.Run.MissionFileGeneration.publish config
                    | Ok _ ->
                        ()
                    do! support.ServerControl.MessageAll ["Next mission ready"]
                    return serverProc, KillOrSkip
                }
            | KillOrSkip ->
                async {
                    support.Logging.LogInfo "Next mission..."
                    match serverProc with
                    | Some proc when DateTime.Now - proc.StartTime >= TimeSpan(12, 0, 0) ->
                        support.Logging.LogInfo "Server process will be killed"
                        do!
                            [ "Server is restarting before next mission start"
                              "Wait a minute, then join again from server list to keep playing"
                            ]
                            |> support.ServerControl.MessageAll
                        do! Async.Sleep(15000)
                        return serverProc, KillServer
                    | None ->
                        support.Logging.LogInfo "Server process could not be found"
                        return None, StartServer
                    | _ ->
                        do! support.ServerControl.MessageAll ["Rotating missions now"]
                        do! support.ServerControl.SkipMission
                        let expectedMissionEnd = System.DateTime.UtcNow + System.TimeSpan(600000000L * int64 config.MissionLength)
                        return serverProc, WaitForMissionEnd expectedMissionEnd
                }
            | KillServer ->
                async {
                    support.Logging.LogInfo "Kill server..."
                    killServer(config, serverProc)
                    return None, StartServer
                }
            | StartServer ->
                async {
                    support.Logging.LogInfo "Start server..."
                    let serverProc = startServer(config)
                    match serverProc with
                    | None ->
                        return None, Failed("Failed to start server", None, this)
                    | Some _ ->
                        let expectedMissionEnd = System.DateTime.UtcNow + System.TimeSpan(600000000L * int64 config.MissionLength)
                        return serverProc, WaitForMissionEnd expectedMissionEnd
                }
            | WaitForMissionEnd time ->
                async {
                    support.Logging.LogInfo "Check if mission is over..."
                    if System.DateTime.UtcNow >= time then
                        return serverProc, ExtractResults
                    else
                        return serverProc, WaitForMissionEnd time
                }
            | ExtractResults ->
                async {
                    support.Logging.LogInfo "Extract results..."
                    do!
                        [ "Mission has ended"
                          "Actions past this point will not affect the campaign"
                          "Note: personal stats keep being collected until the round ends"
                          "Note: friendly fire and wrecking are still being watched and penalized"
                        ]
                        |> support.ServerControl.MessageAll
                    let! missionLogEntries =
                        tryOrNotifyPlayers
                            [ "Bad news, mission log entries were not found"
                              "Campaign is now halted"
                              "Sorry for the inconvenience" ]
                            (fun() -> killServer(config, serverProc))
                            (async { return Campaign.Run.MissionLogParsing.stage0 config })
                    let! missionResults =
                        tryOrNotifyPlayers
                            [ "Bad news, result extraction failed"
                              "Campaign is now halted"
                              "Sorry for the inconvenience" ]
                            (fun() -> killServer(config, serverProc))
                            (async { return Campaign.Run.MissionLogParsing.stage1(config, missionLogEntries) })
                    Campaign.Run.MissionLogParsing.backupFiles config
                    support.Logging.LogInfo "Make weather..."
                    let date = Campaign.Run.WeatherComputation.getNextDateFromState config
                    let weather = Campaign.Run.WeatherComputation.run(config, date)
                    announceWeather weather
                    Campaign.Run.MissionLogParsing.updateHangars(config, missionResults, missionLogEntries)
                    let! updatedState =
                        tryOrNotifyPlayers
                            [ "Bad news, campaign update failed"
                              "Campaign is now halted"
                              "Sorry for the inconvenience" ]
                            (fun() -> killServer(config, serverProc))
                            (async { return Campaign.Run.MissionLogParsing.updateState(config, missionResults) })
                    let newProduction, battleResults, ((oldState, newState) as states) = updatedState
                    let world =
                        try
                            let serializer = FsPickler.CreateXmlSerializer(indent = true)
                            use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                            serializer.Deserialize<Campaign.WorldDescription.World>(worldFile)
                        with
                        | e ->
                            killServer(config, serverProc)
                            failwithf "Failed to read world data. Reason was: '%s'" e.Message
                    match newState.VictoriousSide(world) with
                    | Some Allies ->
                        do!
                            [ "Campaign is over"
                              "Allies are victorious"
                            ]
                            |> support.ServerControl.MessageAll
                        return serverProc, CampaignOver(Allies)
                    | Some Axis ->
                        do!
                            [ "Campaign is over"
                              "Axis is victorious"
                            ]
                            |> support.ServerControl.MessageAll
                        return serverProc, CampaignOver(Axis)
                    | None ->
                        do!
                            [ "Campaign continues"
                              "Next mission is being generated..."
                            ]
                            |> support.ServerControl.MessageAll
                        let axisAAR, alliesAAR = Campaign.Run.MissionLogParsing.buildAfterActionReports(config, oldState, newState, missionResults.TakeOffs, missionResults.Landings, missionResults.StaticDamages @ missionResults.VehicleDamages, newProduction)
                        Campaign.Run.MissionLogParsing.stage2 config (oldState, newState, axisAAR, alliesAAR, battleResults)
                        announceResults(axisAAR, alliesAAR, battleResults)
                        announceWorldState(world, newState)
                        updateMap(world, newState)
                        return serverProc, DecideOrders
                }
            | CampaignOver _ ->
                async {
                    support.Logging.LogInfo "Cannot continue campaign that is over."
                    return serverProc, KillServer
                }
            | Failed(msg, stackTrace, state) ->
                async {
                    support.Logging.LogInfo "Failed!"
                    return serverProc, this
                }

        member this.Save(config) =
            let serializer = FsPickler.CreateXmlSerializer(indent = true)
            let statusFile = Path.Combine(config.OutputDir, "loopState.xml")
            if File.Exists(statusFile) then
                File.Delete(statusFile)
            use s = File.CreateText(statusFile)
            serializer.Serialize(s, this)

        static member Restore(config) =
            let serializer = FsPickler.CreateXmlSerializer(indent = true)
            let statusFile = Path.Combine(config.OutputDir, "loopState.xml")
            if File.Exists(statusFile) then
                use s = File.OpenText(statusFile)
                serializer.Deserialize<ExecutionState>(s)
            else
                GenerateMission

    let loadWorldThenDo (support : SupportApis, config) action =
        try
            let serializer = FsPickler.CreateXmlSerializer(indent = true)
            use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
            use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
            let world = serializer.Deserialize<Campaign.WorldDescription.World>(worldFile)
            let state = serializer.Deserialize<Campaign.WorldState.WorldState>(stateFile)
            action(world, state)
        with
        | _ -> support.Logging.LogError("Failed to generate situational map")

    let start(support : SupportApis, config, status, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage, updateMap) =
        let rec work (status : ExecutionState) (serverProc : Process option) =
            match status with
            | Failed(msg, _, _) ->
                support.Logging.LogInfo(sprintf "Execution aborted due to failure: %s" msg)
                status.Save(config)
                postMessage "Hey @coconut something went wrong, check the server"
                NoTask
            | CampaignOver(victorious) ->
                match victorious with
                | Axis -> "Axis has won"
                | Allies -> "Allies have won"
                |> sprintf "Campaign is over, %s the battle!"
                |> support.Logging.LogInfo
                killServer(config, serverProc)
                status.Save(config)
                onCampaignOver victorious
                NoTask
            | _ ->
                let step action =
                    async {
                        try
                            let! proc, (status : ExecutionState) = action
                            status.Save(config)
                            match status with
                            | WaitForMissionEnd time ->
                                return SomeTask(time, async { return work status proc }, status.Description)
                            | _ ->
                                return ScheduledTask.SomeTaskNow status.Description (async { return work status proc })
                        with
                        | exc ->
                            let exc =
                                match exc.InnerException with
                                | null -> exc
                                | inner -> inner
                            return work (Failed(exc.Message, Some exc.StackTrace, status)) None
                    }
                loadWorldThenDo(support, config) updateMap
                let action = status.GetAsync(support, config, serverProc, announceResults, announceWeather, announceWorldState, updateMap)
                ScheduledTask.SomeTaskNow "next campaign state" (step action)

        let status =
            status
            |> Option.defaultVal (ExecutionState.Restore(config))
        // If the status was failed, plan to retry the operation that failed.
        let status =
            match status with
            | Failed(msg, _, ExtractResults) ->
                support.Logging.LogInfo "Previously failed to extract results, will restart the mission"
                GenerateMission
            | Failed(msg, _, status) ->
                support.Logging.LogInfo(sprintf "Retry after failure '%s'" msg)
                status
            | _ ->
                status
        // If the saved state was waiting, check if we got back before the expected end time and the server is running
        let status =
            match status with
            | WaitForMissionEnd time ->
                if System.DateTime.UtcNow < time then
                    if findRunningServers(config).Length > 0 then
                        support.Logging.LogInfo "Resume waiting"
                        status // deadline not passed, and server running -> resume waiting
                    else
                        support.Logging.LogInfo "Restart server"
                        GenerateMission // deadline not passed, no server running -> regenerate mission and restart
                else
                    support.Logging.LogInfo "Extract results"
                    ExtractResults // deadline passed -> extract results
            | _ ->
                support.Logging.LogInfo "Resume"
                status
        let proc =
            match findRunningServers(config) with
            | [| proc |] -> Some proc
            | _ -> None
        work status proc

    let reset(support : SupportApis, config : Configuration, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage, updateMap) =
        async {
            // Delete log and campaign files
            let logDir = Path.Combine(config.ServerDataDir, "logs")
            let logs = List.ofSeq(Directory.EnumerateFiles(logDir, "*.txt"))
            let campaignFiles = List.ofSeq(Directory.EnumerateFiles(config.OutputDir))
            let! result = Async.keepTryingPaced 10 6 (fun file -> if File.Exists(file) then File.Delete(file)) (logs @ campaignFiles)
            match result with
            | Ok _ -> ()
            | Error(_, files) -> failwithf "Failed to delete: %s" (String.concat ", " files)
            // Initial campaign state
            let startDate, _ = Campaign.Run.Init.createWorld config
            ignore <| Campaign.Run.WeatherComputation.run(config, startDate)
            ignore <| Campaign.Run.Init.createState config
            Campaign.Run.OrderDecision.run config
            loadWorldThenDo (support, config) updateMap
            // Start campaign
            return start(support, config, Some GenerateMission, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage, updateMap)
        }
        |> ScheduledTask.SomeTaskNow "generate mission"

    /// Build a graphical representation of the strategic situation
    let mkMapGraphics(world : World, state : WorldState) : MapGraphics.MapPackage =
        let sg = state.FastAccess
        let wg = world.FastAccess
        let icons : MapGraphics.MapIcon list =
            [
                for reg, regState in List.zip world.Regions state.Regions do
                    match regState.Owner with
                    | Some coalition ->
                        let numTanks = regState.NumVehicles |> Map.toSeq |> Seq.sumBy snd
                        let needs = world.RegionAmmoCost(reg.RegionId)
                        yield {
                            Position = reg.Position
                            Icon = MapGraphics.Base
                            Color = if coalition = Axis then MapGraphics.Gray else MapGraphics.Red
                            Label = Some (sprintf "%s (%1.0f%%)" (string reg.RegionId) (100.0f * regState.Supplies / needs))
                            Description = Some (sprintf "%d tanks" numTanks)
                            Depth = 0.0f
                        }
                    | None ->
                        ()
                for bf, defending in Campaign.Battlefield.identifyBattleAreas world state do
                    let bf = wg.GetAntiTankDefenses(bf)
                    yield {
                        Position = bf.Position.Pos
                        Icon = MapGraphics.Clash
                        Color = if defending = Axis then MapGraphics.Red else MapGraphics.Gray
                        Label = None
                        Description = None
                        Depth = 0.0f
                    }
                for af, afState in List.zip world.Airfields state.Airfields do
                    match sg.GetRegion(af.Region).Owner with
                    | Some coalition ->
                        let numPlanes = afState.NumPlanes |> Map.toSeq |> Seq.sumBy snd
                        yield {
                            Position = af.Pos
                            Icon = MapGraphics.Airfield
                            Color = if coalition = Axis then MapGraphics.Gray else MapGraphics.Red
                            Label = None
                            Description = Some (sprintf "%1.0f Kg %d planes" (afState.Supplies / bombCost) (int numPlanes))
                            Depth = 0.0f
                        }
                    | None ->
                        ()
            ]
        let areas : MapGraphics.MapArea list =
            [
                let segments = MapGraphics.Segment.CreateSegments(world, state)
                let loops = MapGraphics.Segment.MakeLoops(state, segments)
                for loop in loops do
                    let color =
                        match loop with
                        | x :: _ ->
                            match sg.GetRegion(x.Region).Owner with
                            | Some Axis -> Some(Vector3(0.5f, 0.5f, 0.5f))
                            | Some Allies -> Some(Vector3(1.0f, 0.0f, 0.0f))
                            | None -> None
                        | [] ->
                            None
                    match color with
                    | Some color ->
                        let boundary =
                            [
                                for segment in loop do
                                    yield fst segment.Edge
                                    yield snd segment.Edge
                            ]
                        yield { Boundaries = boundary; Color = color }
                    | None ->
                        ()
            ]
        let map =
            match world.Map with
            | Contains "stalingrad" -> MapGraphics.Stalingrad
            | Contains "moscow" -> MapGraphics.Moscow
            | Contains "vluki" -> MapGraphics.VelikieLuki
            | Contains "kuban" -> MapGraphics.Kuban
            | _ -> MapGraphics.Stalingrad
        { MapGraphics.MapPackage.Default with
            Name = map
            Icons = icons
            Areas = areas
        }


type Plugin() =
    let mutable support : SupportApis option = None
    let mutable webHookClient : (System.Net.WebClient * System.Uri) option = None
    let mutable commenter : CommentatorRestarter option = None
    let mutable queue = startQueue()
    let logger = NLog.LogManager.GetCurrentClassLogger()

    let onCampaignOver victors =
        match webHookClient with
        | Some hook -> onCampaignOver (queue, hook) victors
        | None -> ()

    let announceResults results =
        match webHookClient with
        | Some hook -> onMissionEnd (queue, hook) results
        | None -> ()

    let announceWeather weather =
        match webHookClient with
        | Some hook -> postWeatherReport (queue, hook) weather
        | None -> ()

    let postMessage message =
        match webHookClient with
        | Some hook -> postMessage (queue, hook) message
        | None -> ()

    let announceToTeam (coalition : CoalitionId, messages : string list) =
        match support with
        | Some support ->
            async {
                let team =
                    match coalition with
                    | Axis -> support.ServerControl.GetAxisTeam()
                    | Allies -> support.ServerControl.GetAlliesTeam()
                return! support.ServerControl.MessageTeam(team, messages)
            }
        | None ->
            async.Zero()

    let announceTakeOffToTeam (player : string, coalition : CoalitionId, airfield : AirfieldId, afCoalition : CoalitionId option, plane : PlaneModel, cargo : float32<K>) =
        match support with
        | Some support ->
            async {
                let atEnemy = Some coalition.Other = afCoalition
                let cargo =
                    if cargo > 0.0f<K> then
                        sprintf " with %3.0fkg of cargo" cargo
                    else
                        ""
                let planeType =
                    match plane.PlaneType with
                    | PlaneType.Attacker -> "an attacker"
                    | PlaneType.Bomber -> "a bomber"
                    | PlaneType.Fighter -> "a fighter"
                    | PlaneType.Transport -> "a transport plane"
                let message =
                    let verb =
                        if atEnemy then
                            "escaped"
                        else
                            "took off"
                    sprintf "%s %s from %s in %s%s"
                        player
                        verb
                        airfield.AirfieldName
                        planeType
                        cargo
                let team =
                    match coalition with
                    | Axis -> support.ServerControl.GetAxisTeam()
                    | Allies -> support.ServerControl.GetAlliesTeam()
                if atEnemy then
                    return! support.ServerControl.MessageAll([message])
                else
                    return! support.ServerControl.MessageTeam(team, [message])
            }
        | None ->
            async { return () }

    let announceLandingToTeam (player : string, coalition : CoalitionId, airfield : AirfieldId, afCoalition : CoalitionId option, plane : PlaneModel, cargo : float32<K>, health : float32, damageInflicted : float32<E>) =
        match support with
        | Some support ->
            async {
                let team =
                    match coalition with
                    | Axis -> support.ServerControl.GetAxisTeam()
                    | Allies -> support.ServerControl.GetAlliesTeam()
                let atEnemy = Some coalition.Other = afCoalition
                let msg =
                    if not atEnemy then
                        if health = 0.0f then
                            sprintf "%s \"landed\" back at %s" player airfield.AirfieldName
                        else
                            let cargo =
                                if cargo > 0.0f<K> then
                                    sprintf " with %3.0fkg of cargo" cargo
                                else
                                    ""
                            let intro =
                                if damageInflicted = 0.0f<E> then
                                    sprintf "%s is back at %s" player airfield.AirfieldName
                                elif damageInflicted < GroundAttackVehicle.LightArmorCost then
                                    sprintf "%s is welcomed back on the ground at %s" player airfield.AirfieldName
                                elif damageInflicted < GroundAttackVehicle.MediumTankCost then
                                    sprintf "%s's return to %s is celebrated" player airfield.AirfieldName
                                else
                                    sprintf "the entire base rushes to welcome %s at %s" player airfield.AirfieldName
                            let difficulty =
                                if health = 1.0f then
                                    ""
                                elif health > 0.9f then
                                    " after a difficult mission"
                                elif health > 0.1f then
                                    " after a dangerous mission"
                                else
                                    " after narrowly escaping death"
                            intro + difficulty + cargo
                    else
                        if damageInflicted = 0.0f<E> then
                            if cargo > 0.0f<K> then
                                sprintf "The enemy welcomes %s and his cargo" player
                            elif health > 0.9f then
                                sprintf "%s has defected to the enemy at %s" player airfield.AirfieldName
                            else
                                sprintf "The enemy has captured %s and what's left of their plane at %s" player airfield.AirfieldName
                        else
                            sprintf "%s receives stern looks after being captured at %s" player airfield.AirfieldName
                if atEnemy then
                    return! support.ServerControl.MessageAll([msg])
                else
                    return! support.ServerControl.MessageTeam(team, [msg])
            }
        | None ->
            async {
                return()
            }

    let announceBattleKillsExceeded (region : string, coalition : CoalitionId) =
        async {
            let msg = sprintf "Max battle damage inflicted at %s, further kills will be ignored" region
            return! announceToTeam(coalition, [msg])
        }

    let getPlayerId(user : UserIds) =
        match support with
        | Some support ->
            async {
                let! players =
                    support.ServerControl.GetPlayerList
                let player =
                    players
                    |> Seq.tryFind (fun p -> p.GetName() = user.Name)
                return player
            }
        | None ->
            async.Return None

    let informPlayer(user : UserIds, messages : string list) =
        match support with
        | Some support ->
            async {
                let! player = getPlayerId user
                match player with
                | Some player ->
                    do! support.ServerControl.MessagePlayer(player, messages)
                | None ->
                    ()
            }
        | None ->
            async.Zero()

    let punishPlayer (penalty : Judgement) =
        match support with
        | Some support ->
            async {
                match penalty.Decision with
                | Informed txt ->
                    logger.Info(sprintf "Warn player '%s': '%s'" penalty.Player.Name txt)
                    do! informPlayer(penalty.Player, [txt])
                    do! Async.Sleep(5000)
                | Banned hours ->
                    logger.Info(sprintf "Ban player '%s' for %d hours" penalty.Player.Name hours)
                    do! support.ServerControl.MessageAll([sprintf "%s was banned for %d hours" penalty.Player.Name hours])
                    do! Async.Sleep(5000)
                    let! player = getPlayerId penalty.Player
                    match player with
                    | Some player ->
                        do! support.ServerControl.BanPlayer(player, hours)
                    | None ->
                        ()
                | Kicked ->
                    do! support.ServerControl.MessageAll([sprintf "%s was kicked" penalty.Player.Name])
                    do! Async.Sleep(5000)
                    let! player = getPlayerId penalty.Player
                    match player with
                    | Some player ->
                        do! support.ServerControl.KickPlayer(player)
                    | None ->
                        ()
            }
        | None ->
            async.Zero()

    let announceWorldState arg =
        match webHookClient with
        | Some hook -> postWorldState (queue, hook) arg
        | None -> ()

    let updateMap arg =
        match support with
        | Some support ->
            support.MapGraphics.SetPackage(Support.mkMapGraphics(arg))
        | None ->
            ()

    member x.StartWebHookClient(config : Configuration) =
        let webHookUri = config.WebHook
        // Create WebClient for web hook, if not already done
        match webHookClient with
        | None ->
            if not(String.IsNullOrEmpty(webHookUri)) then
                webHookClient <- Some(createClient(webHookUri))
                logger.Info("WebHook client created")
        | Some _ ->
            ()

    member x.StopWebHookClient() =
        match webHookClient with
        | None ->
            ()
        | Some(x, _) ->
            x.Dispose()

    member x.StartCommenter(config : Configuration) =
        // Stop commenter
        match commenter with
        | Some commenter ->
            commenter.Dispose()
        | None ->
            ()
        // (Re-)start commenter
        let handlers =
            { OnTookOff = announceTakeOffToTeam
              OnLanded = announceLandingToTeam
              OnMaxBattleDamageExceeded = announceBattleKillsExceeded
              OnPlayerPunished = punishPlayer
              OnMessagesToCoalition = announceToTeam
              OnMessagesToPlayer = informPlayer
            }
        commenter <- Some(new CommentatorRestarter(config, handlers, fun() -> x.StartWebHookClient(config)))
        logger.Info("Commenter set")

    member x.StopCommenter() =
        match commenter with
        | Some commenter ->
            commenter.Dispose()
        | None ->
            ()

    interface CampaignServerApi with
        member x.Init(apis) =
            support <- Some apis
            apis.Logging.LogInfo(sprintf "SturmovikCampaign %s" Campaign.AssemblyInfo.Constants.version)

        member x.StartOrResume configFile =
            let support =
                match support with
                | None -> invalidOp "Must call Init first"
                | Some x -> x
            try
                let config = loadConfigFile configFile
                x.StartWebHookClient(config)
                x.StartCommenter(config)
                Support.start(support, config, None, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage, updateMap)
                |> Choice1Of2
            with
            | e ->
                sprintf "Failed to start or resume campaign: '%s'" e.Message
                |> Choice2Of2

        member x.Reset configFile =
            let support =
                match support with
                | None -> invalidOp "Must call Init first"
                | Some x -> x
            try
                let config = loadConfigFile configFile
                x.StopCommenter()
                x.StopWebHookClient()
                let task = Support.reset(support, config, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage, updateMap)
                let task =
                    task.ContinueWith(fun nextTask ->
                        async {
                            x.StartWebHookClient(config)
                            x.StartCommenter(config)
                            return nextTask
                        })
                Choice1Of2 task
            with
            | e ->
                sprintf "Failed to reset campaign: '%s'" e.Message
                |> Choice2Of2
