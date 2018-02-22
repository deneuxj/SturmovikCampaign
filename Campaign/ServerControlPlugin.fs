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
        member this.GetAsync(support : SupportApis, config, serverProc : Process option, announceResults, announceWeather, announceWorldState) =
            let tryOrNotifyPlayers errorMessage action =
                async {
                    let result =
                        try
                            action()
                            |> Choice1Of2
                        with
                        | e -> Choice2Of2 e
                    match result with
                    | Choice2Of2 e ->
                        do! support.ServerControl.MessageAll errorMessage
                        do! Async.Sleep(5000)
                        raise(Exception("See inner exception", e))
                    | _ ->
                        ()
                    return
                        match result with
                        | Choice1Of2 x -> x
                        | Choice2Of2 _ -> failwith "Unreachable"
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
                    let exitStatus = Campaign.Run.MissionFileGeneration.run config
                    if exitStatus <> 0 then
                        do! support.ServerControl.MessageAll ["Failed to generate next mission"]
                        return serverProc, Failed(sprintf "Resaver failed, exit status %d" exitStatus, None, this)
                    else
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
                        try
                            tryOrNotifyPlayers
                                [ "Bad news, mission log entries were not found"
                                  "Campaign is now halted"
                                  "Sorry for the inconvenience" ]
                                (fun() -> Campaign.Run.MissionLogParsing.stage0 config)
                        with
                        | exc ->
                            killServer(config, serverProc)
                            raise exc
                    let! missionResults =
                        try
                            tryOrNotifyPlayers
                                [ "Bad news, result extraction failed"
                                  "Campaign is now halted"
                                  "Sorry for the inconvenience" ]
                                (fun() -> Campaign.Run.MissionLogParsing.stage1(config, missionLogEntries))
                        with
                        | exc ->
                            killServer(config, serverProc)
                            raise exc
                    Campaign.Run.MissionLogParsing.backupFiles config
                    support.Logging.LogInfo "Make weather..."
                    let date = Campaign.Run.WeatherComputation.getNextDateFromState config
                    let weather = Campaign.Run.WeatherComputation.run(config, date)
                    announceWeather weather
                    let! updatedState =
                        tryOrNotifyPlayers
                            [ "Bad news, campaign update failed"
                              "Campaign is now halted"
                              "Sorry for the inconvenience" ]
                            (fun() -> Campaign.Run.MissionLogParsing.updateState(config, missionResults))
                    let newProduction, battleResults, ((oldState, newState) as states) = updatedState
                    let world =
                        try
                            let serializer = FsPickler.CreateXmlSerializer(indent = true)
                            use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                            serializer.Deserialize<Campaign.WorldDescription.World>(worldFile)
                        with
                        | e -> failwithf "Failed to read world data. Reason was: '%s'" e.Message
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

    let start(support : SupportApis, config, status, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage) =
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
                let action = status.GetAsync(support, config, serverProc, announceResults, announceWeather, announceWorldState)
                try
                    let serializer = FsPickler.CreateXmlSerializer(indent = true)
                    use worldFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.world))
                    use stateFile = File.OpenText(Path.Combine(config.OutputDir, Filenames.state))
                    let world = serializer.Deserialize<Campaign.WorldDescription.World>(worldFile)
                    let state = serializer.Deserialize<Campaign.WorldState.WorldState>(stateFile)
                    // The main intent is to generate the situation map.
                    announceWorldState(world, state)
                with
                | _ -> support.Logging.LogError("Failed to generate situational map")

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

    let reset(support : SupportApis, config : Configuration, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage) =
        async {
            // Delete log files
            let logDir = Path.Combine(config.ServerDataDir, "logs")
            for file in Directory.EnumerateFiles(logDir, "*.txt") do
                try
                    File.Delete(file)
                with
                | _ -> ()
            // Delete campaign files
            for file in Directory.EnumerateFiles(config.OutputDir) do
                try
                    File.Delete(file)
                with
                | _ -> ()
            // Initial campaign state
            let startDate = Campaign.Run.Init.createWorld config
            ignore <| Campaign.Run.WeatherComputation.run(config, startDate)
            Campaign.Run.Init.createState config
            Campaign.Run.OrderDecision.run config
            // Start campaign
            return start(support, config, Some GenerateMission, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage)
        }
        |> ScheduledTask.SomeTaskNow "generate mission"

    /// Build a graphical representation of the strategic situation
    let mkMapGraphics(world : World, state : WorldState) : MapGraphics.MapPackage =
        let icons : MapGraphics.MapIcon list =
            [
                for reg, regState in List.zip world.Regions state.Regions do
                    match regState.Owner with
                    | Some coalition ->
                        let numTanks = regState.NumVehicles |> Map.toSeq |> Seq.sumBy snd
                        yield {
                            Position = reg.Position
                            Icon = MapGraphics.Base
                            Color = if coalition = Axis then MapGraphics.Gray else MapGraphics.Red
                            Label = Some (sprintf "%s (%d tanks, %3.0f supplies)" (string reg.RegionId) numTanks regState.Supplies)
                            Depth = 0.0f
                        }
                    | None ->
                        ()
            ]
        let sg = state.FastAccess
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

    let announceTakeOffToTeam (player : string, coalition : CoalitionId, airfield : AirfieldId, afCoalition : CoalitionId option, plane : PlaneModel, cargo : float32<E>) =
        match support with
        | Some support ->
            async {
                let atEnemy = Some coalition.Other = afCoalition
                let cargo =
                    if cargo > 0.0f<E> then
                        sprintf " with %3.0fkg of cargo" (cargo / bombCost)
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

    let announceLandingToTeam (player : string, coalition : CoalitionId, airfield : AirfieldId, afCoalition : CoalitionId option, plane : PlaneModel, cargo : float32<E>, health : float32, damageInflicted : float32<E>) =
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
                                if cargo > 0.0f<E> then
                                    sprintf " with %3.0fkg of cargo" (cargo / bombCost)
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
                            if cargo > 0.0f<E> then
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
        match support with
        | Some support ->
            async {
                let team =
                    match coalition with
                    | Axis -> support.ServerControl.GetAxisTeam()
                    | Allies -> support.ServerControl.GetAlliesTeam()
                let msg = sprintf "Max battle damage inflicted at %s, further kills will be ignored" region
                return! support.ServerControl.MessageTeam(team, [msg])
            }
        | None ->
            async {
                return()
            }

    let punishPlayer (penalty : Judgement) =
        match support with
        | Some support ->
            async {
                let! players =
                    support.ServerControl.GetPlayerList
                let player =
                    players
                    |> Seq.tryFind (fun p -> p.GetName() = penalty.Player.Name)
                match player, penalty.Decision with
                | Some player, Informed txt ->
                    logger.Info(sprintf "Warn player '%s': '%s'" penalty.Player.Name txt)
                    do! support.ServerControl.MessagePlayer(player, [txt])
                    do! Async.Sleep(5000)
                | Some player, Banned hours ->
                    logger.Info(sprintf "Ban player '%s' for %d hours" penalty.Player.Name hours)
                    do! support.ServerControl.MessageAll([sprintf "%s was banned for %d hours" penalty.Player.Name hours])
                    do! Async.Sleep(5000)
                    do! support.ServerControl.BanPlayer(player, hours)
                | Some player, Kicked ->
                    do! support.ServerControl.MessageAll([sprintf "%s was kicked" penalty.Player.Name])
                    do! Async.Sleep(5000)
                    do! support.ServerControl.KickPlayer(player)
                | None, _ ->
                    // Player not found
                    ()
            }
        | None ->
            async {
                return ()
            }

    let announceWorldState arg =
        match webHookClient with
        | Some hook -> postWorldState (queue, hook) arg
        | None -> ()
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
                Support.start(support, config, None, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage)
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
                let res = Support.reset(support, config, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage)
                x.StartWebHookClient(config)
                x.StartCommenter(config)
                Choice1Of2 res
            with
            | e ->
                sprintf "Failed to reset campaign: '%s'" e.Message
                |> Choice2Of2
