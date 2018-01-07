﻿/// Campaign monitoring exposed to SturmovikServerControl
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

module Support =
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
            printfn "Killing DServer process [%d]..." running.Id
            try
                running.Kill()
            with
            | e ->
                printfn "Failed to kill DServer.exe: %s" e.Message

    let startServer(config) =
        // Start DServer with given SDS file.
        try
            let exePath = Path.Combine(config.ServerBinDir, "game", "DServer.exe")
            let sdsPath = Path.Combine(config.ServerDataDir, config.ServerSdsFile)
            printfn "Will start server '%s' with arg '%s'" exePath sdsPath
            let si = ProcessStartInfo(exePath, sdsPath)
            si.WorkingDirectory <- Path.GetDirectoryName(exePath)
            si.UseShellExecute <- false
            let proc = Process.Start(si)
            printfn "%s [%d] started." proc.ProcessName proc.Id
            Some proc
        with
        | e ->
            printfn "Failed to start DServer.exe: %s" e.Message
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
                          "Actions past this point will not be taken into account"
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
                            [ "Bad news, campaign updated failed"
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

type Plugin() =
    let mutable support : SupportApis option = None
    let mutable webHookClient : (System.Net.WebClient * System.Uri) option = None
    let mutable commenter : CommentatorRestarter option = None
    let mutable queue = startQueue()
    let mutable world = None
    let mutable state = None

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

    let announceTakeOffToTeam (player : string, coalition : CoalitionId, airfield : AirfieldId, plane : PlaneModel, cargo : float32<E>) =
        match support with
        | Some support ->
            async {
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
                    sprintf "%s took off from %s in %s%s"
                        player
                        airfield.AirfieldName
                        planeType
                        cargo
                let team =
                    match coalition with
                    | Axis -> support.ServerControl.GetAxisTeam()
                    | Allies -> support.ServerControl.GetAlliesTeam()
                return! support.ServerControl.MessageTeam(team, [message])
            }
        | None ->
            async { return () }

    let announceLandingToTeam (player : string, coalition : CoalitionId, airfield : AirfieldId, plane : PlaneModel, cargo : float32<E>, health : float32, damageInflicted : float32<E>) =
        match support with
        | Some support ->
            async {
                let cargo =
                    if cargo > 0.0f<E> then
                        sprintf " with %3.0fkg of cargo" (cargo / bombCost)
                    else
                        ""
                let team =
                    match coalition with
                    | Axis -> support.ServerControl.GetAxisTeam()
                    | Allies -> support.ServerControl.GetAlliesTeam()
                let intro =
                    if damageInflicted = 0.0f<E> then
                        sprintf "%s is back" player
                    elif damageInflicted < GroundAttackVehicle.LightArmorCost then
                        sprintf "%s is welcomed back on the ground" player
                    elif damageInflicted < GroundAttackVehicle.MediumTankCost then
                        sprintf "%s's return is celebrated" player
                    else
                        sprintf "the entire base rushes to welcome %s" player
                let difficulty =
                    if health = 1.0f then
                        ""
                    elif health > 0.9f then
                        " after a difficult mission"
                    elif health > 0.1f then
                        " after a dangerous mission"
                    else
                        " after narrowly escaping death"
                return! support.ServerControl.MessageTeam(team, [intro + difficulty + cargo])
            }
        | None ->
            async {
                return()
            }

    let announceWorldState arg =
        match webHookClient with
        | Some hook -> postWorldState (queue, hook) arg
        | None -> ()

    member x.StartWebHookClient(config : Configuration) =
        let webHookUri = config.WebHook
        // Create WebClient for web hook, if not already done
        match webHookClient with
        | None ->
            if not(String.IsNullOrEmpty(webHookUri)) then
                webHookClient <- Some(createClient(webHookUri))
                printfn "WebHook client created"
        | Some _ ->
            ()
        // Stop commenter
        match commenter with
        | Some commenter ->
            commenter.Dispose()
        | None ->
            ()
        // (Re-)start commenter
        match webHookClient, world, state with
        | Some webHookClient, Some world, Some state ->
            let handlers =
                { OnCargoTookOff = announceTakeOffToTeam
                  OnLanded = announceLandingToTeam
                }
            commenter <- Some(new CommentatorRestarter(Path.Combine(config.ServerDataDir, "logs"), config.OutputDir, config.MissionName, handlers, fun() -> x.StartWebHookClient(config)))
            printfn "Commenter set"
        | _, None, _
        | _, _, None
        | None, _, _ ->
            ()

    member x.LoadWorldAndState(path) =
        let serializer = FsPickler.CreateXmlSerializer()
        try
            use worldFile = File.OpenText(Path.Combine(path, Filenames.world))
            use stateFile = File.OpenText(Path.Combine(path, Filenames.state))
            let w = serializer.Deserialize<World>(worldFile)
            let s = serializer.Deserialize<WorldState>(stateFile)
            world <- Some w
            state <- Some s
        with
        | exc ->
            match support with
            | Some apis ->
                apis.Logging.LogError(sprintf "Failed to load world and state from %s: '%s'" path exc.Message)
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
                x.LoadWorldAndState(config.OutputDir)
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
                x.StartWebHookClient(config)
                x.LoadWorldAndState(config.OutputDir)
                Support.reset(support, config, onCampaignOver, announceResults, announceWeather, announceWorldState, postMessage)
                |> Choice1Of2
            with
            | e ->
                sprintf "Failed to reset campaign: '%s'" e.Message
                |> Choice2Of2
