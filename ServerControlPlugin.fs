/// Campaign monitoring exposed to SturmovikServerControl
namespace Campaign.ServerControlPlugin

open CampaignServerControl.Api

open System.Diagnostics
open System.IO
open MBrace.FsPickler
open Campaign.BasicTypes
open Campaign.Run
open Campaign.Configuration
open System

module Support =
    let findRunningServers(config) =
            let procs =
                Process.GetProcessesByName("DServer")
            let procs =
                try
                    procs
                    |> Array.filter (fun proc -> Path.GetFullPath(Path.GetDirectoryName(proc.MainModule.FileName)).StartsWith(Path.GetFullPath(config.ServerBinDir)))
                with
                | exc ->
                    printfn "Failed to filter processes: '%s" exc.Message
                    procs
            procs

    let killServer(config, runningProc : Process option) =
        let procToKill = runningProc |> Option.filter (fun proc -> not proc.HasExited)
        let procsToKill =
            match procToKill with
            | None ->
                let procs = findRunningServers(config)
                // Kill all DServers started from that instance directory (should be one or zero).
                if procs.Length = 0 then
                    printfn "No DServer running, none to kill."
                elif procs.Length > 1 then
                    printfn "Multiple DServer instances running under %s" config.ServerBinDir
                procs
            | Some running ->
                [| running |]
        for running in procsToKill do
            printfn "Killing DServer process..."
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
        | KillServer
        | DecideOrders
        | GenerateMission
        | StartServer
        | WaitForMissionEnd of System.DateTime
        | ExtractResults
        | CampaignOver of victorious: CoalitionId
        | Failed of Message:string * StackTrace:string option * ExecutionState
    with
        member this.GetAsync(config, serverProc : Process option) =
            match this with
            | DecideOrders ->
                async {
                    printfn "Deciding orders..."
                    Campaign.Run.OrderDecision.run config
                    return None, KillServer
                }
            | KillServer ->
                async {
                    printfn "Kill server..."
                    killServer(config, serverProc)
                    return None, GenerateMission
                }
            | GenerateMission ->
                async {
                    printfn "Generate mission..."
                    let exitStatus = Campaign.Run.MissionFileGeneration.run config
                    if exitStatus <> 0 then
                        return None, Failed(sprintf "Resaver failed, exit status %d" exitStatus, None, this)
                    else
                        return None, StartServer
                }
            | StartServer ->
                async {
                    printfn "Start server..."
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
                    printfn "Check if mission is over..."
                    if System.DateTime.UtcNow >= time then
                        return serverProc, ExtractResults
                    else
                        return serverProc, WaitForMissionEnd time
                }
            | ExtractResults ->
                async {
                    printfn "Extract results..."
                    let missionResults = Campaign.Run.MissionLogParsing.stage1 config
                    Campaign.Run.MissionLogParsing.backupFiles config
                    printfn "Make weather..."
                    let date = Campaign.Run.WeatherComputation.getNextDateFromState config
                    Campaign.Run.WeatherComputation.run(config, date)
                    let newProduction, battleResults, ((oldState, newState) as states) = Campaign.Run.MissionLogParsing.updateState(config, missionResults)
                    if not(newState.HasCoalitionFactories(Axis)) then
                        return serverProc, CampaignOver(Allies)
                    elif not(newState.HasCoalitionFactories(Allies)) then
                        return serverProc, CampaignOver(Axis)
                    else
                        let axisAAR, alliesAAR = Campaign.Run.MissionLogParsing.buildAfterActionReports(config, oldState, newState, missionResults.TakeOffs, missionResults.Landings, missionResults.StaticDamages @ missionResults.VehicleDamages, newProduction)
                        Campaign.Run.MissionLogParsing.stage2 config (oldState, newState, axisAAR, alliesAAR, battleResults)
                        return serverProc, DecideOrders
                }
            | CampaignOver _ ->
                async {
                    printfn "Cannot continue campaign that is over."
                    return serverProc, KillServer
                }
            | Failed(msg, stackTrace, state) ->
                async {
                    printfn "Failed!"
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
                StartServer

    let start(config) =
        let rec work (status : ExecutionState) (serverProc : Process option) =
            match status with
            | Failed(msg, _, _) ->
                printfn "Execution aborted due to failure: %s" msg
                status.Save(config)
                NoTask
            | CampaignOver(victorious) ->
                match victorious with
                | Axis -> "Axis has won"
                | Allies -> "Allies have won"
                |> printfn "Campaign is over, %s the battle!"
                status.Save(config)
                NoTask
            | _ ->
                let step action =
                    async {
                        try
                            let! proc, (status : ExecutionState) = action
                            status.Save(config)
                            match status with
                            | WaitForMissionEnd time ->
                                return SomeTask(time, async { return work status proc })
                            | _ ->
                                return ScheduledTask.SomeTaskNow(async { return work status proc })
                        with
                        | exc ->
                            return work (Failed(exc.Message, Some exc.StackTrace, status)) None
                    }
                let action = status.GetAsync(config, serverProc)
                ScheduledTask.SomeTaskNow(step action)

        let status = ExecutionState.Restore(config)
        // If the status was failed, plan to retry the operation that failed.
        let status =
            match status with
            | Failed(msg, _, ExtractResults) ->
                printfn "Previously failed to extract results, will restart the server"
                StartServer
            | Failed(msg, _, status) ->
                printfn "Retry after failure '%s'" msg
                status
            | _ ->
                status
        // If the saved state was waiting, check if we got back before the expected end time and the server is running
        let status =
            match status with
            | WaitForMissionEnd time ->
                if System.DateTime.UtcNow < time then
                    if findRunningServers(config).Length > 0 then
                        printfn "Resume waiting"
                        status // deadline not passed, and server running -> resume waiting
                    else
                        printfn "Restart server"
                        StartServer // deadline not passed, no server running -> restart the server
                else
                    printfn "Extract results"
                    ExtractResults // deadline passed -> extract results
            | _ ->
                printfn "Resume"
                status
        work status None

    let reset (config : Configuration) =
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
            Campaign.Run.WeatherComputation.run(config, startDate)
            Campaign.Run.Init.createState config
            Campaign.Run.OrderDecision.run config
            // Start campaign
            return start config
        }
        |> ScheduledTask.SomeTaskNow

type Plugin() =
    interface CampaignServerApi with
        member x.StartOrResume configFile =
            try
                let config = loadConfigFile configFile
                Support.start config
                |> Choice1Of2
            with
            | e ->
                sprintf "Failed to start or resume campaign: '%s'" e.Message
                |> Choice2Of2
        member x.Reset configFile =
            try
                let config = loadConfigFile configFile
                Support.reset config
                |> Choice1Of2
            with
            | e ->
                sprintf "Failed to reset campaign: '%s'" e.Message
                |> Choice2Of2
