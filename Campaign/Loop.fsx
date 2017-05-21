#I "../Campaign/bin/Debug"

#load "Configuration.fsx"

#r "ploggy"
#r "FsPickler"

open System.Diagnostics
open System.IO
open Configuration
open MBrace.FsPickler
open Campaign.BasicTypes

let findRunningServers() =
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

let killServer(runningProc : Process option) =
    let procToKill = runningProc |> Option.filter (fun proc -> not proc.HasExited)
    let procsToKill =
        match procToKill with
        | None ->
            let procs = findRunningServers()
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

let startServer() =
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
    | MakeWeather
    | DecideOrders
    | GenerateMission
    | StartServer
    | WaitForMissionEnd of System.DateTime
    | ExtractResults
    | CampaignOver of victorious: CoalitionId
    | Failed of Message:string * StackTrace:string option * ExecutionState
with
    member this.GetAsync(serverProc : Process option) =
        match this with
        | KillServer ->
            async {
                printfn "Kill server..."
                killServer serverProc
                return None, ExtractResults
            }
        | MakeWeather ->
            async {
                printfn "Make weather..."
                Campaign.Run.WeatherComputation.run config
                return None, DecideOrders
            }
        | DecideOrders ->
            async {
                printfn "Deciding orders..."
                Campaign.Run.OrderDecision.run config
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
                let serverProc = startServer()
                match serverProc with
                | None ->
                    return None, Failed("Failed to start server", None, this)
                | Some _ ->
                    let expectedMissionEnd = System.DateTime.UtcNow + System.TimeSpan(600000000L * int64 config.MissionLength)
                    return serverProc, WaitForMissionEnd expectedMissionEnd
            }
        | WaitForMissionEnd time ->
            let rec work() =
                async {
                    if System.DateTime.UtcNow >= time then
                        return serverProc, KillServer
                    else
                        do! Async.Sleep 60000
                        return! work()
                }
            async {
                printfn "Wait..."
                return! work()
            }
        | ExtractResults ->
            async {
                printfn "Extract results..."
                let _, ((oldState, newState) as states) = Campaign.Run.MissionLogParsing.stage1 config
                if not(newState.HasCoalitionFactories(Axis)) then
                    return serverProc, CampaignOver(Allies)
                elif not(newState.HasCoalitionFactories(Allies)) then
                    return serverProc, CampaignOver(Axis)
                else
                    states
                    |> Campaign.Run.MissionLogParsing.stage2 config
                    return serverProc, MakeWeather
            }
        | CampaignOver _ ->
            async {
                printfn "Cannot continue campaign that is over."
                return serverProc, this
            }
        | Failed(msg, stackTrace, state) ->
            async {
                printfn "Failed!"
                return serverProc, this
            }

    member this.Save() =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let statusFile = Path.Combine(config.OutputDir, "loopState.xml")
        if File.Exists(statusFile) then
            File.Delete(statusFile)
        use s = File.CreateText(statusFile)
        serializer.Serialize(s, this)

    static member Restore() =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let statusFile = Path.Combine(config.OutputDir, "loopState.xml")
        if File.Exists(statusFile) then
            use s = File.OpenText(statusFile)
            serializer.Deserialize<ExecutionState>(s)
        else
            StartServer

let loop() =
    let rec work (status : ExecutionState) (serverProc : Process option) =
        async {
            match status with
            | Failed(msg, _, _) ->
                printfn "Execution aborted due to failure: %s" msg
                status.Save()
            | CampaignOver(victorious) ->
                match victorious with
                | Axis -> "Axis has won"
                | Allies -> "Allies have won"
                |> printfn "Campaign is over, %s the battle!"
                status.Save()
            | _ ->
                let action = status.GetAsync(serverProc)
                try
                    let! res = action
                    let serverProc, status = res
                    status.Save()
                    return! work status serverProc
                with
                | exc ->
                    return! work (Failed(exc.Message, Some exc.StackTrace, status)) None
        }
    let status = ExecutionState.Restore()
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
                if findRunningServers().Length > 0 then
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
    Async.RunSynchronously(work status None)

loop()
