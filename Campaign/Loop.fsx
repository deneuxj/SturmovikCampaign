#I "../Campaign/bin/Debug"

#load "Configuration.fsx"

#r "ploggy"

open System.Diagnostics
open System.IO
open Configuration


let killServer(runningProc : Process option) =
    let procToKill = runningProc |> Option.filter (fun proc -> not proc.HasExited)
    let procsToKill =
        match procToKill with
        | None ->
            let procs =
                Process.GetProcessesByName("DServer")
            let procs =
                try
                    procs
                    |> Array.filter (fun proc -> Path.GetFullPath(Path.GetDirectoryName(proc.MainModule.FileName)) = Path.GetFullPath(config.ServerBinDir))
                with
                | exc ->
                    printfn "Failed to filter processes: '%s" exc.Message
                    procs
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

let rec loop serverProc =
    async {
        printfn "Preparing to kill server..."
        killServer serverProc
        printfn "Server killed. Preparing to generate mission..."
        let exitStatus = Campaign.Run.MissionFileGeneration.run config
        //let exitStatus = 0
        if exitStatus = 0 then
            printfn "Mission generated, preparing to restart server..."
            let serverProc = startServer()
            match serverProc with
            | Some serverProc ->
                printfn "Server restarted, waiting for mission end..."
                do! Async.Sleep(config.MissionLength * 60000)
                printfn "Mission time elapsed, preparing to extract results and update state.."
                Campaign.Run.MissionLogParsing.stage1 config
                |> snd
                |> Campaign.Run.MissionLogParsing.stage2 config
                printfn "Results extracted and state updated"
                return! loop (Some serverProc)
            | None ->
                printfn "Failed to restart server"
        else
            printfn "Mission not generated"
    }

loop None
|> Async.RunSynchronously