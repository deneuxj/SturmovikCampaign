#I "../Campaign/bin/Debug"

#load "Configuration.fsx"

open System.Diagnostics
open System.IO

let runScript script =
    let scriptPath = Path.Combine(Configuration.ScriptPath, script)
    let p = ProcessStartInfo(Configuration.FsiPath, sprintf "%s" scriptPath)
    p.WorkingDirectory <- System.Environment.CurrentDirectory
    p.UseShellExecute <- false
    Process.Start(p)

let restartServer(runningProc : Process option) =
    let procToKill = runningProc |> Option.filter (fun proc -> not proc.HasExited)
    let procsToKill =
        match procToKill with
        | None ->
            let procs =
                Process.GetProcessesByName("DServer")
                |> Array.filter (fun proc -> Path.GetFullPath(Path.GetDirectoryName(proc.MainModule.FileName)) = Path.GetFullPath(Configuration.ServerBinDir))
            // Kill all DServers started from that instance directory (should be one or zero).
            if procs.Length = 0 then
                printfn "No DServer running, none to kill."
            elif procs.Length > 1 then
                printfn "Multiple DServer instances running under %s" Configuration.ServerBinDir
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

    // Start DServer with given SDS file.
    try
        let exePath = Path.Combine(Configuration.ServerBinDir, "game", "DServer.exe")
        let si = ProcessStartInfo(exePath, Configuration.ServerSdsFile)
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
        let proc = runScript "GenerateMission.fsx"
        proc.WaitForExit()
        if proc.ExitCode = 0 then
            let serverProc = restartServer serverProc
            match serverProc with
            | Some serverProc ->
                do! Async.Sleep(Configuration.MissionLength * 60000)
                let proc = runScript "ParseMissionLog.fsx"
                proc.WaitForExit()
                if proc.ExitCode = 0 then
                    return! loop (Some serverProc)
                else
                    printfn "Failed to extract results from mission log"
            | None ->
                printfn "Failed to restart server"
        else
            printfn "Failed to generate mission"
    }

loop None
|> Async.RunSynchronously