// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
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
open MBrace.FsPickler
open System.Diagnostics
open NLog
open Campaign.BasicTypes

type Settings =
    {
        Address : string
        Port : int
        Login : string
        Password : string
        SdsFile : string
        WorkDir : string
        GameDir : string
        MissionBaseName : string
        MissionDuration : int
        RotateMissionServerInputName : string
        RoadsCapacity : float32
        RailsCapacity : float32
        SimulatedDuration : float32
    }
with
    member this.MissionPath = IO.Path.Combine(this.WorkDir, "Multiplayer", "Dogfight")

    member this.GameMissionPath = IO.Path.Combine(this.GameDir, "Multiplayer", "Dogfight")

    member this.MissionFile = IO.Path.Combine(this.MissionPath, sprintf "%s_1.Mission" this.MissionBaseName)

    member this.AltMissionFile = IO.Path.Combine(this.MissionPath, sprintf "%s_2.Mission" this.MissionBaseName)

    member this.MissionLogs = IO.Path.Combine(this.GameDir, "data", "logs")

    static member DefaultWorkDir = IO.Path.Combine(Environment.GetEnvironmentVariable("LOCALAPPDATA"), "CoconutCampaign", "Current")


type SyncState =
    | PreparingMission of Path: string
    | ResavingMission
    | RunningMission of StartTime: DateTime * EndTime: DateTime
    | ExtractingResults of Path: string
    | AdvancingScenario
with
    static member FileName = "sync.json"


module IO =
    open FSharp.Json

    type IOSettings =
        {
            address : string option
            port : int option
            login : string option
            password : string option
            sds_file : string option
            work_dir : string option
            game_dir : string
            mission_basename : string option
            mission_duration : int option
            rotate_input : string option
            roads_capacity : float option
            rails_capacity : float option
            simulated_duration : float option
        }
    with
        member this.AsSettings =
            let truck = 5.0<M^3>
            let separation = 10.0<M>
            let speed = 50000.0<M/H>
            let numTrucks = speed / separation
            let roadCapacity = float(numTrucks * truck)
            {
                Address = defaultArg this.address "127.0.0.1"
                Port = defaultArg this.port 9001
                Login = defaultArg this.login "admin"
                Password = defaultArg this.password ""
                SdsFile = defaultArg this.sds_file "campaign.sds"
                WorkDir = defaultArg this.work_dir "campaign"
                GameDir = this.game_dir
                MissionBaseName = defaultArg this.mission_basename "CocoCampaign"
                MissionDuration = defaultArg this.mission_duration 180
                RotateMissionServerInputName = defaultArg this.rotate_input "ReqMissionEnd"
                RoadsCapacity = defaultArg this.roads_capacity roadCapacity |> float32
                RailsCapacity = defaultArg this.rails_capacity (3.0 * roadCapacity) |> float32
                SimulatedDuration = defaultArg this.simulated_duration 180.0 |> float32
            }

    /// Create a default settings file and return its content.
    let createDefaultFile path =
        let dirName = IO.Path.GetDirectoryName(path)
        if not(IO.Directory.Exists(dirName)) then
            try
                IO.Directory.CreateDirectory(dirName)
                |> ignore
            with _ ->
                eprintfn "Failed to create work area '%s'" dirName

        let content =
            {
                address = None
                port = None
                login = None
                password = None
                sds_file = None
                work_dir = IO.Path.Combine(dirName, "Current") |> IO.Path.GetFullPath |> Some
                game_dir = @"C:\Program Files\IL-2 Sturmovik Battle of Stalingrad"
                mission_basename = None
                mission_duration = None
                rotate_input = None
                roads_capacity = None
                rails_capacity = None
                simulated_duration = None
            }
        let json = Json.serialize content
        IO.File.WriteAllText(path, json)
        content.AsSettings

    /// Load settings from a file, and set the WorkDir value to the directory of the file if it's not set in the file.
    let loadFromFile path =
        let content = IO.File.ReadAllText(path)
        let loaded = Json.deserializeEx<IOSettings> { JsonConfig.Default with deserializeOption = DeserializeOption.AllowOmit } content
        let loaded =
            { loaded with
                work_dir =
                    loaded.work_dir
                    |> Option.orElse (Some(path |> IO.Path.GetDirectoryName |> fun x -> IO.Path.Combine(x, "Current") |> IO.Path.GetFullPath)) }
        loaded.AsSettings

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

        static member TryLoad(workDir) =
            try
                use file = IO.File.OpenText(IO.Path.Combine(workDir, SyncState.FileName))
                FSharp.Json.Json.deserialize<SyncStateVessel>(file.ReadToEnd())
                |> Some
            with _ -> None
            |> Option.map (fun vessel -> vessel.SyncState)


type IGameServerControl =
    abstract IsRunning : obj option -> bool
    abstract StartProcessWithSds : string -> Async<Result<obj, string>>
    abstract KillProcess : obj option -> Result<unit, string>
    abstract AttachRcon : unit -> Async<Result<unit, string>>
    abstract IsAttached : bool
    abstract SignalMissionEnd : unit -> Async<Result<unit, string>>
    abstract RotateMission : unit -> Async<Result<unit, string>>
    abstract LoadSds : string -> Async<Result<unit, string>>
    abstract ResaveMission : string -> Async<Result<unit, string>>
    abstract CopyRenameMission : string -> Async<Result<unit, string>>


type RConGameServerControl(settings : Settings, ?logger) =
    let mutable logger = defaultArg logger (LogManager.GetCurrentClassLogger())
    let mutable client = None
    let mutable proc = None

    let connect() =
        async {
            try
                let cl = new RConClient.Client(settings.Address, settings.Port, settings.Login, settings.Password)
                client <- Some cl
                let! s = cl.Auth()
                logger.Info s
                return Ok ()
            with
            | e ->
                logger.Error e
                return Error <| sprintf "connect() failed: %s" e.Message
        }

    let start(sds) =
        try
            let exePath = IO.Path.Combine(settings.GameDir, "bin", "game", "DServer.exe")
            let filename =
                IO.Path.Combine("..", "..", "data", sds)
            let si = ProcessStartInfo(exePath, filename)
            si.WorkingDirectory <- IO.Path.GetDirectoryName(exePath)
            si.UseShellExecute <- false
            let p = Process.Start(si)
            logger.Info p
            proc <- Some p
            Ok (p :> obj)
        with
        | e ->
            logger.Error e
            Error (sprintf "Failed to start DServer: %s" e.Message)

    let checkIfRunning() =
        let startDir =
            IO.Path.Combine(settings.GameDir, "bin", "game")
            |> IO.Path.GetFullPath
        let procs =
            Process.GetProcessesByName("DServer")
            |> Array.filter (fun proc -> IO.Path.GetFullPath(IO.Path.GetDirectoryName(proc.MainModule.FileName)) = startDir)
        logger.Debug procs
        if procs.Length > 0 then
            logger.Info procs.[0]
            proc <- Some procs.[0]
            true
        else
            false

    let kill() =
        if proc.IsNone then
            checkIfRunning()
            |> ignore
        try
            proc |> Option.iter (fun proc -> proc.Kill())
            proc <- None
            Ok()
        with e ->
            logger.Error e
            Error (sprintf "Failed to kill DServer: %s" e.Message)

    let rotateMission() =
        async {
            if client.IsNone then
                let! _ = connect()
                ()
            match client with
            | Some client ->
                let! s = client.ServerInput(settings.RotateMissionServerInputName)
                logger.Info s
                return Ok ()
            | None ->
                return Error "No connection to DServer"
        }

    let signalMissionEnd() =
        async {
            if client.IsNone then
                let! _ = connect()
                ()
            match client with
            | Some client ->
                let! s = client.MessageAll "Mission has ended. Actions beyond that point will not affect the campaign."
                logger.Info s
                return Ok()
            | None ->
                return Error "No connection to DServer"
        }

    let loadSds(sds : string) =
        async {
            if client.IsNone then
                let! _ = connect()
                ()
            match client with
            | Some client ->
                let! s = client.OpenSds(sds)
                logger.Info s
                return Ok ()
            | None ->
                return Error "No connection to DServer"
        }

    let resaveMission(virtDataPath : string, path : string) =
        async {
            try
                let resaverDir = IO.Path.Combine(settings.GameDir, "bin", "resaver")
                let p = ProcessStartInfo("MissionResaver.exe", sprintf "-d \"%s\" -f \"%s\"" virtDataPath path)
                p.WorkingDirectory <- resaverDir
                p.UseShellExecute <- true
                let proc = Process.Start(p)
                logger.Debug proc
                let rec awaitExited() =
                    async {
                        if not proc.HasExited then
                            do! Async.Sleep(5000)
                            return! awaitExited()
                    }
                do! awaitExited()
                if proc.ExitCode <> 0 then
                    logger.Info proc
                    return Error "Resaver failed"
                else
                    return Ok()
            with
            | e ->
                logger.Error e
                return Error <| sprintf "ResaveMission failed: %s" e.Message
        }

    let copyRenameMission(path : string) =
        async {
            try
                let dirname = IO.Path.GetDirectoryName(path)
                let filename = IO.Path.GetFileNameWithoutExtension(path)
                for file in IO.Directory.GetFiles(dirname, sprintf "%s.*" filename) do
                    let filename2 = IO.Path.GetFileNameWithoutExtension(file)
                    let ext = IO.Path.GetExtension(file)
                    let filename2 =
                        if filename2.EndsWith("_1") then
                            filename2.Substring(0, filename2.Length - 2) + "_2" + ext
                        else
                            filename2 + "_2" + ext
                    let file2 = IO.Path.Combine(IO.Path.GetDirectoryName(file), filename2)
                    logger.Debug (file, file2)
                    IO.File.Copy(file, file2, true)
                return Ok ()
            with e ->
                return Error <| sprintf "CopyRename failed: %s" e.Message
        }

    member this.Dispose() =
        proc |> Option.iter (fun proc -> proc.Dispose())
        client |> Option.iter (fun client -> client.Dispose())

    interface System.IDisposable with
        member this.Dispose() = this.Dispose()

    interface IGameServerControl with
        member this.AttachRcon() =
            connect()

        member this.CopyRenameMission(path) =
            copyRenameMission(path)

        member this.IsAttached =
            client.IsSome

        member this.KillProcess(proc) =
            match proc with
            | Some(:? Process as proc) ->
                try
                    proc.Kill()
                    Ok()
                with
                | e -> Error <| sprintf "Kill failed: %s" e.Message
            | Some _ ->
                invalidArg "proc" "Must be a Process option"
            | None ->
                kill()

        member this.LoadSds(sds) =
            loadSds(sds)

        member this.ResaveMission(path) =
            resaveMission(settings.WorkDir, path)

        member this.RotateMission() =
            rotateMission()

        member this.SignalMissionEnd() =
            signalMissionEnd()

        member this.StartProcessWithSds(sds) =
            async.Return(start(sds))

        member this.IsRunning(proc2) =
            match proc2 with
            | Some (:? Process as proc2) ->
                not proc2.HasExited
            | Some _ ->
                invalidArg "proc2" "Must be of type Process option"
            | None ->
                checkIfRunning()


open IO

/// Controls execution of DServer, depending on status of campaign scenario controller.
type Sync(settings : Settings, gameServer : IGameServerControl, ?logger) =
    let mutable logger = defaultArg logger (LogManager.GetCurrentClassLogger())
    let mutable serverProcess = None
    let mutable state =
        SyncState.TryLoad(settings.WorkDir)
        |> Option.defaultValue (PreparingMission settings.MissionFile)

    let cancellation = new System.Threading.CancellationTokenSource()

    // Number of DServer restart attempts
    let maxRetries = 3

    let missionCompleted = Event<Result<string, string>>()

    let missionPrepared = Event<Result<unit, string>>()

    let missionLogsExtracted = Event<Result<unit, string>>()

    let scenarioAdvanced = Event<Result<unit, string>>()

    let terminated = Event<string>()

    /// Get the current synchronization state
    member this.SyncState = state

    /// Campaign scenario controller triggers this when mission file has been generated
    member this.NotifyMissionPrepared(result) = missionPrepared.Trigger(result)

    /// Campaign scenario controller triggers this when the mission log have been extracted
    member this.NotifyMissionLogsExtracted(result) = missionLogsExtracted.Trigger(result)

    /// Campaign scenario controller triggers this when scenario simulation is done
    member this.NotifyScenarioAdvanced(result) = scenarioAdvanced.Trigger(result)

    /// Campaign scenario controller subscribes to this event to know when a round is over
    member this.MissionCompleted = missionCompleted.Publish

    /// Campaign scenario controller subscribes to this event to know when sync is terminated
    member this.Terminated = terminated.Publish

    /// Stop synchonization, typically after an unrecoverable error.
    member this.Die(msg) =
        state.Save(settings.WorkDir)
        let msg =
            sprintf "Game server sync terminated: %s" msg
        logger.Info msg
        terminated.Trigger(msg)

    /// Cancel any ongoing task.
    member this.Interrupt(msg, killServer) =
        let msg =
            sprintf "Game server sync interrupted: %s" msg
        logger.Info msg
        if killServer then
            gameServer.KillProcess(serverProcess)
            |> ignore
        cancellation.Cancel()

    member this.PublishMissionFiles() =
        for file in IO.Directory.EnumerateFiles(settings.MissionPath) do
            try
                if IO.Path.GetExtension(file).ToLowerInvariant() = ".mission" then
                    IO.File.Delete(IO.Path.Combine(settings.GameDir, IO.Path.GetFileName(file)))
                else
                    IO.File.Copy(file, IO.Path.Combine(settings.GameDir, IO.Path.GetFileName(file)), true)
            with
            | exc -> logger.Warn(sprintf "Failed to copy %s: %s" file exc.Message)

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
                state <- RunningMission (now, now + TimeSpan.FromMinutes(float settings.MissionDuration))
                logger.Debug state
                state.Save(settings.WorkDir)
                let! s = gameServer.AttachRcon()
                logger.Debug s
                return! this.ResumeAsync(retriesLeft - 1)
            | Error msg ->
                serverProcess <- None
                return this.Die(msg)
        }

    /// Check current state and act accordingly
    member this.ResumeAsync(?restartsLeft) =
        let restartsLeft = defaultArg restartsLeft maxRetries
        async {
            logger.Trace restartsLeft
            match state with
            | PreparingMission ->
                match! Async.AwaitEvent(missionPrepared.Publish, fun () -> this.Die("Interrupted")) with
                | Ok() ->
                    state <- ResavingMission
                    logger.Info state
                    state.Save(settings.WorkDir)
                    return! this.ResumeAsync()
                | Error msg ->
                    return this.Die(msg)

            | ResavingMission ->
                match! gameServer.CopyRenameMission settings.MissionFile with
                | Error msg ->
                    return this.Die(msg)
                | Ok() ->
                match! gameServer.ResaveMission settings.MissionFile with
                | Error msg ->
                    return this.Die(msg)
                | Ok() ->
                match! gameServer.ResaveMission settings.AltMissionFile with
                | Error msg ->
                    return this.Die(msg)
                | Ok() ->
                this.PublishMissionFiles()
                if gameServer.IsRunning serverProcess then
                    let! s = gameServer.RotateMission()
                    logger.Debug s
                    match s with
                    | Ok() ->
                        let now = DateTime.UtcNow
                        state <- RunningMission (now, now + TimeSpan.FromMinutes(float settings.MissionDuration))
                        logger.Info state
                        state.Save(settings.WorkDir)
                        return! this.ResumeAsync()
                    | Error msg ->
                        return this.Die(msg)
                else
                    return! this.StartServerAsync()
            | RunningMission(startTime, endTime) ->
                if not(gameServer.IsRunning serverProcess) then
                    return! this.StartServerAsync(restartsLeft)
                else
                // Check that DServer is running every 15s until the deadline has passed
                // If DServer dies before, restart it.
                let rec monitor() =
                    async {
                        let untilDeadLine = (endTime - DateTime.UtcNow).TotalMilliseconds
                        if untilDeadLine > 0.0 then
                            if not (gameServer.IsRunning serverProcess) then
                                return! this.StartServerAsync(restartsLeft)
                            else
                                try
                                    do!  Async.Sleep(15000)
                                    return! monitor()
                                with
                                | :? OperationCanceledException ->
                                    return this.Die("Interrupted")
                        else
                            return()
                    }
                do! monitor()
                let! _ = gameServer.SignalMissionEnd()
                // Find the set of log files to use. Pick the first file with suffix [0] that is newer than the mission start time.
                let latestStartingMissionReport =
                    try
                        IO.Directory.GetFiles(settings.MissionLogs, "missionReport*.txt")
                        |> Seq.filter (fun file -> IO.Path.GetFileNameWithoutExtension(file).EndsWith("[0]") && IO.File.GetCreationTimeUtc(file) > startTime)
                        |> Seq.minBy (fun file -> IO.File.GetCreationTimeUtc(file))
                        |> Some
                    with _ -> None
                match latestStartingMissionReport with
                | Some path ->
                    missionCompleted.Trigger(Ok path)
                    state <- ExtractingResults path
                    logger.Info state
                    state.Save(settings.WorkDir)
                    return! this.ResumeAsync()
                | None ->
                    missionCompleted.Trigger(Error "No logs found")
                    // Restart mission
                    gameServer.KillProcess(serverProcess) |> ignore
                    do! Async.Sleep(5000)
                    return! this.ResumeAsync()

            | ExtractingResults ->
                match! Async.AwaitEvent(missionLogsExtracted.Publish, fun () -> this.Die("Interrupted")) with
                | Ok() ->
                    state <- AdvancingScenario
                    logger.Info state
                    state.Save(settings.WorkDir)
                    return! this.ResumeAsync()
                | Error msg ->
                    // Result extraction failed, stop sync.
                    return this.Die(msg)

            | AdvancingScenario ->
                match! Async.AwaitEvent(scenarioAdvanced.Publish, fun () -> this.Die("Interrupted")) with
                | Ok() ->
                    state <- PreparingMission settings.MissionFile
                    logger.Info state
                    state.Save(settings.WorkDir)
                    return! this.ResumeAsync()
                | Error msg ->
                    // Scenario failed, stop sync.
                    return this.Die(msg)

        }

    /// Token to use with ResumeAsync in order for Interrupt to work.
    member this.CancellationToken = cancellation.Token

    /// Start synchronization.
    member this.Resume() =
        Async.StartImmediate(this.ResumeAsync(), this.CancellationToken)

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