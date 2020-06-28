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
    }
with
    member this.MissionPath = IO.Path.Combine(this.WorkDir, "Multiplayer", "Dogfight")


module IO =
    open FSharp.Json

    type IOSettings =
        {
            address : string option
            port : int option
            login : string option
            password : string option
            sds_file : string option
            work_dir : string
            game_dir : string
            mission_basename : string option
            mission_duration : int option
            rotate_input : string option
        }
    with
        member this.AsSettings =
            {
                Address = defaultArg this.address "127.0.0.1"
                Port = defaultArg this.port 9001
                Login = defaultArg this.login "admin"
                Password = defaultArg this.password ""
                SdsFile = defaultArg this.sds_file "campaign.sds"
                WorkDir = this.work_dir
                GameDir = this.game_dir
                MissionBaseName = defaultArg this.mission_basename "CocoCampaign"
                MissionDuration = defaultArg this.mission_duration 180
                RotateMissionServerInputName = defaultArg this.rotate_input "EndMission"
            }

    let loadFromFile path =
        let content = IO.File.ReadAllText(path)
        Json.deserializeEx<IOSettings> { JsonConfig.Default with deserializeOption = DeserializeOption.AllowOmit } content


type SyncState =
    | PreparingMission
    | RunningMission of EndTime: DateTime
    | ExtractingResults
with
    static member FileName = "sync.xml"

    member this.Save(workDir : string) =
        let serializer = FsPickler.CreateXmlSerializer()
        let tmpFile = IO.Path.Combine(workDir, SyncState.FileName + ".tmp")
        use file = IO.File.CreateText(tmpFile)
        serializer.Serialize(file, this)
        IO.File.Copy(tmpFile, IO.Path.Combine(workDir, "sync.xml"), true)
        IO.File.Delete(tmpFile)

    static member TryLoad(workDir) =
        let serializer = FsPickler.CreateXmlSerializer()
        try
            use file = IO.File.OpenText(IO.Path.Combine(workDir, SyncState.FileName))
            serializer.Deserialize<SyncState>(file)
            |> Some
        with _ -> None


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

    let resaveMission(path : string) =
        async {
            try
                let filename = IO.Path.GetFileName(path)
                let dirpath = IO.Path.GetDirectoryName(path)
                let resaverDir = IO.Path.Combine(settings.GameDir, "bin", "resaver")
                let p = ProcessStartInfo("MissionResaver.exe", sprintf "-d \"%s\" -f \"%s\"" dirpath filename)
                p.WorkingDirectory <- resaverDir
                p.UseShellExecute <- true
                let proc = Process.Start(p)
                logger.Debug proc
                let! _ = Async.AwaitEvent proc.Exited
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
                    IO.File.Copy(file, file2)
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
            resaveMission(path)

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


/// Controls execution of DServer, depending on status of campaign scenario controller.
type Sync(settings : Settings, gameServer : IGameServerControl, ?logger) =
    let mutable logger = defaultArg logger (LogManager.GetCurrentClassLogger())
    let mutable serverProcess = None
    let mutable state = SyncState.TryLoad(settings.WorkDir) |> Option.defaultValue PreparingMission

    let cancellation = new System.Threading.CancellationTokenSource()

    // Number of DServer restart attempts
    let maxRetries = 3

    let missionCompleted = Event<Result<unit, string>>()

    let missionPrepared = Event<Result<unit, string>>()

    let missionLogsExtracted = Event<Result<unit, string>>()

    let terminated = Event<string>()

    /// Campaign scenario controller triggers this when mission file has been generated
    member this.NotifyMissionPreparred(result) = missionPrepared.Trigger(result)

    /// Campaign scenario controller triggers this when the mission log have been extracted
    member this.NotifyMissionLogsExtracted(result) = missionLogsExtracted.Trigger(result)

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

    member this.Interrupt(msg) =
        cancellation.Cancel()

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
                state <- RunningMission (DateTime.UtcNow + TimeSpan.FromMinutes(float settings.MissionDuration))
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
                    if gameServer.IsRunning serverProcess then
                        let! s = gameServer.RotateMission()
                        logger.Debug s
                        match s with
                        | Ok() ->
                            state <- RunningMission (DateTime.UtcNow + TimeSpan.FromMinutes(float settings.MissionDuration))
                            logger.Info state
                            state.Save(settings.WorkDir)
                            return! this.ResumeAsync()
                        | Error msg ->
                            return this.Die(msg)
                    else
                        return! this.StartServerAsync()
                | Error msg ->
                    return this.Die(msg)

            | RunningMission deadline ->
                if not(gameServer.IsRunning serverProcess) then
                    return! this.StartServerAsync(restartsLeft)
                else
                // Check that DServer is running every 15s until the deadline has passed
                // If DServer dies before, restart it.
                let rec monitor() =
                    async {
                        let untilDeadLine = (deadline - DateTime.UtcNow).TotalMilliseconds
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
                missionCompleted.Trigger(Ok())
                state <- ExtractingResults
                logger.Info state
                state.Save(settings.WorkDir)
                return! this.ResumeAsync()

            | ExtractingResults ->
                match! Async.AwaitEvent(missionLogsExtracted.Publish, fun () -> this.Die("Interrupted")) with
                | Ok() ->
                    state <- PreparingMission
                    logger.Info state
                    state.Save(settings.WorkDir)
                    return! this.ResumeAsync()
                | Error msg ->
                    // Result extraction failed, try running the mission again
                    return this.Die(msg)
        }

    /// Token to use with ResumeAsync in order for Interrupt to work.
    member this.CancellationToken = cancellation.Token

    /// Start synchronization.
    member this.Resume() =
        Async.StartImmediate(this.ResumeAsync(), this.CancellationToken)

    member this.Dispose() =
        cancellation.Dispose()

    interface IDisposable with
        member this.Dispose() = this.Dispose()