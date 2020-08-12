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
open System.IO
open System.Diagnostics
open NLog
open FSharp.Json

open FSharp.Control

open Campaign
open Campaign.BasicTypes
open Campaign.NewWorldDescription
open Campaign.NewWorldDescription.IO
open Campaign.WarState
open Campaign.WarState.IO
open Campaign.BasicTypes
open Util
open Campaign.CampaignScenario
open Campaign.CampaignScenario.IO
open Campaign.WarStateUpdate.CommandExecution

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
    /// Root of the data dir, passed to resaver.exe
    member this.GameDataPath = IO.Path.Combine(this.GameDir, "data")

    /// Directory where the mission file is generated
    member this.MissionPath = IO.Path.Combine(this.WorkDir, "Staging", "data", "Multiplayer", "Dogfight")

    /// Directory where the generated mission is copied
    member this.GameMissionPath = IO.Path.Combine(this.GameDir, "data", "Multiplayer", "Dogfight")

    /// Filename without extension of the first variant of the generated mission
    member this.MissionFile = sprintf "%s_1" this.MissionBaseName

    /// Path to the first variant of the generated mission
    member this.MissionFilePath = IO.Path.Combine(this.MissionPath, this.MissionFile) + ".Mission"

    /// Filename without extension of the alternative generated mission
    member this.AltMissionFile = sprintf "%s_2" this.MissionBaseName

    /// Directory where DServer saves logs
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

        static member Delete(workDir : string) =
            IO.File.Delete(IO.Path.Combine(workDir, SyncState.FileName))

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

type IPlayerNotifier =
    abstract MessageAll : string -> Async<Result<unit, string>>
    abstract MessageCoalition : CoalitionId * string -> Async<Result<unit, string>>
    abstract MessagePlayer : guidOrName:string * message:string -> Async<Result<unit, string>>
    abstract KickPlayer : string -> Async<Result<unit, string>>
    abstract BanPlayer : string -> Async<Result<unit, string>>

/// Inform players of interesting events while a game is going on
type LiveNotifier(commands : AsyncSeq<WarStateUpdate.Commands>, war : WarState, notifier : IPlayerNotifier) =
    let mutable isMuted = true

    member this.UnMute() = isMuted <- false

    member this.Run() =
        commands
        |> AsyncSeq.iterAsync(fun command -> async {
            if not isMuted then
                match command with
                | WarStateUpdate.Commands.RegisterPilotFlight(pid, flight, health) ->
                    let pilot = war.GetPilot(pid)
                    let rank =
                        Pilots.tryComputeRank war.World.Ranks pilot
                        |> Option.map (fun rank -> rank.RankAbbrev)
                        |> Option.defaultValue ""
                    let eventDescription =
                        match flight.Return with
                        | Targets.CrashedInEnemyTerritory -> "crashed in enemy territory"
                        | Targets.CrashedInFriendlyTerritory _ -> "crash-landed"
                        | Targets.AtAirfield afId -> sprintf "landed at %s" afId.AirfieldName
                    let msg = sprintf "%s %s %s (%d) has %s" rank pilot.PilotFirstName pilot.PilotLastName pid.AsInt eventDescription
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
                | WarStateUpdate.Commands.UpdatePilot(pilot) ->
                    let player =
                        match war.TryGetPlayer(pilot.PlayerGuid) with
                        | Some player -> player.Name
                        | None -> "<incognito>"
                    let rank =
                        Pilots.tryComputeRank war.World.Ranks pilot
                        |> Option.map (fun rank -> rank.RankAbbrev)
                        |> Option.defaultValue ""
                    let msg = sprintf "%s has taken control of %s %s %s (%d)" player rank pilot.PilotFirstName pilot.PilotLastName pilot.Id.AsInt
                    let! s = notifier.MessageAll(msg)
                    ()
                | _ ->
                    ()
            command.Execute(war) |> ignore
        })

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

    let tryOnClient task =
        async {
            if client.IsNone then
                let! _ = connect()
                ()
            match client with
            | Some client ->
                try
                    return! task client
                with
                | exc ->
                    logger.Debug(exc)
                    return Error "Failed RConClient task"
            | None ->
                return Error "No connection to DServer"
        }

    let rotateMission() =
        tryOnClient <| fun client -> async {
            let! s = client.ServerInput(settings.RotateMissionServerInputName)
            logger.Info s
            return Ok ()
        }

    let signalMissionEnd() =
        tryOnClient <| fun client -> async {
            let! s = client.MessageAll "Mission has ended. Actions beyond that point will not affect the campaign."
            logger.Info s
            return Ok()
        }

    let loadSds(sds : string) =
        tryOnClient <| fun client -> async {
            let! s = client.OpenSds(sds)
            logger.Info s
            return Ok ()
        }

    let resaveMission(filename : string) =
        async {
            try
                // Copy mission files to game's dir
                for file in IO.Directory.EnumerateFiles(settings.MissionPath, sprintf "%s.*" filename) do
                    try
                        IO.File.Copy(file, IO.Path.Combine(settings.GameMissionPath, IO.Path.GetFileName(file)), true)
                    with
                    | exc ->
                        logger.Warn(sprintf "Failed to copy %s: %s" file exc.Message)
                        logger.Warn(exc)
                        failwith "Failed to copy mission to game's dir"

                let resaverDir = IO.Path.Combine(settings.GameDir, "bin", "resaver")
                let path = IO.Path.Combine(settings.GameMissionPath, filename) + ".Mission"
                let p = ProcessStartInfo("MissionResaver.exe", sprintf "-d \"%s\" -f \"%s\"" settings.GameDataPath path)
                p.WorkingDirectory <- resaverDir
                p.UseShellExecute <- false
                p.RedirectStandardError <- true
                p.RedirectStandardOutput <- true
                let oldCwd = Environment.CurrentDirectory
                let proc =
                    try
                        Environment.CurrentDirectory <- resaverDir
                        Process.Start(p)
                    finally
                        Environment.CurrentDirectory <- oldCwd
                logger.Debug proc.StartInfo.Arguments
                let rec awaitExited() =
                    async {
                        if not proc.HasExited then
                            do! Async.Sleep(5000)
                            return! awaitExited()
                    }
                do! awaitExited()
                logger.Info(proc.StandardOutput.ReadToEnd())
                logger.Warn(proc.StandardError.ReadToEnd())
                if proc.ExitCode <> 0 then
                    logger.Info proc
                    return Error "Resaver failed"
                else
                    let listFile = IO.Path.GetFileNameWithoutExtension(path) + ".list"
                    try
                        // Check that the list file exists, and contains at least one line
                        IO.File.ReadAllLines(IO.Path.Combine(settings.GameMissionPath, listFile)).[0] |> ignore
                        // Remove text mission file, forcing DServer to use the msnbin file, which is faster
                        try
                            IO.File.Delete(IO.Path.Combine(settings.GameMissionPath, filename) + ".Mission")
                        with exc ->
                            logger.Warn("Failed to delete text mission file after resaving")
                            logger.Warn(exc)
                        return Ok()
                    with _ ->
                        return Error "Failed to produce non-empty list file"
            with
            | e ->
                logger.Error e
                return Error <| sprintf "ResaveMission failed: %s" e.Message
        }

    let copyRenameMission(filename : string) =
        async {
            try
                let dirname = settings.MissionPath
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

    let messageAll(msg : string) =
        tryOnClient <| fun client -> async {
            let! s = client.MessageAll(msg)
            logger.Info s
            return Ok ()
        }

    let messageCoalition(coalition : CoalitionId, msg : string) =
        tryOnClient <| fun client -> async {
            let team =
                match coalition with
                | Axis -> 1
                | Allies -> 2
            let! s = client.MessageTeam(team, msg)
            logger.Info s
            return Ok ()
        }

    let getPlayer(client : RConClient.Client, guidOrName : string) =
        async {
            match! client.GetPlayerList() with
            | Some players ->
                match
                    (players
                     |> Array.tryFind (fun player ->
                        player.PlayerId = guidOrName ||
                        player.ProfileId = guidOrName ||
                        player.Name = guidOrName)
                    )
                    with
                | Some player ->
                    return Some player
                | None ->
                    return None
            | _ -> return None
        }

    let messagePlayer(guidOrName : string, msg : string) =
        tryOnClient <| fun client -> async {
            match! getPlayer(client, guidOrName) with
            | Some player ->
                let! s = client.MessagePlayer(player.ClientId, msg)
                logger.Info s
                return Ok ()
            | None ->
                return Error "Failed to find player to message"
        }

    let kickPlayer(guidOrName : string) =
        tryOnClient <| fun client -> async {
            match! getPlayer(client, guidOrName) with
            | Some player ->
                let! s = client.KickPlayer(player.ClientId)
                logger.Info s
                return Ok()
            | None ->
                return Error "Failed to find player to kick"
        }

    let banPlayer(guidOrName : string) =
        tryOnClient <| fun client -> async {
            match! getPlayer(client, guidOrName) with
            | Some player ->
                let! s = client.BanPlayer(player.ClientId)
                logger.Info s
                return Ok()
            | None ->
                return Error "Failed to find player to kick"
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

        member this.ResaveMission(filename) =
            resaveMission(filename)

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

    interface IPlayerNotifier with
        member this.BanPlayer(player) = banPlayer(player)
        member this.KickPlayer(player) = kickPlayer(player)
        member this.MessageAll(msg) = messageAll(msg)
        member this.MessageCoalition(coalition, msg) = messageCoalition(coalition, msg)
        member this.MessagePlayer(player, msg) = messagePlayer(player, msg)

module BaseFileNames =
    open System.Text.RegularExpressions
    open RegexActivePatterns

    let worldFilename = "world.xml"
    let stateBaseFilename = "-state.xml"
    let stepBaseFilename = "-step.xml"
    let simulationBaseFilename = "-simulation.xml"
    /// Files that contain state update commands extracted from the game logs, and the results of the commands
    let effectsBaseFilename = "-effects.xml"

    let getStateFilename idx = sprintf "%03d%s" idx stateBaseFilename
    let getStepFilename idx = sprintf "%03d%s" idx stepBaseFilename
    let getSimulationFilename idx = sprintf "%03d%s" idx simulationBaseFilename
    let getEffectsFilename idx = sprintf "%03d%s" idx effectsBaseFilename

    /// Get the highest N such that N-state.xml exists
    let getCurrentIndex (path : string) =
        let pattern = Regex(@"^(\d*)-state.xml")
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

open BaseFileNames
open IO

/// Controls execution of DServer, depending on status of campaign scenario controller.
type Sync(settings : Settings, gameServer : IGameServerControl, ?logger) =
    let mutable logger = defaultArg logger (LogManager.GetCurrentClassLogger())
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

    let wkPath f = Path.Combine(settings.WorkDir, f)

    /// Get the current synchronization state
    member this.SyncState = state

    /// Campaign scenario controller subscribes to this event to know when sync is terminated
    member this.Terminated = terminated.Publish

    /// Campaign scenario controller subscribes to this event to know when war state has changed
    member this.StateChanged = stateChanged.Publish

    /// Controls whether the workflow should be terminated when the current mission (or the one being prepared) ends.
    member this.StopAfterMission
        with get() = stopAfterMission
        and set x = stopAfterMission <- x

    /// Get a seed computed from the state of the war
    member this.Seed =
        match war with
        | Some war -> int (war.Date.Ticks &&& 0x7FFFFFFFL)
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
                    Directory.EnumerateFiles(settings.WorkDir, "*.xml")
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
                // TODO. For now we just create a controller for Bodenplatte
                let controller0 =
                    match war0 with
                    | Some war ->
                        let sctrl : IScenarioController =
                            let planeSet = BodenplatteInternal.PlaneSet.Default
                            upcast(Bodenplatte(war.World, BodenplatteInternal.Constants.Default, planeSet))
                        Some sctrl
                    | None ->
                        None

                // Load scenario state
                let latestStep =
                    Directory.EnumerateFiles(settings.WorkDir, "*.xml")
                    |> Seq.filter (fun s -> s.EndsWith(stepBaseFilename))
                    |> Seq.sortDescending
                    |> Seq.tryHead
                let step0 =
                    match controller0, war0 with
                    | Some sctrl, Some state ->
                        match latestStep with
                        | Some path ->
                            use reader = new StreamReader(path)
                            let step = ScenarioStep.Deserialize(reader)
                            Some step
                        | None ->
                            None
                    | _ ->
                        None

                let syncState =
                    SyncState.TryLoad(settings.WorkDir)
                    |> Option.defaultValue (PreparingMission settings.MissionFilePath)

                controller <- controller0
                war <- war0
                step <- step0
                state <- Some syncState

                return Ok()
            with
            | exc ->
                logger.Debug("Failed to initialize:")
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
                let names =
                    let path = Path.Combine("Config", "names.json")
                    if File.Exists path then
                        PilotRanks.NameDatabase.FromFile path
                    else
                        PilotRanks.NameDatabase.Default
                match world with
                | Error e ->
                    Error e
                | Ok world ->
                    let world = { world with Names = names }
                    let (world, sctrl : IScenarioController, axisPlanesFactor, alliesPlanesFactor) =
                        let planeSet = BodenplatteInternal.PlaneSet.Default
                        let world = planeSet.Setup world
                        world, upcast(Bodenplatte(world, BodenplatteInternal.Constants.Default, planeSet)), 1.5f, 1.0f
                    let state0 = Init.mkWar world
                    sctrl.InitAirfields(axisPlanesFactor, Axis, state0)
                    sctrl.InitAirfields(alliesPlanesFactor, Allies, state0)
                    let step = sctrl.Start state0
                    Ok(world, state0, step, sctrl)

            let writeData (world : World, state0 : WarState, step0 : ScenarioStep, sctrl : IScenarioController) =
                try
                    world.SaveToFile(wkPath worldFilename)
                    state0.SaveToFile(wkPath(getStateFilename 0))
                    step0.SaveToFile(wkPath(getStepFilename 0))
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
                // Plan next round
                let advance = sctrl.NextStep(stepData)
                let nextStep = advance war
                // Write war state and campaign step files
                let stateFile, stepFile, simFile =
                    Seq.initInfinite (fun i -> (wkPath(getStateFilename i), wkPath(getStepFilename i), wkPath(getSimulationFilename i)))
                    |> Seq.find (fun (stateFile, stepFile, simFile) ->
                        [stateFile; stepFile; simFile]
                        |> Seq.forall (File.Exists >> not))
                war.SaveToFile(stateFile)
                stateChanged.Trigger(war.Clone())
                nextStep.SaveToFile(stepFile)
                use writer = new StreamWriter(simFile)
                let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
                serializer.SerializeSequence(writer, results) |> ignore
                step <- Some nextStep
                return Ok(results)
            | _, _, Some _ ->
                return (Error "Cannot advance campaign, it has reached its final state")
            | _ ->
                return (Error "Campaign data missing")
        }

    /// Prepare mission file
    member this.PrepareMission() =
        async {
            match state with
            | Some(PreparingMission _)-> ()
            | _ -> failwith "State is not PreparingMission"

            match controller, step, war with
            | Some(ctrl), Some(Ongoing stepData), Some state ->
                try
                    let selection = ctrl.SelectMissions(stepData, state)
                    let random = System.Random(this.Seed)
                    let missionGenSettings : MissionFileGeneration.MissionGenSettings =
                        {
                            MissionFileGeneration.MaxAiPatrolPlanes = 6
                            MissionFileGeneration.MaxAntiAirCannons = 100
                            MissionFileGeneration.OutFilename = settings.MissionFilePath
                        }
                    let mission = MissionFileGeneration.mkMultiplayerMissionContent random stepData.Briefing state selection
                    mission.BuildMission(random, missionGenSettings, state)
                    return Ok()
                with
                e -> return (Error e.Message)
            | _ ->
                return (Error "Cannot select missions in the current state")
        }

    /// Extract mission log
    member this.ExtractMissionLog(firstLogFile : string) =
        async {
            match state with
            | Some(ExtractingResults _) -> ()
            | _ -> failwith "State is not ExtractingResults"

            match war with
            | Some war ->
                // Find initial log file created right after the mission was started
                let dir = System.IO.Path.GetDirectoryName(firstLogFile)
                let filename = System.IO.Path.GetFileNameWithoutExtension(firstLogFile)
                // Replace final [0] by * in the pattern, and add .txt extension
                let pattern = filename.Substring(0, filename.Length - "[0]".Length) + "*.txt"
                let lines =
                    asyncSeq {
                        let files =
                            System.IO.Directory.EnumerateFiles(dir, pattern)
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
                        for command in commands do
                            try
                                logger.Debug("Command from game logs: " + Json.serialize command)
                            with exc ->
                                logger.Debug("Command from game logs.")
                                logger.Debug(exc)
                            let effects = command.Execute(war)
                            yield (command, effects)
                    }
                    |> AsyncSeq.toArrayAsync
                stateChanged.Trigger(war.Clone())
                // Write effects to file
                // Write war state and campaign step files
                let effectsFile =
                    Seq.initInfinite (fun i -> (wkPath(getEffectsFilename i), wkPath(getStateFilename i)))
                    |> Seq.pairwise
                    |> Seq.pick (fun ((effectsFile, _), (_, stateFile)) -> if not(File.Exists stateFile) then Some effectsFile else None)
                let results =
                    effects
                    |> Seq.map (fun (cmd, results) -> "From played mission", cmd, results)
                    |> Array.ofSeq
                use writer = new StreamWriter(effectsFile)
                let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
                serializer.SerializeSequence(writer, results) |> ignore
                return Ok()
            | None ->
                return (Error "No war state")
        }

    /// Check current state and act accordingly
    member this.ResumeAsync(?restartsLeft) =
        let restartsLeft = defaultArg restartsLeft maxRetries
        async {
            logger.Trace restartsLeft
            match state with
            | None
            | Some(PreparingMission _)->
                let! status = this.PrepareMission()
                match status with
                | Ok() ->
                    state <- Some ResavingMission
                    logger.Info state
                    this.SaveState()
                    return! this.ResumeAsync()
                | Error msg ->
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
                if gameServer.IsRunning serverProcess then
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
                        return this.Die(msg)
                else
                    return! this.StartServerAsync()

            | Some(RunningMission(startTime, endTime)) ->
                if not(gameServer.IsRunning serverProcess) then
                    return! this.StartServerAsync(restartsLeft)
                else

                // Find the set of log files to use. Pick the first file with suffix [0] that is newer than the mission start time.
                let tryGetLatestStartingMissionReport() =
                    try
                        IO.Directory.GetFiles(settings.MissionLogs, "missionReport*.txt")
                        |> Seq.filter (fun file -> IO.Path.GetFileNameWithoutExtension(file).EndsWith("[0]") && IO.File.GetCreationTimeUtc(file) > startTime)
                        |> Seq.minBy (fun file -> IO.File.GetCreationTimeUtc(file))
                        |> Some
                    with _ -> None

                let! latestStartingMissionReport =
                    let rec keepTrying() =
                        async {
                            match tryGetLatestStartingMissionReport() with
                            | Some x -> return x
                            | None ->
                                do! Async.Sleep(15000)
                                return! keepTrying()
                        }
                    keepTrying()

                let! cancelLiveReporting =
                    async {
                        // Start live reporting
                        match war, gameServer with
                        | Some war, (:? IPlayerNotifier as messaging) ->
                            let commands =
                                let basename = Path.GetFileNameWithoutExtension(latestStartingMissionReport.Substring(0, latestStartingMissionReport.IndexOf('[')))
                                asyncSeq {
                                    for log in WatchLogs.watchLogs settings.MissionLogs basename DateTime.UtcNow do
                                        match log with
                                        | WatchLogs.Old x | WatchLogs.Fresh x -> yield x
                                }
                                |> MissionResults.commandsFromLogs war
                            let liveReporter = LiveNotifier(commands, war, messaging)
                            let cancellation = new Threading.CancellationTokenSource()
                            Async.StartImmediate(liveReporter.Run(), cancellation.Token)
                            // Give time to execute old commands
                            do! Async.Sleep(15000)
                            liveReporter.UnMute()
                            return fun () -> cancellation.Cancel()
                        | _ ->
                            logger.Warn("No live notifier started")
                            return ignore
                    }

                let! ourCancellation = Async.CancellationToken
                ourCancellation.Register(fun () -> cancelLiveReporting()) |> ignore

                // Check that DServer is running every 15s until the deadline has passed
                // If DServer dies before, restart it.
                let rec monitor() =
                    async {
                        let untilDeadLine = (endTime - DateTime.UtcNow).TotalMilliseconds
                        if untilDeadLine > 0.0 then
                            if not (gameServer.IsRunning serverProcess) then
                                cancelLiveReporting()
                                return! this.StartServerAsync(restartsLeft)
                            else
                                try
                                    do! Async.Sleep(15000)
                                    return! monitor()
                                with
                                | :? OperationCanceledException ->
                                    return this.Die("Interrupted")
                        else
                            cancelLiveReporting()
                            return()
                    }
                do! monitor()
                let! _ = gameServer.SignalMissionEnd()
                state <- Some(ExtractingResults latestStartingMissionReport)
                logger.Info state
                this.SaveState()
                if not stopAfterMission then
                    return! this.ResumeAsync()
                else
                    this.Interrupt("Stop after mission", true)
                    this.Die("Terminate sync after mission")
                    return ()

            | Some(ExtractingResults path) ->
                let! status = this.ExtractMissionLog(path)
                match status with
                | Ok() ->
                    state <- Some AdvancingScenario
                    logger.Info state
                    this.SaveState()
                    return! this.ResumeAsync()
                | Error msg ->
                    // Result extraction failed, stop sync.
                    return this.Die(msg)

            | Some AdvancingScenario ->
                let! status = this.Advance()
                match status with
                | Ok _ ->
                    state <- Some(PreparingMission settings.MissionFilePath)
                    logger.Info state
                    this.SaveState()
                    return! this.ResumeAsync()
                | Error msg ->
                    // Scenario failed, stop sync.
                    return this.Die(msg)

        }

    /// Token to use with ResumeAsync in order for Interrupt to work.
    member this.CancellationToken = cancellation.Token

    /// Start synchronization and resume flow, unless already running.
    member this.Resume() =
        if not isRunning then
            isRunning <- true
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