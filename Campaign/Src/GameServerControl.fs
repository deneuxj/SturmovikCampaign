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

/// Interaction with DServer
module Campaign.GameServerControl

open System.Diagnostics
open System
open FSharp.Json

open Campaign.Common.BasicTypes

type IGameServerControl =
    abstract IsRunning : obj option -> Process option
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
        MaxTrainsPerCoalition : int
        MaxTruckConvoysPerCoalition : int
        MaxActivePatrolsPerCoalition : int
        MaxAttackPlanesCpuCost : float32
        MaxAAGuns : int
        MaxBattleArtillery : int
        MaxBattleTanks : int
        MaxBattleAntiTankGuns : int
        MaxBattleRocketArtillery : int
        MinFightersInPlayableMission : int
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

    /// Limits on number of entities in ground battles
    member this.GroundBattleLimits : Campaign.MissionGen.MissionFileGeneration.GroundBattleNumbers =
        {
            NumTanks = this.MaxBattleTanks
            NumRocketArtillery = this.MaxBattleRocketArtillery
            NumArtillery = this.MaxBattleArtillery
            NumAntiTankGuns = this.MaxBattleAntiTankGuns
        }

    static member DefaultWorkDir = IO.Path.Combine(Environment.GetEnvironmentVariable("LOCALAPPDATA"), "CoconutCampaign", "Current")

module IO =
    open Util.Json

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
            max_trains : int option
            max_truck_convoys : int option
            max_active_patrols : int option
            max_attack_planes_cpu : int option
            max_aa : int option
            max_battle_at_guns : int option
            max_battle_tanks : int option
            max_battle_artillery : int option
            max_battle_rocket_artillery : int option
            min_fighters_playable_mission : int option
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
                MaxTrainsPerCoalition = defaultArg this.max_trains 3
                MaxTruckConvoysPerCoalition = defaultArg this.max_truck_convoys 2
                MaxActivePatrolsPerCoalition = defaultArg this.max_active_patrols 4
                MaxAttackPlanesCpuCost = 100.0f * float32(defaultArg this.max_attack_planes_cpu 16)
                MaxAAGuns = defaultArg this.max_aa 1000
                MaxBattleAntiTankGuns = defaultArg this.max_battle_at_guns 15
                MaxBattleArtillery = defaultArg this.max_battle_artillery 15
                MaxBattleRocketArtillery = defaultArg this.max_battle_rocket_artillery 7
                MaxBattleTanks = defaultArg this.max_battle_tanks 7
                MinFightersInPlayableMission = defaultArg this.min_fighters_playable_mission 100
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
                max_active_patrols = None
                max_trains = None
                max_truck_convoys = None
                max_attack_planes_cpu = None
                max_aa = None
                max_battle_tanks = None
                max_battle_artillery = None
                max_battle_at_guns = None
                max_battle_rocket_artillery = None
                min_fighters_playable_mission = None
            }
        let json = Json.serialize content
        IO.File.WriteAllText(path, json)
        content.AsSettings

    /// Load settings from a file, and set the WorkDir value to the directory of the file if it's not set in the file.
    let loadFromFile path =
        let content = IO.File.ReadAllText(path)
        let loaded = Json.deserializeEx<IOSettings> JsonConfig.IL2Default content
        let loaded =
            { loaded with
                work_dir =
                    loaded.work_dir
                    |> Option.orElse (Some(path |> IO.Path.GetDirectoryName |> fun x -> IO.Path.Combine(x, "Current") |> IO.Path.GetFullPath)) }
        loaded.AsSettings


type RConGameServerControl(settings : Settings, ?logger) =
    let mutable logger = defaultArg logger (NLog.LogManager.GetCurrentClassLogger())
    let mutable client = None
    let mutable proc = None

    let connect() =
        async {
            try
                let cl = new RConClient.Client(settings.Address, settings.Port, settings.Login, settings.Password)
                client <- Some cl
                let! s = cl.Auth()
                logger.Info("RCon auth response:" + s)
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
                if IO.Path.IsPathRooted(sds) then
                    sds
                else
                    IO.Path.Combine("..", "..", "data", sds)
            logger.Debug("Path to DServer config file: " + sds)
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
        if procs.Length > 0 then
            if procs.Length > 1 then
                logger.Warn(
                    let pids =
                        procs
                        |> Seq.map (fun proc -> string proc.Id)
                        |> String.concat ", "
                    sprintf "Multiple DServer processes found: %s" pids)
            logger.Info procs.[0]
            proc <- Some procs.[0]
            proc
        else
            None

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
        let rec attempt (attemptsLeft : int) =
            async {
                match client with
                | Some client' ->
                    try
                        return! task client'
                    with
                    | exc ->
                        logger.Debug("Failed RConClient task")
                        logger.Debug(exc)
                        client <- None
                        if attemptsLeft > 0 then
                            return! attempt (attemptsLeft - 1)
                        else
                            return Error "Failed RConClient task"
                | None ->
                    if attemptsLeft > 0 then
                        return! attempt (attemptsLeft - 1)
                    else
                        return Error "No connection to DServer"
            }
        async {
            if client.IsNone then
                let! s = connect()
                logger.Debug(
                    let status =
                        match s with
                        | Ok() -> "OK"
                        | Error s -> "Error: " + s
                    "Status of RCon connection: " + status)
            return! attempt 1
        }

    let rotateMission() =
        tryOnClient <| fun client -> async {
            let! s = client.ServerInput(settings.RotateMissionServerInputName)
            logger.Info("RCON Rotate mission: " + s)
            return Ok ()
        }

    let signalMissionEnd() =
        tryOnClient <| fun client -> async {
            let! s = client.MessageAll "Mission has ended. Actions beyond that point will not affect the campaign."
            logger.Info("RCON Inform mission ended: " + s)
            return Ok()
        }

    let loadSds(sds : string) =
        tryOnClient <| fun client -> async {
            let! s = client.OpenSds(sds)
            logger.Info("RCON Open SDS: " + s)
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
            logger.Debug("Message all: " + msg)
            let! s = client.MessageAll(msg)
            logger.Info s
            return Ok ()
        }

    let messageCoalition(coalition : CoalitionId, msg : string) =
        tryOnClient <| fun client -> async {
            logger.Debug("Message " + string coalition + ": " + msg)
            let team = int coalition.ToCoalition
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
            logger.Debug("Message " + guidOrName + ": " + msg)
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
                return Error "Failed to find player to ban"
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
                if not proc2.HasExited then
                    Some proc2
                else
                    None
            | Some _ ->
                invalidArg "proc2" "Must be of type Process option"
            | None ->
                match proc with
                | Some proc ->
                    if not proc.HasExited then
                        Some proc
                    else
                        None
                | None ->
                    checkIfRunning()

    interface IPlayerNotifier with
        member this.BanPlayer(player) = banPlayer(player)
        member this.KickPlayer(player) = kickPlayer(player)
        member this.MessageAll(msg) = messageAll(msg)
        member this.MessageCoalition(coalition, msg) = messageCoalition(coalition, msg)
        member this.MessagePlayer(player, msg) = messagePlayer(player, msg)
