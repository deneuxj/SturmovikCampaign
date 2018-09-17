// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
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

module Campaign.Commenting

open System
open System.IO
open System.Text.RegularExpressions
open Campaign.BasicTypes
open Campaign.Configuration
open Util
open ploggy
open FSharp.Control
open Campaign.BasicTypes
open Campaign.ResultExtraction
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.PlaneModel
open Campaign.PlayerDiscipline
open Campaign.PlayerHangar
open Campaign.WatchLogs
open MBrace.FsPickler
open Campaign.PlaneAvailabilityChecks
open Campaign.ChatCommands

let private logger = NLog.LogManager.GetCurrentClassLogger()

/// Quickly replay a sequence of events.
/// This is needed to correctly reconstuct results which depend on ordering of events, e.g. collection of rewards for actions during flights
let replayQuick (events : LogEntry seq) =
    asyncSeq {
        for ev in events do
            match ev with
            | :? VersionEntry -> () // Skip those useless entries
            | _ ->
                yield ev
                do! Async.Sleep 10
    }

type EventHandlers =
    // player name, coalition, airfield, coalition of airfield, plane, cargo
    { OnTookOff : string * CoalitionId * AirfieldId * CoalitionId option * PlaneModel * float32<K> -> Async<unit>
      // playername, coalition of player, airfield, coalition of airfield, plane, cargo, health, damages inflicted
      OnLanded : string * CoalitionId * AirfieldId * CoalitionId option * PlaneModel * float32<K> * float32 * float32<E> -> Async<unit>
      // Region name, attacker coalition
      OnMaxBattleDamageExceeded : string * CoalitionId -> Async<unit>
      // A player was banned or kicked
      OnPlayerPunished : Judgement -> Async<unit>
      // Send messages to a player
      OnMessagesToPlayer : UserIds * string list -> Async<unit>
      // Send messages to a coalition
      OnMessagesToCoalition : CoalitionId * string list -> Async<unit>
      // Update planeset at airfield
      OnPlaneSetChanged : AirfieldId * int -> Async<unit>
      // Update player hangars
      OnHangarsUpdated : Map<string * CoalitionId, PlayerHangar> -> Async<unit>
      // A player entered
      OnPlayerEntered : Guid -> Async<unit>
    }

let private findLogFiles(stateDate, missionLogsDir) =
    let files =
        let unordered = Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt")
        let r = Regex(@"(missionReport\(.*\))\[([0-9]+)\]\.txt")
        let filtered =
            unordered
            |> Seq.choose(fun path ->
                let m = r.Match(Path.GetFileName(path))
                if not m.Success then
                    None
                else
                    Some(path, (m.Groups.[1].ToString(), System.Int32.Parse(m.Groups.[2].ToString()))))
        let groupHasCorrectDate group =
            let _, files = group
            files
            |> Seq.collect (fun (path, _) ->
                File.ReadAllLines(path)
                |> Seq.choose (LogEntry.Parse >> Option.ofObj))
            |> Seq.exists (
                function
                | :? MissionStartEntry as start ->
                    start.MissionTime = stateDate
                | _ -> false)
        let lastGroup =
            filtered
            |> Seq.groupBy (snd >> fst)
            |> Seq.filter groupHasCorrectDate
            |> Seq.tryLast
            |> Option.map snd
            |> Option.defaultVal Seq.empty
        let sorted =
            lastGroup
            |> Seq.sortBy snd
            |> Seq.map fst
        sorted
        |> Array.ofSeq
    files

/// Skip elements in an asyncSeq xs for as long as old mission log entries are yielded, then iterate over items in xs
// This is used when starting a commenter while a mission is ongoing to avoid reacting to the old mission log entries
let inline private asyncIterNonMuted asyncSeqEntries f xs =
    AsyncSeq.mergeChoice xs asyncSeqEntries
    |> AsyncSeq.skipWhile(
        function
        | Choice1Of2 _ -> true
        | Choice2Of2(Old _) -> true
        | Choice2Of2(Fresh _) -> false)
    |> AsyncSeq.choose(
        function
        | Choice1Of2 x -> Some x
        | Choice2Of2 _ -> None)
    |> AsyncSeq.iterAsync f

let private updateAirfieldPlaneset(allowCapturedPlanes, wg : WorldFastAccess, sg : WorldStateFastAccess, handlers, afId, numPlanes : Map<PlaneModel, float32>) =
    // State of airfield at start of mission
    let afs = sg.GetAirfield(afId)
    match sg.GetRegion(wg.GetAirfield(afId).Region).Owner with
    | Some coalition ->
        // Compute plane index: For each plane initially found in the spawn, check if it's now available.
        // If so, include it in the plane index, which is a bit mask.
        let planeSetIndex =
            let origNumPlanes =
                afs.NumPlanes
                |> Map.map (fun _ qty -> int(floor(qty)))
            let spawnPlanes, _ =
                let coalitionFilter =
                    if allowCapturedPlanes then
                        None
                    else
                        Some coalition
                Airfield.splitPlaneSpawns coalitionFilter origNumPlanes
            spawnPlanes
            |> Array.fold(fun (res, index) plane ->
                let qty = numPlanes.TryFind(plane) |> Option.defaultValue 0.0f
                if qty >= 1.0f then
                    (res ||| index, index <<< 1)
                else
                    (res, index <<< 1)) (0, 1)
            |> fst
        logger.Info(sprintf "Change to planeset %d at %s" planeSetIndex afId.AirfieldName)
        handlers.OnPlaneSetChanged(afId, planeSetIndex)
    | None ->
        async.Zero()

let private initAirfieldPlanesets(config : Configuration, wg : WorldFastAccess, sg : WorldStateFastAccess, handlers, hangars, files) =
    async {
        let airfields0 =
            sg.State.Airfields
            |> List.map (fun afs -> afs.AirfieldId, afs.NumPlanes)
            |> Map.ofList
        let limits  = Limits.FromConfig config
        let! airfields =
            files
            |> Array.collect(File.ReadAllLines)
            |> Array.choose(LogEntry.Parse >> Option.ofObj)
            |> AsyncSeq.ofSeq
            |> checkPlaneAvailability limits wg.World sg.State hangars
            |> AsyncSeq.fold (fun acc ->
                function
                | PlanesAtAirfield(af, planes) -> Map.add af planes acc
                | _ -> acc) airfields0
        for (af, planes) in Map.toSeq airfields do
            do! updateAirfieldPlaneset(config.MaxCapturedPlanes > 0, wg, sg, handlers, af, planes)
    } |> Async.RunSynchronously

let private mkBattleDamageTask asyncIterNonMuted (config : Configuration, wg : WorldFastAccess, sg : WorldStateFastAccess, handlers, asyncSeqEntries) : Async<unit> =
    let world = wg.World
    let state = sg.State
    let battleDamages =
        let battles =
            Battlefield.identifyBattleAreas world state
            |> Seq.cache
        asyncSeqEntries
        |> extractBattleDamages world state battles
        |> AsyncSeq.filter (fun damage -> damage.KilledByPlayer.IsSome)
    asyncSeq {
        let battleDamage = ref Map.empty
        for battleKill in battleDamages do
            let oldBattleDamage =
                battleDamage.Value.TryFind (battleKill.BattleId, battleKill.Coalition)
                |> Option.defaultValue 0.0f<E>
            let newDamage = oldBattleDamage + battleKill.Vehicle.Cost / (float32 config.BattleKillRatio)
            battleDamage := Map.add (battleKill.BattleId, battleKill.Coalition) newDamage battleDamage.Value
            let totalValue =
                GroundAttackVehicle.AllVehicles
                |> Seq.sumBy (fun vehicle ->
                    (float32(sg.GetRegion(battleKill.BattleId).GetNumVehicles(battleKill.Coalition, vehicle))) * vehicle.Cost)
            if newDamage > totalValue * config.MaxBattleKillsRatioByPlayers then
                yield battleKill.Coalition.Other, battleKill.BattleId
    }
    |> asyncIterNonMuted (fun (coalition, region) -> handlers.OnMaxBattleDamageExceeded(string region, coalition))

let private mkHangarTask asyncIterNonMuted (config : Configuration, wg : WorldFastAccess, sg : WorldStateFastAccess, hangars, handlers, asyncCommands, asyncSeqEntries) =
    let world = wg.World
    let state = sg.State
    asyncSeqEntries
    |> checkPlaneAvailability (Limits.FromConfig config) world state hangars 
    |> AsyncSeq.mergeChoice asyncCommands
    |> AsyncSeq.scan (fun (_, hangars : Map<string * CoalitionId, PlayerHangar>, airfields : Map<AirfieldId, Map<PlaneModel, float32>>) item ->
        match item with
        | Choice1Of2 cmd ->
            let userIds =
                hangars
                |> Map.toSeq
                |> Seq.map snd
                |> Seq.filter (fun hangar -> hangar.PlayerName = cmd.Player)
                |> List.ofSeq
            let uid =
                match userIds with
                | [hangar] ->
                    { UserId = string hangar.Player; Name = hangar.PlayerName }
                | _ ->
                    // Use a fake Guid, OK because the player name can be used instead.
                    { UserId = string (Guid("00000000-0000-0000-0000-000000000000")); Name = cmd.Player }
            Some(Overview(uid, 0, cmd.Interpret(hangars))), hangars, airfields
        | Choice2Of2 (Status(hangars, airfields) as x) ->
            Some x, hangars, airfields
        | Choice2Of2 x ->
            Some x, hangars, airfields
    ) (None, Map.empty, Map.empty)
    |> AsyncSeq.choose (function (Some x, _, _) -> Some x | _ -> None)
    |> asyncIterNonMuted (
        function
        | PlayerEntered(userId) ->
            async {
                Async.Start(
                    async {
                        do! Async.Sleep(15000)
                        return! handlers.OnPlayerEntered(userId)
                    })
            }
        | Overview(user, delay, messages) ->
            async {
                Async.Start(
                    async {
                        do! Async.Sleep(delay * 1000)
                        return! handlers.OnMessagesToPlayer(user, messages)
                    })
            }
        | Warning(user, delay, messages) ->
            async {
                Async.Start(
                    async {
                        do! Async.Sleep(delay * 1000)
                        return! handlers.OnMessagesToPlayer(user, messages)
                    })
            }
        | Announce(coalition, messages) ->
            handlers.OnMessagesToCoalition(coalition, messages)
        | Violation(user, reason) ->
            async {
                do! handlers.OnMessagesToPlayer(user, [sprintf "You are being kicked for %s. This is not a ban, you are welcome back." reason])
                Async.Start(
                    async {
                        do! Async.Sleep(10000)
                        do! handlers.OnPlayerPunished({ Player = user; Decision = Kicked })
                    })
            }
        | PlanesAtAirfield(afId, numPlanes) ->
            updateAirfieldPlaneset(config.MaxCapturedPlanes > 0, wg, sg, handlers, afId, numPlanes)
        | Status(hangars, _) ->
            handlers.OnHangarsUpdated(hangars)
        )

let mkTimeTask(config : Configuration, handlers, files) =
    let rec remainingTime (t : System.TimeSpan) =
        async {
            let msg =
                [sprintf "Time left in mission: %d hours %d minutes" t.Hours t.Minutes]
            logger.Info(msg)
            do! handlers.OnMessagesToCoalition(Axis, msg)
            do! handlers.OnMessagesToCoalition(Allies, msg)
            let quarter = System.TimeSpan(0, 15, 0)
            if t > quarter then
                do! Async.Sleep (int quarter.TotalMilliseconds)
                return! remainingTime (t - quarter)
        }

    let remainingTime =
        async {
            let timePassed =
                // Get timestamp of last entry
                files
                |> Seq.collect (File.ReadLines)
                |> Seq.rev
                |> Seq.map (LogEntry.Parse)
                |> Seq.tryPick (
                    function
                    | null -> None
                    | :? VersionEntry -> None
                    | entry -> Some entry.Timestamp)
                |> Option.defaultValue (System.TimeSpan())
            let t =
                System.TimeSpan.FromMinutes(float config.MissionLength) - timePassed
            return! remainingTime t
        }

    remainingTime

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (config : Configuration, handlers : EventHandlers) =
    let missionLogsDir = Path.Combine(config.ServerDataDir, "logs")
    let campaignDir = config.OutputDir

    let world, state =
        let serializer = FsPickler.CreateXmlSerializer()
        try
            use worldFile = File.OpenText(Path.Combine(campaignDir, "world.xml"))
            use stateFile = File.OpenText(Path.Combine(campaignDir, "state.xml"))
            serializer.Deserialize<World>(worldFile),
            serializer.Deserialize<WorldState>(stateFile)
        with
        | _ -> failwith "Failed to load world and state"

    let hangars =
        tryLoadHangars (Path.Combine(campaignDir, "hangars.xml"))
        |> Option.defaultValue Map.empty
        |> guidToStrings

    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()

    // retrieve entries from most recent mission
    let files = findLogFiles(state.Date, missionLogsDir)
    let asyncSeqEntries =
        let handleOldEntries ss =
            ss
            |> Seq.map (LogEntry.Parse)
            |> Seq.filter (function null -> false | _ -> true)
            |> Seq.map (fun entry -> Old(entry.OriginalString))
            |> AsyncSeq.ofSeq
        resumeWatchlogs(handleOldEntries, missionLogsDir, "missionReport*.txt", files, cancelOnDispose.Token)
    let asyncIterNonMuted action xs = asyncIterNonMuted asyncSeqEntries action xs

    // The log entries fed to the tasks
    let asyncSeqEntries =
        asyncSeqEntries
        |> AsyncSeq.choose (fun line ->
            try
                let x = LogEntry.Parse(string line)
                match x with
                | null -> failwith "null LogEntry"
                | x when x.IsValid() -> Some x
                | invalid -> failwith "non-null invalid LogEntry"
            with
            | _ ->
                logger.Error(sprintf "Failed to parse '%s'" (string line))
                None)
        |> AsyncSeq.filter (function null -> false | _ -> true)

    // The commands typed by players in the game chat
    let asyncCommands =
        if config.ChatLogCommandsEnabled then
            watchCommands(Path.Combine(config.ServerDataDir, "chatlogs"), cancelOnDispose.Token)
            |> AsyncSeq.map (fun line -> ChatCommand.Parse(world, line))
        else
            AsyncSeq.empty

    let wg = world.FastAccess
    let sg = state.FastAccess

    do
        let battleLimitsTask = mkBattleDamageTask asyncIterNonMuted (config, wg, sg, handlers, asyncSeqEntries)

        let disciplineTask =
            asyncSeqEntries
            |> disciplinePlayers config world state
            |> asyncIterNonMuted (fun penalty -> handlers.OnPlayerPunished penalty)

        initAirfieldPlanesets(config, wg, sg, handlers, hangars, files)

        let hangarTask = mkHangarTask asyncIterNonMuted (config, wg, sg, hangars, handlers, asyncCommands, asyncSeqEntries)

        let remainingTime = mkTimeTask(config, handlers, files)

        Async.Start(Async.catchLog "battle limits notifier" battleLimitsTask, cancelOnDispose.Token)
        Async.Start(Async.catchLog "abuse detector" disciplineTask, cancelOnDispose.Token)
        Async.Start(Async.catchLog "plane availability checker" hangarTask, cancelOnDispose.Token)
        Async.Start(Async.catchLog "remaining time notifier" remainingTime, cancelOnDispose.Token)

    member this.Dispose() =
        logger.Info("Commentator disposed")
        cancelOnDispose.Cancel()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

