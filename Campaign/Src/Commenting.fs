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
open Campaign.PlaneChecksContext

let private logger = NLog.LogManager.GetCurrentClassLogger()

type EventHandlers =
    {
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

let private tryGetMissionLogBaseName(stateDate, missionLogsDir) =
    let now = DateTime.UtcNow
    let fileInGoodGroup =
        Directory.EnumerateFilesAsync(missionLogsDir, "missionReport*.txt")
        |> AsyncSeq.scan (fun state filename ->
            let path = Path.Combine(missionLogsDir, filename)
            match state with
            | Choice1Of2 validGroup ->
                let validGroup =
                    match Regex.Match(filename, @"missionReport\(.*\)\[0\].txt") with
                    | m when m.Success ->
                        // First file in a group, check if it corresponds to the correct date
                        File.ReadAllLines(path)
                        |> Seq.choose (LogEntry.Parse >> Option.ofObj)
                        |> Seq.exists (
                            function
                            | :? MissionStartEntry as start -> start.MissionTime = stateDate
                            | _ -> false)
                    | _ -> validGroup
                if validGroup then
                    let fileInfo = FileInfo(path)
                    if fileInfo.LastWriteTimeUtc > now then
                        Choice2Of2 filename
                    else
                        Choice1Of2 true
                else
                    Choice1Of2 false
            | Choice2Of2 _ ->
                state
        ) (Choice1Of2 false)
        |> AsyncSeq.tryPick (function Choice2Of2 filename -> Some filename | _ -> None)
    async {
        let! filename = fileInGoodGroup
        match filename with
        | Some filename ->
            match Regex.Match(filename, @"(missionReport\(.*\))\[\d+\].txt") with
            | m when m.Success ->
                return Some m.Groups.[1].Value
            | _ ->
                logger.Error("Unexpected string mismatch when getting mission log basename")
                return None
        | None ->
            logger.Error("Unexpected early termination of getMissionLogBaseName without finding a good mission")
            return None
    }


let private updateAirfieldPlaneset(allowCapturedPlanes, wg : WorldFastAccess, sg : WorldStateFastAccess, handlers, afId, numPlanes : Map<PlaneModel, float32>) =
    // State of airfield at start of mission
    let afs = sg.GetAirfield(afId)
    let region = sg.GetRegion(wg.GetAirfield(afId).Region)
    match sg.State.RearAirfields.Contains(afId), region.HasInvaders, region.Owner with
    | true, _ , _ | _, true, _ ->
        // Don't attempt to update rear airfields or airfields where spawn is disabled due to an ongoing battle.
        async.Zero()
    | false, false, Some coalition ->
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
    | false, false, None ->
        async.Zero()

let private initAirfieldPlanesets(config : Configuration, wg : WorldFastAccess, sg : WorldStateFastAccess, handlers, hangars, entries) =
    async {
        let airfields0 =
            sg.State.Airfields
            |> List.map (fun afs -> afs.AirfieldId, afs.NumPlanes)
            |> Map.ofList
        let limits  = Limits.FromConfig config
        let! airfields =
            entries
            |> AsyncSeq.takeWhile (function Old _ -> true | Fresh _ -> false)
            |> checkPlaneAvailability config.MissionLengthH limits wg.World sg.State hangars
            |> AsyncSeq.fold (fun acc ->
                function
                | PlanesAtAirfield(af, planes) -> Map.add af planes acc
                | _ -> acc) airfields0
        for (af, planes) in Map.toSeq airfields do
            do! updateAirfieldPlaneset(config.MaxCapturedPlanes > 0, wg, sg, handlers, af, planes)
    }

let private mkBattleDamageTask (config : Configuration, wg : WorldFastAccess, sg : WorldStateFastAccess, handlers, asyncSeqEntries) : Async<unit> =
    let world = wg.World
    let state = sg.State
    let battleDamages =
        let battles =
            Battlefield.identifyBattleAreas world state
            |> Seq.cache
        asyncSeqEntries
        |> extractBattleDamages world state battles
        |> AsyncSeq.filter (function
            | Choice1Of2 _ -> true
            | Choice2Of2 damage -> damage.KilledByPlayer.IsSome)
    asyncSeq {
        let mutable battleDamage = Map.empty
        let mutable isMuted = true
        for battleKill in battleDamages do
            match battleKill with
            | Choice1Of2 () -> isMuted <- false
            | Choice2Of2 battleKill ->
                let oldBattleDamage =
                    battleDamage.TryFind (battleKill.BattleId, battleKill.Coalition)
                    |> Option.defaultValue 0.0f<E>
                let newDamage = oldBattleDamage + battleKill.Vehicle.Cost / (float32 config.BattleKillRatio)
                battleDamage <- battleDamage.Add ((battleKill.BattleId, battleKill.Coalition), newDamage)
                let totalValue =
                    GroundAttackVehicle.AllVehicles
                    |> Seq.sumBy (fun vehicle ->
                        (float32(sg.GetRegion(battleKill.BattleId).GetNumVehicles(battleKill.Coalition, vehicle))) * vehicle.Cost)
                if not isMuted && newDamage > totalValue * config.MaxBattleKillsRatioByPlayers then
                    yield battleKill.Coalition.Other, battleKill.BattleId
    }
    |> AsyncSeq.iterAsync (fun (coalition, region) -> handlers.OnMaxBattleDamageExceeded(string region, coalition))

let private mkHangarTask (config : Configuration, wg : WorldFastAccess, sg : WorldStateFastAccess, hangars, handlers, asyncSeqEntries) =
    let world = wg.World
    let state = sg.State

    asyncSeqEntries
    |> checkPlaneAvailability config.MissionLengthH (Limits.FromConfig config) world state hangars
    |> AsyncSeq.scan (fun (isMuted, _) msg ->
        logger.Debug(
            // Hide details of complex Status and Airfields messages
            let msg =
                match msg with
                | Status(_, _) -> Status(Map.empty, Map.empty)
                | PlanesAtAirfield(af, _) -> PlanesAtAirfield(af, Map.empty) 
                | x -> x
            sprintf "hangar task received message %A (isMuted = %s)" msg (string isMuted))
        let muteMask, action =
            match isMuted, msg with
            | _, Unmute ->
                false, async { return() }
            | true, PlayerEntered _
            | true, Overview _
            | true, Warning _
            | true, Announce _
            | true, Violation _
            | true, PlanesAtAirfield _ ->
                true, async { return()}
            | false, PlayerEntered(userId) ->
                true,
                async {
                    Async.Start(
                        async {
                            do! Async.Sleep(15000)
                            return! handlers.OnPlayerEntered(userId)
                        })
                    return()
                }
            | false, Overview(user, delay, messages) ->
                true,
                async {
                    Async.Start(
                        async {
                            do! Async.Sleep(delay * 1000)
                            return! handlers.OnMessagesToPlayer(user, messages)
                        })
                    return ()
                }
            | false, Warning(user, delay, messages) ->
                true,
                async {
                    Async.Start(
                        async {
                            do! Async.Sleep(delay * 1000)
                            return! handlers.OnMessagesToPlayer(user, messages)
                        })
                    return ()
                }
            | false, Announce(coalition, messages) ->
                true, handlers.OnMessagesToCoalition(coalition, messages)
            | false, Violation(user, reason) ->
                true,
                async {
                    do! handlers.OnMessagesToPlayer(user, [StringResources.kickNoBan reason])
                    Async.Start(
                        async {
                            do! Async.Sleep(10000)
                            do! handlers.OnPlayerPunished({ Player = user; Decision = Kicked })
                        })
                    return ()
                }
            | false, PlanesAtAirfield(afId, numPlanes) ->
                true, updateAirfieldPlaneset(config.MaxCapturedPlanes > 0, wg, sg, handlers, afId, numPlanes)
            | _, Status(hangars, _) ->
                true, handlers.OnHangarsUpdated(hangars)
        isMuted && muteMask, action
        ) (true, async { return() })
    |> AsyncSeq.iterAsync snd

let mkTimeTask(config : Configuration, handlers, entries : AsyncSeq<LogData<LogEntry>>) =
    let resolutions =
        [ TimeSpan(0, 15, 0); TimeSpan(0, 5, 0); TimeSpan(0, 1, 0); TimeSpan(0, 0, 30); TimeSpan(0, 0, 10); TimeSpan(0, 0, 1); ]
    let rec remainingTime (t : System.TimeSpan) =
        async {
            let msg =
                let times =
                    [
                        if t.Hours > 0 then
                            yield StringResources.hours t.Hours
                        if t.Minutes > 0 then
                            yield StringResources.minutes t.Minutes
                        if t.Hours = 0 && t.Minutes = 0 then
                            yield StringResources.seconds t.Seconds
                    ]
                    |> String.concat " "
                [ StringResources.timeLeft times ]
            logger.Info(msg)
            do! handlers.OnMessagesToCoalition(Axis, msg)
            do! handlers.OnMessagesToCoalition(Allies, msg)
            let resolution =
                resolutions
                |> List.tryFind (fun res -> t > res)
            match resolution with
            | Some resolution ->
                do! Async.Sleep (int resolution.TotalMilliseconds)
                let timeLeft = t - resolution
                return! remainingTime timeLeft
            | None ->
                return()
        }

    let remainingTime =
        async {
            let! timePassed =
                // Get timestamp of last entry
                entries
                |> AsyncSeq.takeWhile (function Old _ -> true | Fresh _ -> false)
                |> AsyncSeq.fold (fun latest entry ->
                    max entry.Data.Timestamp latest) (TimeSpan())
            let t =
                System.TimeSpan.FromMinutes(float config.MissionLength) - timePassed
            return! remainingTime t
        }

    remainingTime

let injectOldAndFresh entries oldArtificialEntries getInjectedData storeInjectedData =
        let mergedOldEntries =
            let oldRegularEntries =
                entries
                |> AsyncSeq.takeWhile (function Old _ -> true | Fresh _ -> false)
                |> AsyncSeq.map (fun x -> x.Data)
                |> AsyncSeq.toBlockingSeq
            let getTimestamp (ev : LogEntry) = ev.Timestamp
            Seq.mergeOrdered getTimestamp getTimestamp oldRegularEntries oldArtificialEntries
            |> Seq.map (function Choice1Of2 x | Choice2Of2 x -> Old x)
            |> AsyncSeq.ofSeq

        let freshEntries : AsyncSeq<LogEntry> =
            entries
            |> AsyncSeq.skipWhile (function Old _ -> true | _ -> false)
            |> AsyncSeq.map (fun x -> x.Data)

        let injectedData =
            AsyncSeq.initInfiniteAsync (fun _ -> getInjectedData())
            |> AsyncSeq.choose (Option.map (fun data -> ArtificialEntry(0L, data)))

        let mergedFreshEntries =
            AsyncSeq.mergeChoice freshEntries injectedData
            |> AsyncSeq.scan (fun ticks entry ->
                logger.Debug(sprintf "Seeing merge log entry %A" entry)
                match entry with
                | Choice1Of2 entry ->
                    let ticks =
                        if timeLessEntryTypes.Contains entry.EntryType then
                            ticks
                            |> Option.map fst
                            |> Option.defaultValue 0L
                        else
                            entry.Timestamp.Ticks / 200000L
                    Some(ticks, Fresh entry)
                | Choice2Of2 entry ->
                    let ticks =
                        ticks
                        |> Option.map fst
                        |> Option.defaultValue 0L
                    let entry = ArtificialEntry(ticks, entry.Data)
                    // Append the new artificial entry to the log, so that it's handled again if the commenter is interrupted and restarted while the mission runs.
                    try
                        storeInjectedData entry
                    with
                    | exc -> logger.Error(sprintf "Failed to append '%s' to extraLogs.txt: '%s'" entry.OriginalString exc.Message)
                    Some(ticks, Fresh (entry :> LogEntry))
            ) None
            |> AsyncSeq.choose (Option.map snd)

        AsyncSeq.append mergedOldEntries mergedFreshEntries

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (config : Configuration, handlers : EventHandlers, getInjectedData : Async<string option>) =
    let missionLogsDir = Path.Combine(config.ServerDataDir, "logs")
    let campaignDir = config.OutputDir
    let extraLogsName = Path.Combine(campaignDir, "extraLogs.txt")
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

    // The log entries before live artificial entry injection
    let entries =
        asyncSeq {
            let! baseName = tryGetMissionLogBaseName(state.Date, missionLogsDir)
            match baseName with
            | Some baseName ->
                let now = DateTime.UtcNow
                let logFiles =
                    watchLogs missionLogsDir baseName now
                    |> AsyncSeq.pairwise // Return 2nd must recent file
                    |> AsyncSeq.map fst  //
                for file in logFiles do
                    logger.Debug(sprintf "Parsing log file %A" file)
                    for line in File.ReadAllLines(Path.Combine(missionLogsDir, file.Data)) do
                        let entry =
                            try
                                match LogEntry.Parse line with
                                | null -> None
                                | x when not (x.IsValid()) -> None
                                | x -> Some x
                            with
                            | exc ->
                                logger.Error(sprintf "Failed to parse '%s': '%s'" line exc.Message)
                                None
                        match entry, file with
                        | Some entry, Old _ -> yield Old entry
                        | Some entry, Fresh _ -> yield Fresh entry
                        | None, _ -> ()
            | None ->
                logger.Error("Commenter not started")
        }
        |> AsyncSeq.cache

    let mergedEntries = 
        let oldArtificialEntries =
            if File.Exists(extraLogsName) then
                File.ReadAllLines(extraLogsName)
                |> Seq.map (LogEntry.Parse)
                |> Seq.filter (function
                    | null -> false
                    | x when not (x.IsValid()) -> false
                    | _ -> true)
                |> Array.ofSeq
            else
                [||]

        let store (entry : ArtificialEntry) =
            File.AppendAllLines(extraLogsName, [entry.OriginalString])

        injectOldAndFresh entries oldArtificialEntries (fun () -> getInjectedData) store

    let wg = world.FastAccess
    let sg = state.FastAccess

    do
        let battleLimitsTask = mkBattleDamageTask (config, wg, sg, handlers, entries)

        let disciplineTask =
            entries
            |> disciplinePlayers config world state
            |> AsyncSeq.iterAsync (fun penalty -> handlers.OnPlayerPunished penalty)

        let hangarTask = mkHangarTask (config, wg, sg, hangars, handlers, mergedEntries)

        let remainingTime = mkTimeTask(config, handlers, entries)

        async {
            do! initAirfieldPlanesets(config, wg, sg, handlers, hangars, entries)
            Async.Start(Async.catchLog "battle limits notifier" battleLimitsTask, cancelOnDispose.Token)
            Async.Start(Async.catchLog "abuse detector" disciplineTask, cancelOnDispose.Token)
            Async.Start(Async.catchLog "plane availability checker" hangarTask, cancelOnDispose.Token)
            Async.Start(Async.catchLog "remaining time notifier" remainingTime, cancelOnDispose.Token)
        }
        |> Async.Start

    member this.Dispose() =
        logger.Info("Commentator disposed")
        cancelOnDispose.Cancel()

    interface IDisposable with
        member this.Dispose() = this.Dispose()

