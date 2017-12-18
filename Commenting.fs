module Campaign.Commenting

open System
open System.IO
open System.Text.RegularExpressions
open Campaign.BasicTypes
open Campaign.Configuration
open Campaign.Util
open ploggy
open System.Numerics
open FSharp.Control
open Campaign.ResultExtraction
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.BasicTypes
open Campaign.PlaneModel

type EventHandlers =
    // player name, coalition, airfield, plane, cargo
    { OnCargoTookOff : string * CoalitionId * AirfieldId * PlaneModel * float32<E> -> Async<unit>
      // playername, coalition, airfield, plane, cargo, health, damages inflicted
      OnLanded : string * CoalitionId * AirfieldId * PlaneModel * float32<E> * float32 * float32<E> -> Async<unit>
      // Called when a mission starts: current commentator should be killed, new one started with updated world and state
      OnNewMission : unit -> unit
    }

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (missionLogsDir : string, handlers : EventHandlers, world : World, state : WorldState) =
    // retrieve entries from most recent mission, if it matches the state's mission and start date.
    let files =
        let unordered = Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt")
        let r = Regex(@"(missionReport\(.*\))\[([0-9]+)\]\.txt")
        let ordered =
            unordered
            |> Seq.choose(fun path ->
                let m = r.Match(Path.GetFileName(path))
                if not m.Success then
                    None
                else
                    Some(path, (m.Groups.[1].ToString(), System.Int32.Parse(m.Groups.[2].ToString()))))
            |> Seq.sortBy snd
            |> Seq.map fst
        ordered
        |> List.ofSeq
    let lines =
        seq {
            for file in files do
                yield! File.ReadAllLines(file)
        }
        |> List.ofSeq
    let rec skipEntries lines =
        seq {
            match lines with
            | line :: rest ->
                match LogEntry.Parse(line) with
                | :? MissionStartEntry as start ->
                    if start.MissionTime = state.Date then
                        yield! collectEntries rest
                    else
                        yield! skipEntries rest
                | _ ->
                    yield! skipEntries rest
            | [] ->
                ()
        }
    and collectEntries lines =
        seq {
            match lines with
            | line :: rest ->
                yield LogEntry.Parse(line)
                yield! collectEntries rest
            | [] ->
                ()
        }
    let initialEntries =
        skipEntries lines
        |> List.ofSeq
    let watcher = new FileSystemWatcher()
    do watcher.Path <- missionLogsDir
       watcher.Filter <- "missionReport*.txt"
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    // An AsyncSeq built from new file notifications: generate log entries
    let rec getEntry (batched, alreadyHandled : Set<string>) = 
        async {
            match batched with
            | [] ->
                let! ev = Async.AwaitEvent watcher.Changed
                if not(alreadyHandled.Contains(ev.FullPath)) then
                    let entries2 =
                        try
                            [
                                for line in File.ReadAllLines(ev.FullPath) do
                                    yield LogEntry.Parse(line)
                            ]
                        with
                        | e ->
                            eprintfn "Failed to parse '%s' because '%s'" ev.FullPath e.Message
                            []
                    let missionEnded =
                        entries2
                        |> List.exists (fun entry ->
                            match entry with
                            | :? RoundEndEntry
                            | :? MissionEndEntry
                            | :? MissionStartEntry -> true
                            | _ -> false
                        )
                    if missionEnded then
                        return None
                    else
                        return! getEntry (entries2, Set.add  ev.FullPath alreadyHandled)
                else
                    return! getEntry (batched, alreadyHandled)
            | x :: xs ->
                return Some(x, (xs, alreadyHandled))
        }
    let asyncSeqEntries =
        AsyncSeq.unfoldAsync getEntry (initialEntries, Set.ofSeq files)
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    // Notify of interesting take-offs and landings
    do
        let task =
            asyncSeqEntries
            |> extractTakeOffsAndLandings world state
            |> AsyncSeq.iterAsync (fun entry ->
                async {
                    match entry with
                    | TookOff ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        if x.Cargo > 0.0f<E> then
                            return! handlers.OnCargoTookOff(player, coalition, x.Airfield, x.Plane, x.Cargo)
                    | Landed ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        if x.Cargo > 0.0f<E> || x.Health < 1.0f then
                            return! handlers.OnLanded(player, coalition, x.Airfield, x.Plane, x.Cargo, x.Health, 0.0f<E>)
                    | _ ->
                        return()
                })
        Async.Start(task, cancelOnDispose.Token)
    // Notify of mission ends. Used to kill this commentator and build a new one with updated state
    do
        let task =
            async {
                do! asyncSeqEntries |> AsyncSeq.iter ignore
                handlers.OnNewMission()
                return()
            }
        Async.Start(task, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()
