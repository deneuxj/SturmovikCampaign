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
open MBrace.FsPickler

type EventHandlers =
    // player name, coalition, airfield, plane, cargo
    { OnCargoTookOff : string * CoalitionId * AirfieldId * PlaneModel * float32<E> -> Async<unit>
      // playername, coalition, airfield, plane, cargo, health, damages inflicted
      OnLanded : string * CoalitionId * AirfieldId * PlaneModel * float32<E> * float32 * float32<E> -> Async<unit>
    }

// Scan state of a sequence of log entries
type ScanState =
    | Skipping // Current mission is not one we are interested in
    | Handling of AsyncSeq<LogEntry> // Current mission is of interest: keep building result extraction state

// Split flow of log entries into groups corresponding to missions of interest.
let split (date : DateTime) (entries : AsyncSeq<LogEntry>) =
    entries
    |> AsyncSeq.scan (fun ss entry ->
        match ss, entry with
        | _, (:? MissionStartEntry as start) ->
            if start.MissionTime = date then
                Handling(AsyncSeq.singleton entry)
            else
                Skipping
        | Skipping, _ ->
            Skipping
        | Handling entries, _ ->
            Handling(AsyncSeq.append entries (AsyncSeq.singleton entry))
    ) Skipping
    |> AsyncSeq.choose (fun ss ->
        match ss with
        | Skipping -> None
        | Handling entries -> Some entries)

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
    let initialEntries =
        lines
        |> Seq.map LogEntry.Parse
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
            |> split state.Date
            |> AsyncSeq.map (extractTakeOffsAndLandings world state)
            |> AsyncSeq.iterAsync (fun events ->
                events
                |> AsyncSeq.iterAsync(fun event ->
                    async {
                        match event with
                        | TookOff ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                            if true || x.Cargo > 0.0f<E> then
                                return! handlers.OnCargoTookOff(player, coalition, x.Airfield, x.Plane, x.Cargo)
                        | Landed ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                            if true || x.Cargo > 0.0f<E> || x.Health < 1.0f then
                                return! handlers.OnLanded(player, coalition, x.Airfield, x.Plane, x.Cargo, x.Health, 0.0f<E>)
                        | _ ->
                            return()
                    }))
        Async.Start(task, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()

/// Monitor state.xml, (re-) starting a commentator whenever the file is modified
type CommentatorRestarter(missionLogsDir : string, campaignDir : string, handlers : EventHandlers, onStateWritten : unit -> unit) =
    let watcher = new FileSystemWatcher()
    do watcher.Path <- campaignDir
       watcher.Filter <- "state.xml"
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    let serializer = FsPickler.CreateXmlSerializer()
    let worldFile = File.OpenText(Path.Combine(campaignDir, "world.xml"))
    let world = serializer.Deserialize<World>(worldFile)
    do worldFile.Dispose()
    let rec work commentator =
        async {
            let! ev = Async.AwaitEvent watcher.Changed
            onStateWritten()
            match commentator with
            | Some (commentator : Commentator) -> commentator.Dispose()
            | None -> ()
            let state =
                try
                    use stateFile = File.OpenText(Path.Combine(campaignDir, "state.xml"))
                    serializer.Deserialize<WorldState>(stateFile) |> Some
                with
                | exc ->
                    eprintfn "Failed to parse state.xml: %s" exc.Message
                    None
            match state with
            | Some state ->
                let commentator = new Commentator(missionLogsDir, handlers, world, state)
                return! work(Some commentator)
            | None ->
                return! work None
        }
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    do Async.Start(work None, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()