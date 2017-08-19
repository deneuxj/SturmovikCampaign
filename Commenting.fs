﻿module Campaign.Commenting

open System
open System.IO
open System.Text.RegularExpressions
open Campaign.BasicTypes
open Campaign.Configuration
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Util
open ploggy
open System.Numerics

/// <summary>
/// A player-controlled flight
/// </summary>
type InFlight =
    { Player : Guid
      Time : DateTime
      Coalition : CoalitionId option
      PlaneId : int
    }

/// <summary>
/// State of ongoing round
/// </summary>
type RoundState =
    { Pilots : Map<int, Guid>
      InFlight : InFlight list
      Damages : Map<int, float32>
    }

/// <summary>
/// Update the state of a round and call event handlers
/// </summary>
let update (world : World, state : WorldState) (onPlayerTookOff, onPlayerLanded) (entries : LogEntry list, round : RoundState) =
    entries
    |> List.fold (fun round entry ->
        match entry with
        | :? PlayerPlaneEntry as planeEntry ->
            let pilots =
                Map.add planeEntry.VehicleId planeEntry.NickId round.Pilots
            { round with Pilots = pilots }
        | :? TakeOffEntry as takeOff ->
            match round.Pilots.TryFind takeOff.VehicleId with
            | Some player ->
                let pos = Vector2(takeOff.Position.X, takeOff.Position.Z)
                let af = world.GetClosestAirfield(pos)
                let region =
                    try
                        state.Regions
                        |> List.find (fun region -> region.RegionId = af.Region)
                        |> Some
                    with
                    | _ -> None
                match region with
                | Some region ->
                    let inFlight =
                        { Player = player
                          Time = DateTime.Now
                          Coalition = region.Owner
                          PlaneId = takeOff.VehicleId
                        }
                    onPlayerTookOff(inFlight, 1 + List.length round.InFlight)
                    { round with InFlight = inFlight :: round.InFlight }
                | None ->
                    round
            | None ->
                round
        | :? LandingEntry as landing ->
            match round.Pilots.TryFind landing.VehicleId with
            | Some player ->
                let damages =
                    round.Damages.TryFind landing.VehicleId
                    |> Option.defaultVal 0.0f
                let flightDuration =
                    round.InFlight
                    |> List.tryFind (fun flight -> flight.PlaneId = landing.VehicleId)
                    |> Option.map (fun flight -> DateTime.Now - flight.Time)
                onPlayerLanded(player, damages, flightDuration)
                { round with
                    Pilots = round.Pilots |> Map.filter (fun planeId _ -> planeId <> landing.VehicleId)
                    InFlight = round.InFlight |> List.filter (fun flight -> flight.PlaneId <> landing.VehicleId)
                    Damages = round.Damages |> Map.filter (fun planeId _ -> planeId <> landing.VehicleId)
                }
            | None ->
                round
        | :? DamageEntry as damage ->
            let oldDamage =
                round.Damages.TryFind damage.TargetId
                |> Option.defaultVal 0.0f
            let newDamage =
                oldDamage + damage.Damage
            { round with Damages = Map.add damage.TargetId newDamage round.Damages }
        | :? KillEntry as killed ->
            { round with
                Pilots = round.Pilots |> Map.filter (fun planeId _ -> planeId <> killed.TargetId)
                InFlight = round.InFlight |> List.filter (fun flight -> flight.PlaneId <> killed.TargetId)
                Damages = round.Damages |> Map.filter (fun planeId _ -> planeId <> killed.TargetId)
            }
        | _ ->
            round
    ) round

let initState (world, state) (entries : LogEntry list) =
    let round =
        { Pilots = Map.empty
          InFlight = []
          Damages = Map.empty
        }
    update (world, state) (ignore, ignore) (entries, round)

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (world : World, state : WorldState, config : Configuration, init : LogEntry list -> RoundState, update : LogEntry list * RoundState -> RoundState) =
    let missionLogsDir = Path.Combine(config.ServerDataDir, "logs")
    // retrieve entries from most recent mission, if it matches the state's mission and start date.
    let initialEntries =
        seq {
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
            for file in ordered do
                for line in File.ReadAllLines(file) do
                    yield LogEntry.Parse(line)
        }
        |> Seq.fold (fun entries entry ->
            match entry with
            | :? MissionStartEntry as start ->
                let expectedMissionFile = sprintf "Multiplayer/%s.msnbin" config.MissionName
                if start.MissionTime = state.Date && start.MissionFile = expectedMissionFile then
                    Some [entry] // Start new list
                else
                    None // Start new skip
            | _ ->
                match entries with
                | None -> None
                | Some x -> Some(entry :: x)
        ) None
        |> Option.defaultVal []
        |> List.rev
    let mutable state = init initialEntries
    let watcher = new FileSystemWatcher()
    do watcher.Path <- missionLogsDir
       watcher.Filter <- "missionReport*.txt"
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    let onChanged = watcher.Changed.Subscribe(fun ev ->
        let entries =
            try
                [
                    for line in File.ReadAllLines(ev.FullPath) do
                        yield LogEntry.Parse(line)
                ]
            with
            | e ->
                eprintfn "Failed to parse '%s'" ev.FullPath
                []
        try
            state <- update(entries, state)
        with
        | e -> eprintfn "Failed to update state: '%s'" e.Message)

    member this.Dispose() =
        onChanged.Dispose()
        watcher.Dispose()
