module Campaign.Commenting

open System
open System.IO
open System.Text.RegularExpressions
open Campaign.BasicTypes
open Campaign.Configuration
open Campaign.Util
open ploggy
open System.Numerics

/// <summary>
/// A player-controlled flight
/// </summary>
type InFlight =
    { Player : Guid
      Time : DateTime
      PlaneId : int
    }

type Pilot =
    { Player : Guid
      Coalition : CoalitionId option
    }

/// <summary>
/// State of ongoing round
/// </summary>
type RoundState =
    { Pilots : Map<int, Pilot>
      InFlight : InFlight list
      Damages : Map<int, float32>
    }

/// <summary>
/// Update the state of a round and call event handlers
/// </summary>
let update (onPlayerTookOff, onPlayerLanded, onMissionStarted) (entries : LogEntry list, round : RoundState) =
    entries
    |> List.fold (fun round entry ->
        match entry with
        | :? MissionStartEntry as start ->
            onMissionStarted start.MissionTime
            { Pilots = Map.empty
              InFlight = []
              Damages = Map.empty
            }
        | :? PlayerPlaneEntry as planeEntry ->
            let pilot =
                { Pilot.Player = planeEntry.NickId
                  Coalition =
                    match planeEntry.Country with
                    | Country.Germany | Country.OtherAxis -> Some Axis
                    | Country.USSR | Country.OtherAllies -> Some Allies
                    | _ -> None
                }
            let pilots =
                Map.add planeEntry.VehicleId pilot round.Pilots
            { round with Pilots = pilots }
        | :? TakeOffEntry as takeOff ->
            match round.Pilots.TryFind takeOff.VehicleId with
            | Some pilot ->
                let inFlight =
                    { Player = pilot.Player
                      Time = DateTime.Now
                      PlaneId = takeOff.VehicleId
                    }
                onPlayerTookOff(inFlight, pilot, 1 + List.length round.InFlight)
                { round with InFlight = inFlight :: round.InFlight }
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
        | :? LeaveEntry as leave ->
            // Find the plane flown by the player that left, if any
            let plane =
                round.InFlight
                |> List.tryFind (fun flight -> flight.Player = leave.NickId)
                |> Option.map (fun flight -> flight.PlaneId)
            // Remove all info linked to the player that left.
            { round with
                Pilots = round.Pilots |> Map.filter (fun _ pilot -> pilot.Player <> leave.NickId)
                InFlight = round.InFlight |> List.filter (fun flight -> flight.Player <> leave.NickId)
                Damages = round.Damages |> Map.filter (fun planeId _ -> Some planeId <> plane)
            }
        | _ ->
            round
    ) round

let initState (entries : LogEntry list) =
    let round =
        { Pilots = Map.empty
          InFlight = []
          Damages = Map.empty
        }
    update (ignore, ignore, ignore) (entries, round)

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (missionLogsDir : string, init : LogEntry list -> RoundState, update : LogEntry list * RoundState -> RoundState) =
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
                [entry] // Start new list
            | _ -> entry :: entries
        ) []
        |> List.rev
    let mutable state = init initialEntries
    let watcher = new FileSystemWatcher()
    do watcher.Path <- missionLogsDir
       watcher.Filter <- "missionReport*.txt"
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    let alreadyHandled = System.Collections.Generic.HashSet<string>()
    let onChanged = watcher.Changed.Subscribe(fun ev ->
        if not(alreadyHandled.Contains(ev.FullPath)) then
            let entries =
                try
                    [
                        for line in File.ReadAllLines(ev.FullPath) do
                            yield LogEntry.Parse(line)
                    ]
                with
                | e ->
                    eprintfn "Failed to parse '%s' because '%s'" ev.FullPath e.Message
                    []
            try
                state <- update(entries, state)
            with
            | e -> eprintfn "Failed to update state: '%s'" e.Message
            alreadyHandled.Add(ev.FullPath) |> ignore)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        onChanged.Dispose()
        watcher.Dispose()
