module Campaign.MissionResults

open FSharp.Control
open FSharp.Json
open System.Numerics

open Util
open VectorExtension

open Campaign.GameLogEvents
open Campaign.BasicTypes
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.WorldDescription
open Campaign.Targets.ActivePatterns
open Campaign.Targets
open Campaign.Pilots
open System.Collections.Generic

/// Transform a name from a log: lower case, delete all non-alphanum chars.
let normalizeLogName (name : string) =
    name.ToLowerInvariant()
    |> Array.ofSeq
    |> Array.filter System.Char.IsLetterOrDigit
    |> System.String

/// Transform a script or model name: retain filename only, lower case, delete all non-alphanum chars.
let normalizeScript (path : string) =
    System.IO.Path.GetFileNameWithoutExtension(path).ToLowerInvariant()
    |> Array.ofSeq
    |> Array.filter System.Char.IsLetterOrDigit
    |> System.String

type IWarStateQuery with
    /// Get the nearest airfield to a position
    member this.GetNearestAirfield((x, _, z) : Position) =
        let p = Vector2(x, z)
        this.World.Airfields.Values
        |> Seq.minBy (fun af -> (af.Position - p).Length())

    /// Try to get a plane by its name as it appears in the logs
    member this.TryGetPlane(logName : string) =
        let logName = logName.ToLowerInvariant()
        this.World.PlaneSet.Values
        |> Seq.tryFind (fun plane -> logName.Contains(plane.LogName.ToLowerInvariant()))

    /// Get all buildings that match a name, have the given subpart as reported as important, and cover the given position.
    member this.GetBuildingsAt(logName : string, part : int, (x, _, z) : Position) =
        let logName = normalizeLogName logName
        let pos = Vector2(x, z)
        Seq.append this.World.Buildings.Values this.World.Bridges.Values
        |> Seq.filter (fun building ->
            normalizeScript(building.Properties.Script) = logName &&
            List.contains part building.Properties.SubParts &&
            pos.IsInConvexPolygon building.Boundary)

    /// Try to get the static plane at the given position, and the airfield it is assigned to.
    member this.TryGetStaticPlaneAt(logName : string, ((x, _, z) as pos)) =
        // We don't check that there actually is a plane at that position, only that's it's "close enough" to the nearest airfield.
        let af = this.GetNearestAirfield(pos)
        if (af.Position - Vector2(x, z)).Length() < 3000.0f then
            let logName = normalizeLogName logName
            this.World.PlaneSet.Values
            |> Seq.tryFind (fun plane -> normalizeScript(plane.StaticScriptModel.Script) = logName)
            |> Option.map (fun plane -> af.AirfieldId, plane)
        else
            None

    /// Try to get the region at some position
    member this.TryGetRegionAt((x, _, z)) =
        let pos = Vector2(x, z)
        this.World.Regions.Values
        |> Seq.tryFind (fun region -> pos.IsInConvexPolygon region.Boundary)

    /// Get an existing pilot of a player given the starting airfield, or create a new pilot
    member this.GetPlayerPilotFrom(playerGuid : string, afId : AirfieldId, country) =
        let latestPilot =
            try
                this.GetPlayerPilots(playerGuid)
                |> Seq.filter (fun pilot -> this.IsPilotHealty(pilot.Id))
                |> Seq.filter (fun pilot -> this.IsPilotAvailableFrom(pilot.Id, afId))
                |> Seq.maxBy (fun pilot -> pilot.Id)
                |> Some
            with _ -> None
        let pilot =
            latestPilot
            |> Option.defaultWith (fun () -> this.NewPilot(playerGuid, country))
        pilot

    /// Try to get the coalition of a country from its event log value
    member this.TryGetCoalitionOfCountry(country : int) =
        let country = CountryId.FromMcuValue(enum country)
        country
        |> Option.bind (fun country ->
            this.World.Countries.TryGetValue(country)
            |> Option.ofPair)

type AmmoType with
    static member FromLogName(logName : string) : AmmoType = AmmoName logName

/// Extract war state updade commands from the game logs.
/// Note: The state must be updated as soon as a command is yielded.
let commandsFromLogs (state : IWarStateQuery) (logs : AsyncSeq<string>) =
    let logger = NLog.LogManager.GetCurrentClassLogger()
    asyncSeq {
        // Object ID to Binding
        let bindings = Seq.mutableDict []
        // Vehicle ID to ObjectTaken
        let pilotOf = Seq.mutableDict []
        // Pilot ID to ObjectTaken
        let vehicleOf = Seq.mutableDict []
        // Log Pilot ID to Pilot and FlightRecord
        let flightRecords : Dictionary<int, {| Pilot : Pilot; Record : FlightRecord |}> = Seq.mutableDict []
        // Object ID to float32
        let healthOf = Seq.mutableDict []
        // Vehicle ID to ObjectHit
        let latestHit : Dictionary<int, {| Ammo : string; AttackerId : int; TargetId : int |}> = Seq.mutableDict []

        let handleDamage(amount, targetId, position) =
            asyncSeq {
                match bindings.TryGetValue(targetId) with
                | true, binding ->
                    // Emit DamageBuildingPart
                    let sub = binding.Sub |> Option.defaultValue -1
                    for building in state.GetBuildingsAt(binding.Name, sub, position) do
                        yield DamageBuildingPart(building.Id, sub, amount)

                    // Emit RemovePlane for damages to static planes
                    match state.TryGetStaticPlaneAt(binding.Name, position) with
                    | Some(afId, plane) ->
                        yield RemovePlane(afId, plane.Id, amount)
                    | None ->
                        ()

                    // Emit DestroyGroundForces for damages to ground forces
                    match binding.Name with
                    | TargetTypeByName target when target.GroundForceValue > 0.0f<MGF> ->
                        let country : SturmovikMission.DataProvider.Mcu.CountryValue = enum binding.Country
                        match CountryId.FromMcuValue country, state.TryGetRegionAt(position) with
                        | Some country, Some region ->
                            match state.World.Countries.TryGetValue(country) with
                            | true, coalition ->
                                yield DestroyGroundForces(region.RegionId, coalition, amount * target.GroundForceValue)
                            | false, _ ->
                                ()
                        | _ ->
                            ()
                    | _ ->
                        ()
                | _ ->
                    ()
            }

        /// Generate all the damage entries corresponding to a target.
        // Typically should be one or zero, but it's OK if they are more. Players won't complain if they are assigned victories incorrectly.
        let recordedDamages(amount : float32, targetId, position) =
            [
                let ammo =
                    match latestHit.TryGetValue(targetId) with
                    | true, hit ->
                        Some(AmmoType.FromLogName(hit.Ammo))
                    | false, _ ->
                        None
                match ammo with
                | Some ammo ->
                    match bindings.TryGetValue(targetId) with
                    | true, binding ->
                        // Buildings
                        let sub = binding.Sub |> Option.defaultValue -1
                        for building in state.GetBuildingsAt(binding.Name, sub, position) do
                            yield (TargetType.Building(building.Id, sub), ammo, amount)

                        // Parked planes
                        match state.TryGetStaticPlaneAt(binding.Name, position) with
                        | Some(afId, plane) ->
                            yield (TargetType.ParkedPlane(afId, plane.Id), ammo, amount)
                        | None ->
                            ()

                        // Flying planes
                        match state.TryGetPlane(binding.Name) with
                        | Some plane ->
                            yield (TargetType.Air(plane.Id), ammo, amount)
                        | None ->
                            ()

                        // Others
                        match binding.Name with
                        | TargetTypeByName target ->
                            yield (target, ammo, amount)
                        | _ ->
                            ()
                    | false, _ ->
                        ()
                | None -> ()
            ]

        /// Update flight record with damage
        let updateFlightRecord(attackerId, damage, targetId, position) =
            let differentCoalitions =
                match bindings.TryGetValue(attackerId), bindings.TryGetValue(targetId) with
                | (true, attacker), (true, target) ->
                    // Both IDs have bindings, and their respective coalitions could be identified, and they are different
                    (state.TryGetCoalitionOfCountry(attacker.Country), state.TryGetCoalitionOfCountry(target.Country))
                    ||> Option.map2 (fun c1 c2 -> c1 <> c2)
                    |> Option.defaultValue false // Default: coalitions aren't known to be different
                | _ ->
                    false
            if not differentCoalitions then
                ()
            else

            match pilotOf.TryGetValue(attackerId) with
            | true, (taken : ObjectTaken) ->
                match flightRecords.TryGetValue(taken.PilotId) with
                | true, x ->
                    let flight =
                        { x.Record with
                            TargetsDamaged = x.Record.TargetsDamaged @ recordedDamages(damage, targetId, position)
                        }
                    flightRecords.[taken.PilotId] <- {| x with Record = flight |}
                | false, _ ->
                    ()
            | false, _ ->
                ()

        /// Update flight record with air kills
        let updateAirKills(pilotId, targetId) =
            match bindings.TryGetValue(targetId), flightRecords.TryGetValue(pilotId) with
            | (true, binding), (true, x) ->
                let differentCoalitions =
                    match state.TryGetCoalitionOfCountry(binding.Country), state.World.Countries.[x.Pilot.Country] with
                    | Some country1, country2 -> country1 <> country2
                    | _ -> false
                if differentCoalitions && state.TryGetPlane(binding.Typ).IsSome then
                    flightRecords.[pilotId] <-
                        {| x with
                            Record = { x.Record with AirKills = x.Record.AirKills + 1 }
                        |}
            | _ ->
                ()

        let handleLine line =
            asyncSeq {
                match line with
                | ObjectEvent(_, ObjectBound binding) ->
                    logger.Debug("Updating mappings with binding " + Json.serialize binding)
                    // Update mappings
                    bindings.[binding.Id] <- binding
                    healthOf.[binding.Id] <- 1.0f

                | PlayerEvent(_, PlayerTakesObject taken) ->
                    logger.Debug("Updating mappings with taken " + Json.serialize taken)
                    // Update mappings
                    pilotOf.[taken.VehicleId] <- taken
                    vehicleOf.[taken.PilotId] <- taken

                    // Emit RemovePlane command
                    match bindings.TryGetValue(taken.VehicleId) with
                    | true, binding ->
                        match state.TryGetPlane(binding.Name) with
                        | Some plane ->
                            yield RemovePlane(state.GetNearestAirfield(taken.Position).AirfieldId, plane.Id, 1.0f)
                        | None ->
                            ()
                    | false, _ ->
                        ()

                    // Emit UpdatePlayer command
                    yield UpdatePlayer(taken.UserId, taken.Name)

                | ObjectEvent(timeStamp, ObjectTakesOff takeOff) ->
                    logger.Debug("Updating mappings with takenOff " + Json.serialize takeOff)
                    // Start flight record
                    let afId = state.GetNearestAirfield(takeOff.Position).AirfieldId
                    let planeHealth =
                        healthOf.TryGetValue(takeOff.Id)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    match pilotOf.TryGetValue(takeOff.Id) with
                    | true, taken ->
                        let pilotHealth =
                            healthOf.TryGetValue(taken.PilotId)
                            |> Option.ofPair
                            |> Option.defaultValue 1.0f
                        let country =
                            CountryId.FromMcuValue(enum taken.Country)
                        let plane = state.TryGetPlane(taken.Typ)
                        match country, plane with
                        | Some country, Some plane ->
                            let pilot = state.GetPlayerPilotFrom(taken.UserId, afId, country)
                            let record : FlightRecord =
                                {
                                    Date = state.Date + timeStamp
                                    Length = System.TimeSpan(0L)
                                    Plane = plane.Id
                                    PlaneHealth = planeHealth
                                    PilotHealth = pilotHealth
                                    Start = afId
                                    AirKills = 0
                                    TargetsDamaged = []
                                    Return = CrashedInEnemyTerritory // Meaningless, will get updated when player lands or crashes. Might be useful as a default for in-air disconnects.
                                }
                            flightRecords.[taken.PilotId] <- {| Pilot = pilot; Record = record |}
                            yield UpdatePilot(pilot)
                        | _ ->
                            ()
                    | false, _ -> ()

                | ObjectEvent(timeStamp, ObjectLands landing) ->
                    logger.Debug("Updating mappings with landing " + Json.serialize landing)
                    // Update flight record
                    let afId = state.GetNearestAirfield(landing.Position).AirfieldId
                    let planeHealth =
                        healthOf.TryGetValue(landing.Id)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    match pilotOf.TryGetValue(landing.Id) with
                    | true, taken ->
                        match flightRecords.TryGetValue(taken.PilotId) with
                        | true, x ->
                            let flight =
                                { x.Record with
                                    Length = state.Date + timeStamp - x.Record.Date
                                    PlaneHealth = planeHealth
                                    Return = AtAirfield afId
                                }
                            flightRecords.[taken.PilotId] <- {| x with Record = flight |}
                        | false, _ ->
                            ()
                    | false, _ ->
                        ()

                | ObjectEvent(_, ObjectHit hit) ->
                    logger.Debug("Updating mappings with hit " + Json.serialize hit)
                    latestHit.[hit.TargetId] <- hit

                | ObjectEvent(_, ObjectDamaged damaged) ->
                    logger.Debug("Updating mappings with damaged " + Json.serialize damaged)
                    // Update mappings
                    let health =
                        healthOf.TryGetValue(damaged.TargetId)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    let health = health - damaged.Damage
                    healthOf.[damaged.TargetId] <- health
                    // Update flight record
                    match pilotOf.TryGetValue(damaged.AttackerId) with
                    | true, taken ->
                        updateFlightRecord(taken.PilotId, damaged.Damage, damaged.TargetId, damaged.Position)
                    | false, _ ->
                        ()
                    // Emit commands
                    yield! handleDamage(damaged.Damage, damaged.TargetId, damaged.Position)

                | ObjectEvent(_, ObjectKilled killed) ->
                    logger.Debug("Updating mappings with killed " + Json.serialize killed)
                    // Update mappings
                    let oldHealth =
                        healthOf.TryGetValue(killed.TargetId)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    healthOf.[killed.TargetId] <- 0.0f
                    // Update flight record
                    match pilotOf.TryGetValue(killed.AttackerId) with
                    | true, taken ->
                        updateFlightRecord(taken.PilotId, oldHealth, killed.TargetId, killed.Position)
                        updateAirKills(taken.PilotId, killed.TargetId)
                    | false, _ ->
                        ()
                    // Emit commands
                    yield! handleDamage(oldHealth, killed.TargetId, killed.Position)

                | PlayerEvent(timeStamp, PlayerEndsMission missionEnded) ->
                    logger.Debug("Updating mappings with missionEnded " + Json.serialize missionEnded)
                    let afId =
                        flightRecords.TryGetValue(missionEnded.PilotId)
                        |> Option.ofPair
                        |> Option.bind (fun x -> state.TryGetReturnAirfield(x.Record, state.World.Countries.[x.Pilot.Country]))
                    // Emit AddPlane command
                    match bindings.TryGetValue(missionEnded.VehicleId), afId, healthOf.TryGetValue(missionEnded.VehicleId) with
                    | (true, binding), Some afId, (true, health) when health > 0.0f ->
                        match state.TryGetPlane(binding.Name) with
                        | Some plane ->
                            yield AddPlane(afId, plane.Id, health)
                        | None ->
                            ()
                    | _ ->
                        ()
                    // Emit RegisterPilotFlight
                    let pilotHealth =
                        healthOf.TryGetValue(missionEnded.PilotId)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    let planeHealth =
                        healthOf.TryGetValue(missionEnded.VehicleId)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    match flightRecords.TryGetValue(missionEnded.PilotId) with
                    | true, x ->
                        let injury =
                            1.0f - pilotHealth
                        let healthStatus =
                            if injury < 0.01f then
                                Healthy
                            else if injury >= 1.0f then
                                Dead
                            else
                                state.Date + timeStamp + System.TimeSpan(int(ceil(injury * 30.0f)), 0, 0, 0)
                                |> Injured
                        let flight =
                            { x.Record with
                                PilotHealth = pilotHealth
                                PlaneHealth = planeHealth
                            }
                        yield RegisterPilotFlight(x.Pilot.Id, flight, healthStatus)
                    | false, _ ->
                        ()
                    // Update mappings
                    pilotOf.Remove(missionEnded.VehicleId) |> ignore
                    vehicleOf.Remove(missionEnded.PilotId) |> ignore
                    healthOf.Remove(missionEnded.VehicleId) |> ignore
                    flightRecords.Remove(missionEnded.PilotId) |> ignore
                | _ ->
                    ()
            }

        for line in logs do
            try
                yield! handleLine line
            with exc ->
                logger.Error("Exception while processing log")
                logger.Error(exc)
    }