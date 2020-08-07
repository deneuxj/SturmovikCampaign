module Campaign.MissionResults

open FSharp.Control
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

type AmmoType with
    static member FromLogName(logName : string) : AmmoType =
        failwith "TODO"

/// The status of a plane piloted by a human pilot.
type FlightStatus =
    | Spawned of AirfieldId
    | InAir of TookOffFrom: AirfieldId
    | Landed of AirfieldId

/// Extract war state updade commands from the game logs.
let commandsFromLogs (state : IWarStateQuery) (logs : AsyncSeq<string>) =
    asyncSeq {
        let bindings = Seq.mutableDict []
        let pilotOf = Seq.mutableDict []
        let vehicleOf = Seq.mutableDict []
        let flights = Seq.mutableDict []
        let flightRecords = Seq.mutableDict []
        let healthOf = Seq.mutableDict []
        let latestHit = Seq.mutableDict []

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
                        AmmoType.FromLogName("")
                    | false, _ ->
                        AmmoType.Bullets
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
            ]

        /// Update flight record with damage
        let updateFlightRecord(attackerId, damage, targetId, position) =
            match pilotOf.TryGetValue(attackerId) with
            | true, (taken : ObjectTaken) ->
                match flightRecords.TryGetValue(taken.UserId) with
                | true, (x : {| Pilot : Pilot; Record : FlightRecord |}) ->
                    let flight =
                        { x.Record with
                            TargetsDamaged = x.Record.TargetsDamaged @ recordedDamages(damage, targetId, position)
                        }
                    flightRecords.[taken.UserId] <- {| x with Record = flight |}
                | false, _ ->
                    ()
            | false, _ ->
                ()

        for line in logs do
            match line with
            | ObjectEvent(_, ObjectBound binding) ->
                // Update mappings
                bindings.[binding.Id] <- binding
                healthOf.[binding.Id] <- 1.0f

            | PlayerEvent(_, PlayerTakesObject taken) ->
                // Update mappings
                pilotOf.[taken.VehicleId] <- taken
                vehicleOf.[taken.PilotId] <- taken
                flights.[taken.VehicleId] <- Spawned(state.GetNearestAirfield(taken.Position).AirfieldId)

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
                // Update mappings
                let afId =
                    match flights.TryGetValue(takeOff.Id) with
                    | true, Spawned afId | true, Landed afId -> afId
                    | _ -> state.GetNearestAirfield(takeOff.Position).AirfieldId
                flights.[takeOff.Id] <- InAir afId
                // Start flight record
                match pilotOf.TryGetValue(takeOff.Id) with
                | true, taken ->
                    let country =
                        CountryId.FromMcuValue(enum taken.Country)
                    let plane = state.TryGetPlane(taken.Typ)
                    match country, plane with
                    | Some country, Some plane ->
                        let pilot = state.GetPlayerPilotFrom(taken.UserId, afId, country)
                        let record : FlightRecord =
                            { Date = state.Date + timeStamp
                              Length = System.TimeSpan(0L)
                              Plane = plane.Id
                              Start = afId
                              TargetsDamaged = []
                              Return = CrashedInEnemyTerritory // Meaningless, will get updated when player lands or crashes. Might be useful as a default for in-air disconnects.
                            }
                        flightRecords.[taken.UserId] <- {| Pilot = pilot; Record = record |}
                    | _ ->
                        ()
                | false, _ -> ()

            | ObjectEvent(_, ObjectDamaged damaged) ->
                // Update mappings
                let health =
                    healthOf.TryGetValue(damaged.TargetId)
                    |> Option.ofPair
                    |> Option.defaultValue 1.0f
                let health = health - damaged.Damage
                healthOf.[damaged.TargetId] <- health
                // Update flight record
                updateFlightRecord(damaged.AttackerId, damaged.Damage, damaged.TargetId, damaged.Position)

                // Emit commands
                yield! handleDamage(damaged.Damage, damaged.TargetId, damaged.Position)

            | ObjectEvent(_, ObjectKilled killed) ->
                // Update mappings
                let oldHealth =
                    healthOf.TryGetValue(killed.TargetId)
                    |> Option.ofPair
                    |> Option.defaultValue 1.0f
                healthOf.[killed.TargetId] <- 0.0f
                // Update flight record
                updateFlightRecord(killed.AttackerId, oldHealth, killed.TargetId, killed.Position)

                // Emit commands
                yield! handleDamage(oldHealth, killed.TargetId, killed.Position)

            | PlayerEvent(_, PlayerEndsMission missionEnded) ->
                // Emit AddPlane command
                match bindings.TryGetValue(missionEnded.VehicleId), flights.TryGetValue(missionEnded.VehicleId), healthOf.TryGetValue(missionEnded.VehicleId) with
                | (true, binding), (true, Landed afId), (true, health) when health > 0.0f ->
                    match state.TryGetPlane(binding.Name) with
                    | Some plane ->
                        yield AddPlane(afId, plane.Id, health)
                    | None ->
                        ()
                | _ ->
                    ()
                // Update mappings
                pilotOf.Remove(missionEnded.VehicleId) |> ignore
                vehicleOf.Remove(missionEnded.PilotId) |> ignore
                flights.Remove(missionEnded.VehicleId) |> ignore
                healthOf.Remove(missionEnded.VehicleId) |> ignore

            | _ ->
                ()
    }