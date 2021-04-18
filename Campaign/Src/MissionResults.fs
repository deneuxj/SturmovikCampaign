﻿// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
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

/// Extract results from game logs and generate war state update commands.
module Campaign.MissionResults

open FSharp.Control
open FSharp.Json
open System.Numerics

open Util
open Util.RegexActivePatterns
open VectorExtension

open Campaign.Common.BasicTypes
open Campaign.Common.Targets.ActivePatterns
open Campaign.Common.Targets

open Campaign.GameLogEvents
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.WarStateUpdate.CommandExecution
open Campaign.Pilots
open System.Collections.Generic
open Campaign.MissionGen.MissionFileGeneration

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
    member this.TryGetNearestAirfield((x, _, z) : Position, coalitionFilter : CoalitionId option) =
        let p = Vector2(x, z)
        try
            this.World.Airfields.Values
            |> Seq.filter (fun af ->
                match coalitionFilter, this.GetOwner(af.Region) with
                | None, _ -> true
                | Some c, Some owner -> c = owner
                | Some _, None -> false)
            |> Seq.map (fun af -> af, (af.Position - p).Length())
            |> Seq.minBy snd
            |> Some
        with _ -> None

    /// Try to get a plane by its name as it appears in the logs
    member this.TryGetPlane(logName : string) =
        let logName = normalizeLogName logName
        this.World.PlaneSet.Values
        |> Seq.tryFind (fun plane -> logName.Contains(normalizeLogName plane.LogName))
        |> Option.orElseWith (fun () ->
            // Look in plane alternatives, and if found return the plane it replaces.
            // There should be at most one such plane, otherwise the plane that's returned is whichever is matched first.
            this.World.PlaneAlts
            |> Seq.tryPick (fun kvp ->
                let plane, planeAlts = kvp.Key, kvp.Value
                let found =
                    planeAlts
                    |> List.exists (fun planeAlt -> logName.Contains(normalizeLogName planeAlt.LogName))
                if found then
                    this.World.PlaneSet.TryGetValue plane
                    |> Option.ofPair
                else
                    None
            )
        )

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
        match this.TryGetNearestAirfield(pos, None) with
        | Some (af, dist) when dist < 3000.0f ->
            let logName = normalizeLogName logName
            this.World.PlaneSet.Values
            |> Seq.tryFind (fun plane -> normalizeScript(plane.StaticScriptModel.Script) = logName)
            |> Option.map (fun plane -> af.AirfieldId, plane)
        | _ ->
            None

    /// Try to get the region at some position
    member this.TryGetRegionAt((x, _, z)) =
        let pos = Vector2(x, z)
        this.World.Regions.Values
        |> Seq.tryFind (fun region -> pos.IsInConvexPolygon region.Boundary)

    /// Get an existing pilot of a player given the starting airfield, or create a new pilot
    member this.GetPlayerPilotFrom(playerGuid : string, afId : AirfieldId, country, isFemale) =
        let logger = NLog.LogManager.GetCurrentClassLogger()

        let candidates =
            this.GetPlayerPilots(playerGuid)
            |> Seq.filter (fun pilot -> pilot.Country = country && pilot.IsFemale = isFemale)
            |> Seq.filter (fun pilot -> this.IsPilotHealthy(pilot.Id))
            |> Seq.filter (fun pilot -> match this.TryGetPilotHome(pilot.Id) with None -> true | Some home -> home = afId)
            |> Seq.sortByDescending (fun pilot -> pilot.LatestFlightStart, this.TryGetPilotHome(pilot.Id) = Some afId)
            |> Seq.cache

        logger.Debug(sprintf "Pilots of player %s: %s" playerGuid (candidates |> Seq.map (fun pilot -> pilot.FullName) |> String.concat ", "))

        Seq.tryHead candidates
        |> Option.defaultWith (fun () -> this.NewPilot(playerGuid, country, isFemale))

    /// Try to get the coalition of a country from its event log value
    member this.TryGetCoalitionOfCountry(country : int) =
        let country = CountryId.FromMcuValue(enum country)
        country
        |> Option.bind (fun country ->
            this.World.Countries.TryGetValue(country)
            |> Option.ofPair)

    member this.HealthStatusFromHealthLevel(timeStamp : System.TimeSpan, pilotHealth : float32) =
        let injury =
            1.0f - pilotHealth
        let healthStatus =
            if injury < 0.01f then
                Healthy
            else if injury >= 1.0f then
                Dead
            else
                this.Date + timeStamp + System.TimeSpan(int(ceil(injury * 30.0f)), 0, 0, 0)
                |> Injured
        healthStatus

type AmmoType with
    static member FromLogName(logName : string) : AmmoType = AmmoName logName

type Binding with
    member this.TargetType(war : IWarStateQuery) =
        match this.Name with
        | TargetTypeByName tt -> Some tt
        | _ ->
        let logName = normalizeLogName this.Typ
        let country = CountryId.FromMcuValue (enum this.Country)
        let countryGroundUnits =
            war.World.GroundUnitsList
            |> Seq.filter (fun vehicle ->
                match war.World.CountryOfGroundUnit.TryGetValue(vehicle.Id) with
                | false, _ -> false
                | true, c -> Some c = country)

        let dynVehicle =
            lazy
                countryGroundUnits
                |> Seq.filter(fun vehicle -> Some logName = (vehicle.DynamicScriptModel |> Option.map(fun data -> normalizeScript data.Script)))
                |> Seq.tryPick(fun vehicle ->
                    [ TargetType.Tank; TargetType.ArmoredCar; TargetType.Artillery; TargetType.Truck ]
                    |> List.tryFind (fun tt -> tt.IsCompatibleWith(vehicle))
                )
        let staVehicle =
            lazy
                countryGroundUnits
                |> Seq.filter(fun vehicle -> Some logName = (vehicle.StaticScriptModel |> Option.map(fun data -> normalizeScript data.Script)))
                |> Seq.tryPick(fun vehicle ->
                    [ TargetType.Tank; TargetType.ArmoredCar; TargetType.Artillery; TargetType.Truck ]
                    |> List.tryFind (fun tt -> tt.IsCompatibleWith(vehicle))
                )

        let convoyVehicle =
            lazy
                seq {
                    for v in ConvoyMember.All do
                        for c in CountryId.All do
                            yield v, c
                }
                |> Seq.tryFind (fun (vehicle, country) -> this.Name = vehicle.Name || normalizeScript (vehicle.StaticVehicleData country).Script = logName)
                |> Option.map (
                    fst
                    >> function
                    | Train -> TargetType.Train
                    | Truck -> TargetType.Truck
                    | Tank -> TargetType.Tank
                    | ArmoredCar -> TargetType.ArmoredCar
                    | AntiAirTruck -> TargetType.Artillery
                    | StaffCar -> TargetType.ArmoredCar
                )
        match convoyVehicle.Value with
        | Some x -> Some x
        | None ->
        match dynVehicle.Value with
        | Some x -> Some x
        | None ->
        match staVehicle.Value with
        | Some x -> Some x
        | None ->
            None

type StateMachineController<'S, 'E> =
    /// Called when a log line is received. Receives the old state, and produces intermediate data of type 'E that will be passed to HandlePostEvent
    abstract HandlePreEvent : state:'S * event:string -> 'E
    /// Called after HandlePreEvent with its result. Produces an updated state and a list of description, timestamp and optional command tuples.
    abstract HandlePostEvent : state:'S * preResult:'E -> 'S * (string * System.TimeSpan * Commands option) list

let handlePre (ctrl : StateMachineController<'S, 'E>, state, event) =
    ctrl.HandlePreEvent(state, event)

let handlePost (ctrl : StateMachineController<'S, 'E>, state, preResult) =
    ctrl.HandlePostEvent(state, preResult)

type Mappings =
    {
        Bindings : Map<int, Binding>
        PilotOf : Map<int, ObjectTaken>
        VehicleOf : Map<int, ObjectTaken>
        LatestHit : Map<int, {| Ammo : string; AttackerId : int; TargetId : int |}>
    }
with
    static member Empty =
        {
            Bindings = Map.empty
            PilotOf = Map.empty
            VehicleOf = Map.empty
            LatestHit = Map.empty
        }

    static member Controller =
        { new StateMachineController<Mappings, Mappings> with
            member this.HandlePreEvent(state, event) =
                match event with
                | ObjectEvent(_, ObjectBound binding) ->
                    { state with
                        Bindings = state.Bindings.Add(binding.Id, binding) }
                | PlayerEvent(_, PlayerTakesObject taken) ->
                    { state with
                        PilotOf = state.PilotOf.Add(taken.VehicleId, taken)
                        VehicleOf = state.VehicleOf.Add(taken.PilotId, taken)
                    }
                | PlayerEvent(_, PlayerEndsMission missionEnded) ->
                    { state with
                        PilotOf = state.PilotOf.Remove(missionEnded.VehicleId)
                        VehicleOf = state.VehicleOf.Remove(missionEnded.VehicleId)
                    }
                | ObjectEvent(_, ObjectHit hit) ->
                    { state with
                        LatestHit = state.LatestHit.Add(hit.TargetId, hit)}
                | _ ->
                    state

            member this.HandlePostEvent(state, preResult) = preResult, []
        }


/// A modified asyncSeq builder that update a war state whenever a command is yielded.
type ImpAsyncSeq(warState : IWarState, logger : NLog.Logger) =
    member this.Yield(v) =
        match v with
        | (_, _, Some (cmd : Commands)) ->
            try
                cmd.Execute(warState) |> ignore
            with
            | e ->
                logger.Error(sprintf "Execution of command in ImpAsyncSeq failed: %s" e.Message)
                logger.Debug(e)
        | _ -> ()
        asyncSeq.Yield(v)

    member this.YieldFrom(source) =
        asyncSeq.YieldFrom(source)

    member this.For(source : AsyncSeq<'T>, action) =
        asyncSeq.For(source, action)

    member this.For(source : 'T seq, action) =
        asyncSeq.For(source, action)

    member this.Zero() = asyncSeq.Zero()

    member this.Combine(seq1, seq2) = asyncSeq.Combine(seq1, seq2)

    member this.TryWith(body, handler) = asyncSeq.TryWith(body, handler)

    member this.Delay(f) = asyncSeq.Delay(f)

/// Extract war state updade commands from the game logs and update the war state.
let processLogs (state : WarState) (logs : AsyncSeq<string>) =
    let logger = NLog.LogManager.GetCurrentClassLogger()

    // We'll be mutating a clone of the state to avoid potential read/write race conditions with consumers (i.e. the live notifier) of the commands.
    let state = state.Clone()

    let impAsyncSeq =
        ImpAsyncSeq(state, logger)

    impAsyncSeq {
        let mutable mappings = Mappings.Empty
        let mappingsController = Mappings.Controller

        // Log Pilot ID to Pilot and FlightRecord
        let flightRecords : Dictionary<int, {| Pilot : Pilot; Record : FlightRecord |}> = Seq.mutableDict []
        // Object ID to float32
        let healthOf = Seq.mutableDict []
        // Pilot ID to pilot record
        let pilotRecordOf = Seq.mutableDict []

        let handleDamage(timeStamp, amount, targetId, position) =
            impAsyncSeq {
                match mappings.Bindings.TryGetValue(targetId) with
                | true, binding ->
                    // Emit DamageBuildingPart
                    let sub = binding.Sub |> Option.defaultValue -1
                    for building in state.GetBuildingsAt(binding.Typ, sub, position) do
                        yield "Damage to building", timeStamp, Some(DamageBuildingPart(building.Id, sub, amount))

                    // Emit RemovePlane for damages to static planes
                    match state.TryGetStaticPlaneAt(binding.Name, position) with
                    | Some(afId, plane) ->
                        yield sprintf "Damage to parked %s" plane.Name, timeStamp, Some(RemovePlane(afId, plane.Id, amount))
                    | None ->
                        ()

                    // Emit DestroyGroundForces for damages to ground forces
                    match binding.TargetType(state) with
                    | Some target when target.GroundForceValue > 0.0f<MGF> ->
                        let country : SturmovikMission.DataProvider.Mcu.CountryValue = enum binding.Country
                        match CountryId.FromMcuValue country, state.TryGetRegionAt(position) with
                        | Some country, Some region ->
                            match state.World.Countries.TryGetValue(country) with
                            | true, coalition ->
                                yield sprintf "Damage to ground forces %s" target.Description, timeStamp, Some(DestroyGroundForces(region.RegionId, coalition, amount * target.GroundForceValue))
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
                    match mappings.LatestHit.TryGetValue(targetId) with
                    | true, hit ->
                        AmmoType.FromLogName(hit.Ammo)
                    | false, _ ->
                        AmmoName "explosion"
                match mappings.Bindings.TryGetValue(targetId) with
                | true, binding ->
                    // Buildings
                    let sub = binding.Sub |> Option.defaultValue -1
                    for building in state.GetBuildingsAt(binding.Typ, sub, position) do
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
                    match binding.TargetType(state) with
                    | Some target ->
                        yield (target, ammo, amount)
                    | _ ->
                        ()
                | false, _ ->
                    ()
            ]

        /// Update flight record with damage
        let updateFlightRecord(attackerId, damage, targetId, position) =
            // Credit attacker
            match mappings.PilotOf.TryGetValue(attackerId) with
            | true, (taken : ObjectTaken) ->
                match flightRecords.TryGetValue(taken.PilotId) with
                | true, x ->
                    let differentCoalitions =
                        match mappings.Bindings.TryGetValue(attackerId), mappings.Bindings.TryGetValue(targetId) with
                        | (true, attacker), (true, target) ->
                            // Both IDs have bindings, and their respective coalitions could be identified, and they are different
                            (state.TryGetCoalitionOfCountry(attacker.Country), state.TryGetCoalitionOfCountry(target.Country))
                            ||> Option.map2 (fun c1 c2 -> c1 <> c2)
                            |> Option.defaultValue false // Default: coalitions aren't known to be different
                        | _ ->
                            false
                    if differentCoalitions then
                        let flight =
                            { x.Record with
                                TargetsDamaged = x.Record.TargetsDamaged @ recordedDamages(damage, targetId, position)
                            }
                        flightRecords.[taken.PilotId] <- {| x with Record = flight |}
                | false, _ ->
                    ()
            | false, _ ->
                ()

            // Record damage to target
            match mappings.PilotOf.TryGetValue(targetId) with
            | true, (taken : ObjectTaken) ->
                match flightRecords.TryGetValue(taken.PilotId) with
                | true, x ->
                    let flight =
                        { x.Record with PlaneHealth = x.Record.PlaneHealth - damage |> max 0.0f
                        }
                    flightRecords.[taken.PilotId] <- {| x with Record = flight |}
                | false, _ ->
                    ()
            | false, _ ->
                ()

        /// Update flight record with air kills
        let updateAirKills(pilotId, targetId) =
            match mappings.Bindings.TryGetValue(targetId), flightRecords.TryGetValue(pilotId) with
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

        /// Update flight record at landing or mission end
        let updateFlightRecordEnd(timeStamp, vehicleId, ownerOfSite, landSite, isEjection) =
            impAsyncSeq {
                match mappings.PilotOf.TryGetValue(vehicleId) with
                | true, taken ->
                    match flightRecords.TryGetValue(taken.PilotId) with
                    | true, x ->
                        let coalitionOfPilot =
                            enum taken.Country
                            |> CountryId.FromMcuValue
                            |> Option.map (fun country -> state.World.Countries.[country])
                        let inEnemyTerritory =
                            match coalitionOfPilot with
                            | Some c -> Some c.Other = ownerOfSite
                            | _ -> false
                        let retAf =
                            if inEnemyTerritory then
                                CrashedInEnemyTerritory
                            else
                                match landSite with
                                | Some (af, dist) ->
                                    if dist < 3000.0f && not isEjection then
                                        AtAirfield af.AirfieldId
                                    else
                                        CrashedInFriendlyTerritory (Some af.AirfieldId)
                                | _ ->
                                    CrashedInFriendlyTerritory None
                        let pilotHealth =
                            healthOf.TryGetValue(taken.PilotId)
                            |> Option.ofPair
                            |> Option.defaultValue 1.0f
                        let healthStatus = state.HealthStatusFromHealthLevel(timeStamp, pilotHealth)
                        let flight =
                            { x.Record with
                                PilotHealth = pilotHealth
                                PlaneHealth = if isEjection then 0.0f else x.Record.PlaneHealth
                                Length = state.Date + timeStamp - x.Record.Date
                                Return = retAf
                            }
                        flightRecords.[taken.PilotId] <- {| x with Record = flight |}
                        yield
                            sprintf "%s has %s"
                                x.Pilot.FullName
                                (string flight.Return),
                            timeStamp,
                            Some(RegisterPilotFlight(x.Pilot.Id, flight, healthStatus))
                        flightRecords.Remove(taken.PilotId) |> ignore
                    | false, _ ->
                        ()
                | false, _ ->
                    ()
            }

        /// Update flight record on death
        let updateFlightRecordOnDeath(timeStamp, pilotId) =
            impAsyncSeq {
                match flightRecords.TryGetValue(pilotId) with
                | true, x ->
                    let flight =
                        { x.Record with
                            PilotHealth = 0.0f
                            Length = state.Date + timeStamp - x.Record.Date
                            Return = KilledInAction
                        }
                    flightRecords.[pilotId] <- {| x with Record = flight |}
                    yield
                        sprintf "%s was killed in action"
                            x.Pilot.FullName,
                        timeStamp,
                        Some(RegisterPilotFlight(x.Pilot.Id, flight, Dead))
                    flightRecords.Remove(pilotId) |> ignore
                | false, _ ->
                    ()
            }

        let handleLine line =
            impAsyncSeq {
                let newMappings = handlePre(mappingsController, mappings, line)

                match line with
                | ObjectEvent(timeStamp, ObjectBound binding) ->
                    yield "Timestamp", timeStamp, None
                    logger.Debug(string timeStamp + " Updating mappings with binding " + Json.serialize binding)
                    // Update mappings
                    healthOf.[binding.Id] <- 1.0f

                | PlayerEvent(timeStamp, PlayerTakesObject taken) ->
                    logger.Debug(string timeStamp + " Updating mappings with taken " + Json.serialize taken)

                    // Emit RemovePlane command
                    match mappings.Bindings.TryGetValue(taken.VehicleId) with
                    | true, binding ->
                        match state.TryGetPlane(binding.Name) with
                        | Some plane ->
                            match state.TryGetNearestAirfield(taken.Position, None) with
                            | Some (airfield, _) ->
                                yield sprintf "Plane %s taken by player" plane.Name, timeStamp, Some(RemovePlane(airfield.AirfieldId, plane.Id, 1.0f))
                            | None ->
                                ()
                        | None ->
                            ()
                    | false, _ ->
                        ()

                    // Emit UpdatePlayer command
                    yield "Seen player " + taken.Name, timeStamp, Some(UpdatePlayer(taken.UserId, taken.Name))

                    // Choose pilot
                    match state.TryGetNearestAirfield(taken.Position, None) with
                    | Some (airfield, _) ->
                        match CountryId.FromMcuValue(enum taken.Country) with
                        | Some country ->
                            let pilot = state.GetPlayerPilotFrom(taken.UserId, airfield.AirfieldId, country, taken.HasFemaleCrew)
                            pilotRecordOf.[taken.PilotId] <- pilot
                        | _ ->
                            ()
                    | _ ->
                        ()
                    ()

                | ObjectEvent(timeStamp, ObjectTakesOff takeOff) ->
                    logger.Debug(string timeStamp + " Updating mappings with takenOff " + Json.serialize takeOff)
                    // Start flight record
                    let planeHealth =
                        healthOf.TryGetValue(takeOff.Id)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    match mappings.PilotOf.TryGetValue(takeOff.Id) with
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
                            match state.TryGetNearestAirfield(takeOff.Position, None) with
                            | Some (airfield, _) ->
                                let pilot =
                                    match pilotRecordOf.TryGetValue(taken.PilotId) with
                                    | true, x -> x
                                    | false, _ ->
                                        state.GetPlayerPilotFrom(taken.UserId, airfield.AirfieldId, country, taken.HasFemaleCrew)
                                let record : FlightRecord =
                                    {
                                        Date = state.Date + timeStamp
                                        Length = System.TimeSpan(0L)
                                        Plane = plane.Id
                                        PlaneHealth = planeHealth
                                        PilotHealth = pilotHealth
                                        Start = airfield.AirfieldId
                                        AirKills = 0
                                        TargetsDamaged = []
                                        Return = CrashedInEnemyTerritory // Meaningless, will get updated when player lands or crashes. Might be useful as a default for in-air disconnects.
                                    }
                                flightRecords.[taken.PilotId] <- {| Pilot = pilot; Record = record |}
                                yield sprintf "%s takes off in %s" pilot.FullName plane.Name, timeStamp, Some(UpdatePilot(pilot))
                            | None ->
                                logger.Warn("Could not find airfield of take-off")
                        | _ ->
                            ()
                    | false, _ -> ()

                | ObjectEvent(timeStamp, ObjectLands landing) ->
                    logger.Debug(string timeStamp + " Updating mappings with landing " + Json.serialize landing)
                    // Update flight record
                    let territory = state.TryGetRegionAt(landing.Position) |> Option.map (fun r -> r.RegionId) |> Option.bind state.GetOwner
                    let landSite = state.TryGetNearestAirfield(landing.Position, territory)
                    yield! updateFlightRecordEnd(timeStamp, landing.Id, territory, landSite, false)

                | ObjectEvent(timeStamp, ObjectDamaged damaged) ->
                    logger.Debug(string timeStamp + " Updating mappings with damaged " + Json.serialize damaged)
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
                    yield! handleDamage(timeStamp, damaged.Damage, damaged.TargetId, damaged.Position)

                | ObjectEvent(timeStamp, ObjectKilled killed) ->
                    logger.Debug(string timeStamp + " Updating mappings with killed " + Json.serialize killed)
                    // Update mappings
                    let oldHealth =
                        healthOf.TryGetValue(killed.TargetId)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    healthOf.[killed.TargetId] <- 0.0f
                    // Update flight record
                    match mappings.PilotOf.TryGetValue(killed.AttackerId) with
                    | true, taken ->
                        updateFlightRecord(taken.PilotId, oldHealth, killed.TargetId, killed.Position)
                        updateAirKills(taken.PilotId, killed.TargetId)
                    | false, _ ->
                        ()
                    yield! updateFlightRecordOnDeath(timeStamp, killed.TargetId)
                    // Emit commands
                    yield! handleDamage(timeStamp, oldHealth, killed.TargetId, killed.Position)

                | BotEvent(timeStamp, BotEject eject) ->
                    logger.Debug(string timeStamp + " Updaing mappings with eject" + Json.serialize eject)
                    // Update flight record
                    let territory = state.TryGetRegionAt(eject.Position) |> Option.map (fun r -> r.RegionId) |> Option.bind state.GetOwner
                    let landSite = state.TryGetNearestAirfield(eject.Position, territory)
                    yield! updateFlightRecordEnd(timeStamp, eject.ParentId, territory, landSite, true)

                | PlayerEvent(timeStamp, PlayerEndsMission missionEnded) ->
                    logger.Debug(string timeStamp + " Updating mappings with missionEnded " + Json.serialize missionEnded)
                    let afId =
                        flightRecords.TryGetValue(missionEnded.PilotId)
                        |> Option.ofPair
                        |> Option.bind (fun x -> state.TryGetReturnAirfield(x.Record))
                    // Emit AddPlane command
                    match mappings.Bindings.TryGetValue(missionEnded.VehicleId), afId, healthOf.TryGetValue(missionEnded.VehicleId) with
                    | (true, binding), Some afId, (true, health) when health > 0.0f ->
                        match state.TryGetPlane(binding.Name) with
                        | Some plane ->
                            yield sprintf "%s registered back at %s" plane.Name afId.AirfieldName, timeStamp, Some(AddPlane(afId, plane.Id, health))
                        | None ->
                            ()
                    | _ ->
                        ()
                    // Update pilot health status
                    match pilotRecordOf.TryGetValue(missionEnded.PilotId) with
                    | true, pilot ->
                        let pilotHealth = healthOf.TryGetValue(missionEnded.PilotId) |> Option.ofPair |> Option.defaultValue 1.0f
                        let pilot = { pilot with Health = state.HealthStatusFromHealthLevel(timeStamp, pilotHealth) }
                        yield sprintf "%s has ended their flight" pilot.FullName, timeStamp, Some(UpdatePilot pilot)
                    | false, _ ->
                        ()
                    // Update mappings
                    healthOf.Remove(missionEnded.VehicleId) |> ignore
                    healthOf.Remove(missionEnded.PilotId) |> ignore
                    pilotRecordOf.Remove(missionEnded.PilotId) |> ignore
                | _ ->
                    ()

                let newMappings, cmds = handlePost(mappingsController, mappings, newMappings)
                for cmd in cmds do
                    yield cmd
                mappings <- newMappings
            }

        let mutable finalTimeStamp = System.TimeSpan(0L)
        for line in logs do
            try
                yield! handleLine line
                match line with
                | MatchesRegex reBase (GroupList (AsInt ticks :: _)) when ticks > 0 ->
                    finalTimeStamp <- System.TimeSpan.OfGameTicks(ticks)
                | _ ->
                    ()
            with exc ->
                logger.Error("Exception while processing log")
                logger.Error(exc)

        // Update health status of players still in the air after the log ends.
        // Do not register their flight, that'll teach them to get back in time on the ground.
        for x in flightRecords.Values do
            let healthStatus = state.HealthStatusFromHealthLevel(finalTimeStamp, x.Record.PilotHealth)
            yield
                sprintf "%s is still in the air" x.Pilot.FullName,
                System.TimeSpan(System.Int64.MaxValue),
                Some(UpdatePilot { x.Pilot with Health = healthStatus })
    }