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

type StateMachineController<'S, 'E, 'Context> =
    /// Called when a log line is received. Receives the old state, and produces intermediate data of type 'E that will be passed to HandlePostEvent
    abstract HandlePreEvent : state:'S * event:string * context:'Context -> 'E
    /// Called after HandlePreEvent with its result. Produces an updated state and a list of description, timestamp and optional command tuples.
    abstract HandlePostEvent : state:'S * preResult:'E * context:'Context -> 'S * (string * System.TimeSpan * Commands option) list

let handlePre (ctrl : StateMachineController<'S, 'E, 'C>, state, event, ctx) =
    ctrl.HandlePreEvent(state, event, ctx)

let handlePost (ctrl : StateMachineController<'S, 'E, 'C>, state, preResult, ctx) =
    ctrl.HandlePostEvent(state, preResult, ctx)

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
        { new StateMachineController<Mappings, Mappings, unit> with
            member this.HandlePreEvent(state, event, ()) =
                match event with
                | ObjectEvent(_, ObjectBound binding) ->
                    { state with
                        Bindings = state.Bindings.Add(binding.Id, binding)
                    }
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
                        LatestHit = state.LatestHit.Add(hit.TargetId, hit)
                    }
                | _ ->
                    state

            member this.HandlePostEvent(state, preResult, ()) = preResult, []
        }

type HealthTracker =
    {
        HealthOf : Map<int, float32>
    }
with
    static member Empty = { HealthOf = Map.empty }

    static member Controller =
        { new StateMachineController<HealthTracker, HealthTracker, unit> with
            member this.HandlePreEvent(state, event, ()) =
                match event with
                | ObjectEvent(_, ObjectBound binding) ->
                    { state with
                        HealthOf = state.HealthOf.Add(binding.Id, 1.0f)
                    }
                | ObjectEvent(_, ObjectDamaged damaged) ->
                    let health =
                        state.HealthOf.TryFind(damaged.TargetId)
                        |> Option.defaultValue 1.0f
                    let health = health - damaged.Damage
                    { state with
                        HealthOf = state.HealthOf.Add(damaged.TargetId, health)
                    }
                | ObjectEvent(_, ObjectKilled killed) ->
                    { state with
                        HealthOf = state.HealthOf.Add(killed.TargetId, 0.0f)
                    }
                | PlayerEvent(_, PlayerEndsMission missionEnded) ->
                    { state with
                        HealthOf = state.HealthOf.Remove(missionEnded.VehicleId).Remove(missionEnded.PilotId)
                    }
                | _ ->
                    state

            member this.HandlePostEvent(state, preResult, ()) = preResult, []
        }

type PlayerFlightState =
    | Spawned of AirfieldId
    | InFlight
    | BackOnGround
    | Destroyed
    | Ended

type PlayerFlightTracker =
    {
        PilotId : int
        VehicleId : int
        PilotData : Pilot
        FlightState : PlayerFlightState
        FlightRecord : FlightRecord
    }
with
    static member Create(pId, vId, afId, date, pilot, plane) =
        {
            PilotId = pId
            VehicleId = vId
            PilotData = pilot
            FlightState = Spawned afId
            FlightRecord = 
                {
                    Date = date
                    Length = System.TimeSpan(0L)
                    Plane = plane
                    PlaneHealth = 1.0f
                    AirKills = 0
                    Start = afId
                    TargetsDamaged = []
                    Return = CrashedInEnemyTerritory
                    PilotHealth = 1.0f
                }
        }

    static member Controller =
        { new StateMachineController<PlayerFlightTracker, _, Mappings * HealthTracker * IWarStateQuery> with
            member this.HandlePreEvent(state, event, (mappings, healths, war)) =
                let planeName =
                    war.World.PlaneSet.TryGetValue(state.FlightRecord.Plane)
                    |> Option.ofPair
                    |> Option.map (fun plane -> plane.Name)
                    |> Option.defaultValue (string state.FlightRecord.Plane)

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
                            for building in war.GetBuildingsAt(binding.Typ, sub, position) do
                                yield (TargetType.Building(building.Id, sub), ammo, amount)

                            // Parked planes
                            match war.TryGetStaticPlaneAt(binding.Name, position) with
                            | Some(afId, plane) ->
                                yield (TargetType.ParkedPlane(afId, plane.Id), ammo, amount)
                            | None ->
                                ()

                            // Flying planes
                            match war.TryGetPlane(binding.Name) with
                            | Some plane ->
                                yield (TargetType.Air(plane.Id), ammo, amount)
                            | None ->
                                ()

                            // Others
                            match binding.TargetType(war) with
                            | Some target ->
                                yield (target, ammo, amount)
                            | _ ->
                                ()
                        | false, _ ->
                            ()
                    ]

                let updateFlightRecord(attackerId, targetId, damage, position, killDelta) =
                    let airKillDelta =
                        match mappings.Bindings.TryFind(targetId) with
                        | Some binding ->
                            match war.TryGetPlane(binding.Name) with
                            | Some _ -> killDelta
                            | None -> 0
                        | _ ->
                            0
                    // Credit attacker
                    if attackerId = state.PilotId then
                        let differentCoalitions =
                            match mappings.Bindings.TryGetValue(attackerId), mappings.Bindings.TryGetValue(targetId) with
                            | (true, attacker), (true, target) ->
                                // Both IDs have bindings, and their respective coalitions could be identified, and they are different
                                (war.TryGetCoalitionOfCountry(attacker.Country), war.TryGetCoalitionOfCountry(target.Country))
                                ||> Option.map2 (fun c1 c2 -> c1 = c2.Other)
                                |> Option.defaultValue false // Default: coalitions aren't known to be different
                            | _ ->
                                false
                        if differentCoalitions then
                            let flight =
                                { state.FlightRecord with
                                    TargetsDamaged = state.FlightRecord.TargetsDamaged @ recordedDamages(damage, targetId, position)
                                    AirKills = state.FlightRecord.AirKills + airKillDelta
                                }
                            { state with
                                FlightRecord = flight
                            },
                            []
                        else
                            state, []
                    // Damage victim's plane
                    elif targetId = state.VehicleId then
                        let health = state.FlightRecord.PlaneHealth - damage |> max 0.0f
                        let flight =
                            { state.FlightRecord with
                                PlaneHealth = health
                            }
                        { state with
                            FlightRecord = flight
                            FlightState = if health <= 0.0f then Destroyed else state.FlightState
                        },
                        []
                    // Hurt victim
                    elif targetId = state.PilotId then
                        let health = state.FlightRecord.PilotHealth - damage |> max 0.0f
                        let isKilled = health <= 0.0f || killDelta > 0
                        let flight =
                            { state.FlightRecord with
                                PilotHealth = health
                                Return = if isKilled then KilledInAction else state.FlightRecord.Return
                            }
                        { state with
                            FlightRecord = flight
                        },
                        []
                    else
                        state, []

                let updateFlightRecordEnd(timeStamp, position, ejected) =
                    let ownerOfSite = war.TryGetRegionAt(position) |> Option.map (fun r -> r.RegionId) |> Option.bind war.GetOwner
                    let landSite = war.TryGetNearestAirfield(position, ownerOfSite)
                    let coalitionOfPilot = war.World.Countries.[state.PilotData.Country]
                    let inEnemyTerritory = (Some coalitionOfPilot.Other = ownerOfSite)
                    let retAf =
                        if state.FlightRecord.Return = KilledInAction then
                            KilledInAction
                        elif inEnemyTerritory then
                            CrashedInEnemyTerritory
                        elif ejected then
                            CrashedInFriendlyTerritory (landSite |> Option.map fst |> Option.map (fun af -> af.AirfieldId))
                        else
                            match landSite with
                            | Some (af, dist) ->
                                if dist < 3000.0f then
                                    AtAirfield af.AirfieldId
                                else
                                    CrashedInFriendlyTerritory (Some af.AirfieldId)
                            | _ ->
                                CrashedInFriendlyTerritory None
                    let healthStatus = war.HealthStatusFromHealthLevel(timeStamp, state.FlightRecord.PilotHealth)
                    let flight =
                        { state.FlightRecord with
                            Length = war.Date + timeStamp - state.FlightRecord.Date
                            Return = retAf
                        }
                    { state with
                        FlightRecord = flight
                        FlightState = if ejected || state.FlightState = Destroyed then Destroyed else BackOnGround
                    },
                    [
                        sprintf "%s has %s"
                            state.PilotData.FullName
                            (string flight.Return),
                        timeStamp,
                        Some(RegisterPilotFlight(state.PilotData.Id, flight, healthStatus));

                        sprintf "%s is back on the ground" state.PilotData.FullName,
                        timeStamp,
                        Some(UpdatePilot state.PilotData)
                    ]

                match event with
                | ObjectEvent(timeStamp, ObjectTakesOff takeOff) ->
                    // Update take off location and time
                    match war.TryGetNearestAirfield(takeOff.Position, None) with
                    | Some (airfield, _) ->
                        { state with
                            FlightRecord =
                                { state.FlightRecord with
                                    Date = war.Date + timeStamp
                                    Start = airfield.AirfieldId
                                }
                            FlightState = InFlight
                        },
                        [sprintf "%s takes off in %s" state.PilotData.FullName planeName, timeStamp, Some(UpdatePilot(state.PilotData))]
                    | None ->
                        { state with
                            FlightRecord =
                                { state.FlightRecord with
                                    Date = war.Date + timeStamp
                                }
                            FlightState = InFlight
                        },
                        [sprintf "%s takes off in %s" state.PilotData.FullName planeName, timeStamp, Some(UpdatePilot(state.PilotData))]

                | ObjectEvent(timeStamp, ObjectLands landing) ->
                    updateFlightRecordEnd(timeStamp, landing.Position, false)

                | ObjectEvent(timeStamp, ObjectDamaged damaged) ->
                    updateFlightRecord(damaged.AttackerId, damaged.TargetId, damaged.Damage, damaged.Position, 0)

                | ObjectEvent(timeStamp, ObjectKilled killed) ->
                    let oldHealth =
                        healths.HealthOf.TryGetValue(killed.TargetId)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    updateFlightRecord(killed.AttackerId, killed.TargetId, oldHealth, killed.Position, 1)

                | BotEvent(timeStamp, BotEject eject) ->
                    updateFlightRecordEnd(timeStamp, eject.Position, true)

                | _ ->
                    state, []

            member this.HandlePostEvent(state, (state2, preCmds), (mappings, healths, war)) =
                state2, preCmds
        }

type PlayerFlights =
    {
        FlightOfPilot : Map<int, PlayerFlightTracker>
    }
with
    static member Empty = { FlightOfPilot = Map.empty }

    static member Controller =
        let pftCtrl = PlayerFlightTracker.Controller
        { new StateMachineController<PlayerFlights, _, Mappings * HealthTracker * IWarStateQuery> with
            member this.HandlePreEvent(state, event, (mappings, healths, war)) =
                match event with
                | PlayerEvent(timeStamp, PlayerTakesObject taken) ->

                    // Emit RemovePlane command
                    let cmds =
                        match mappings.Bindings.TryGetValue(taken.VehicleId) with
                        | true, binding ->
                            match war.TryGetPlane(binding.Name) with
                            | Some plane ->
                                match war.TryGetNearestAirfield(taken.Position, None) with
                                | Some (airfield, _) ->
                                    [sprintf "Plane %s taken by player" plane.Name, timeStamp, Some(RemovePlane(airfield.AirfieldId, plane.Id, 1.0f))]
                                | None ->
                                    []
                            | None ->
                                []
                        | false, _ ->
                            []

                    // Emit UpdatePlayer command
                    let cmds = cmds @ ["Seen player " + taken.Name, timeStamp, Some(UpdatePlayer(taken.UserId, taken.Name))]

                    let state2, cmds =
                        // Choose pilot
                        match war.TryGetNearestAirfield(taken.Position, None), CountryId.FromMcuValue(enum taken.Country), war.TryGetPlane(taken.Typ) with
                        | Some(airfield, _), Some country, Some plane ->
                            let pilot = war.GetPlayerPilotFrom(taken.UserId, airfield.AirfieldId, country, taken.HasFemaleCrew)
                            let flight = PlayerFlightTracker.Create(taken.PilotId, taken.VehicleId, airfield.AirfieldId, war.Date + timeStamp, pilot, plane.Id)
                            let flight, flightCmds = pftCtrl.HandlePreEvent(flight, event, (mappings, healths, war))
                            { state with
                                FlightOfPilot = state.FlightOfPilot.Add(taken.PilotId, flight)
                            }, cmds @ flightCmds
                        | _ ->
                            state, cmds

                    state2, cmds

                | PlayerEvent(_, PlayerEndsMission missionEnded) ->
                    // Gather commands from ending the flight
                    let cmds =
                        state.FlightOfPilot.TryFind(missionEnded.PilotId)
                        |> Option.map (fun flight -> pftCtrl.HandlePreEvent(flight, event, (mappings, healths, war)))
                        |> Option.map snd
                        |> Option.defaultValue []
                    // Unregister the flight
                    { state with
                        FlightOfPilot = state.FlightOfPilot.Remove(missionEnded.PilotId)
                    },
                    cmds

                | _ ->
                    // Dispatch other events to all flights
                    let flightsAndCmds =
                        state.FlightOfPilot
                        |> Map.map (fun pilotId flight -> pftCtrl.HandlePreEvent(flight, event, (mappings, healths, war)))
                    let flights =
                        flightsAndCmds
                        |> Map.map (fun _ -> fst)
                    let cmds =
                        flightsAndCmds
                        |> Map.toSeq
                        |> Seq.collect (snd >> snd)
                        |> List.ofSeq
                    { state with
                        FlightOfPilot = flights
                    },
                    cmds

            member this.HandlePostEvent(state, (state2, cmds), context) =
                // Dispatch
                let flightsAndCmds =
                    state.FlightOfPilot
                    |> Map.map (fun pilotId flight ->
                        match state2.FlightOfPilot.TryFind pilotId with
                        | Some flight2 ->
                            pftCtrl.HandlePostEvent(flight, (flight2, []), context)
                        | None ->
                            flight, [])
                // Extract flights
                let flights =
                    flightsAndCmds
                    |> Map.map (fun _ -> fst)
                // Extract flight-generated commands and add them to the commands from the Pre handler
                let cmds2 =
                    ([cmds], flightsAndCmds)
                    ||> Map.fold (fun cmdGroups _ (_, cmds) -> cmds :: cmdGroups)
                    |> List.rev
                    |> List.concat
                // Result
                { state2 with
                    FlightOfPilot = flights
                }, cmds2
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

        let mutable healths = HealthTracker.Empty
        let healthController = HealthTracker.Controller

        let mutable pilots = PlayerFlights.Empty
        let pilotsController = PlayerFlights.Controller

        let handleLine line =
            impAsyncSeq {
                // Controllers PRE
                let newMappings = handlePre(mappingsController, mappings, line, ())
                let newHealths = handlePre(healthController, healths, line, ())
                let newPilots = handlePre(pilotsController, pilots, line, (mappings, healths, upcast state))

                // Emit timestamp, needed to inform players of time left in mission
                match line with
                | ObjectEvent(timeStamp, _) | MissionEvent(timeStamp, _) | PlayerEvent(timeStamp, _) | BotEvent(timeStamp, _) ->
                    yield "Timestamp", timeStamp, None

                | _ ->
                    ()

                // Controllers POST
                let newMappings, cmds = handlePost(mappingsController, mappings, newMappings, ())
                let newHealths, cmds2 = handlePost(healthController, healths, newHealths, ())
                let newPilots, cmds3 = handlePost(pilotsController, pilots, newPilots, (mappings, healths, upcast state))

                for cmd in Seq.concat [ cmds; cmds2; cmds3 ] do
                    yield cmd

                mappings <- newMappings
                healths <- newHealths
                pilots <- newPilots
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
        for x in pilots.FlightOfPilot.Values do
            let healthStatus = state.HealthStatusFromHealthLevel(finalTimeStamp, x.FlightRecord.PilotHealth)
            yield
                sprintf "%s is still in the air" x.PilotData.FullName,
                System.TimeSpan(System.Int64.MaxValue),
                Some(UpdatePilot { x.PilotData with Health = healthStatus })
    }