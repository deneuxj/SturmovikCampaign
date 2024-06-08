// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2021 Johann Deneux <johann.deneux@gmail.com>
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

/// State machines for processing of game logs
module Campaign.MissionResultsStateMachines

open FSharp.Control
open System.Numerics

open Util
open Util.RegexActivePatterns
open VectorExtension

open Campaign.Common.BasicTypes
open Campaign.Common.Targets

open Campaign.GameLogEvents
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.Pilots
open Campaign.MissionResultsExtensions

[<RequireQualifiedAccess>]
type LiveMessageAudience =
    | Coalition of CoalitionId
    | All
    | Player of string

type AnnotatedCommand =
    {
        Description : string
        TimeStamp : System.TimeSpan
        Command : Commands option
        LiveAudience : LiveMessageAudience option
    }
with
    static member Create(description, time, ?command) =
        {
            Description = description
            TimeStamp = time
            Command = command
            LiveAudience = None
        }

    member this.SetAudience(audience) =
        { this with
            LiveAudience = Some audience
        }

    member this.SetAudienceOpt(audience) =
        { this with
            LiveAudience = audience
        }

type StateMachineController<'S, 'E, 'Context> =
    /// Called when a log line is received. Receives the old state, and produces intermediate data of type 'E that will be passed to HandlePostEvent
    abstract HandlePreEvent : state:'S * event:string * context:'Context -> 'E
    /// Called after HandlePreEvent with its result. Produces an updated state and a list of description, timestamp and optional command tuples.
    abstract HandlePostEvent : state:'S * preResult:'E * context:'Context -> 'S * AnnotatedCommand list

let handlePre (ctrl : StateMachineController<'S, 'E, 'C>, state, event, ctx) =
    ctrl.HandlePreEvent(state, event, ctx)

let handlePost (ctrl : StateMachineController<'S, 'E, 'C>, state, preResult, ctx) =
    ctrl.HandlePostEvent(state, preResult, ctx)

/// Vehicle and pilot IDs mappings
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
        let logger = NLog.LogManager.GetCurrentClassLogger()
        { new StateMachineController<Mappings, Mappings, unit> with
            member this.HandlePreEvent(state, event, ()) =
                match event with
                | ObjectEvent(_, ObjectBound binding) ->
                    logger.Debug(sprintf "Updating bindings with %s" event)
                    { state with
                        Bindings = state.Bindings.Add(binding.Id, binding)
                    }
                | PlayerEvent(_, PlayerTakesObject taken) ->
                    logger.Debug(sprintf "Updating pilot and vehicle mappings with %s" event)
                    { state with
                        PilotOf = state.PilotOf.Add(taken.VehicleId, taken)
                        VehicleOf = state.VehicleOf.Add(taken.PilotId, taken)
                    }
                | PlayerEvent(_, PlayerEndsMission missionEnded) ->
                    logger.Debug(sprintf "Clearing pilot and vehicle mappings after %s" event)
                    { state with
                        PilotOf = state.PilotOf.Remove(missionEnded.VehicleId)
                        VehicleOf = state.VehicleOf.Remove(missionEnded.VehicleId)
                    }
                | ObjectEvent(_, ObjectHit hit) ->
                    logger.Debug(sprintf "Recording latest hit with %s" event)
                    { state with
                        LatestHit = state.LatestHit.Add(hit.TargetId, hit)
                    }
                | _ ->
                    state

            member this.HandlePostEvent(state, preResult, ()) = preResult, []
        }

/// Tracks health of objects, vehicles and pilots
type HealthTracker =
    {
        HealthOf : Map<int, float32>
    }
with
    static member Empty = { HealthOf = Map.empty }

    static member Controller =
        let logger = NLog.LogManager.GetCurrentClassLogger()
        { new StateMachineController<HealthTracker, HealthTracker * AnnotatedCommand list, Mappings * IWarStateQuery> with
            member this.HandlePreEvent(state, event, (mappings, war)) =

                /// Generate commands that reflect the effects of inflicted damages
                let handleDamages(timestamp, amount, objectId, position) =
                    match mappings.Bindings.TryFind(objectId) with
                    | Some binding ->
                        // Check if it's a parked plane
                        match war.TryGetStaticPlaneAt(binding.Typ, position) with
                        | Some(afId, planeModel) ->
                            // There might be multiple planes that share the same static model. Pick the first one that is available at the airfield.
                            let actualPlane =
                                war.GetNumPlanes(afId)
                                |> Map.tryFindKey (fun plane qty -> war.World.PlaneSet.[plane].StaticScriptModel = planeModel.StaticScriptModel && qty >= amount)
                                |> Option.defaultValue planeModel.Id
                            [AnnotatedCommand.Create(sprintf "Parked %s damaged with %2.1f%%" planeModel.Name (100.0f * amount), timestamp, RemovePlane(afId, actualPlane, amount))
                            ]
                        | None ->

                        // Report damages to all building parts that cover the position, typically
                        // that should be at most one, unless there are building groups overlaps.
                        let part = defaultArg binding.Sub -1
                        let buildings = war.GetBuildingsAt(binding.Typ, part, position) |> Seq.cache
                        if not(Seq.isEmpty buildings) then
                            buildings
                            |> Seq.choose (fun building ->
                                match war.World.BuildingProperties.TryGetValue building.Script with
                                | true, properties ->
                                    AnnotatedCommand.Create(
                                        sprintf "Building %s damaged with %2.1f%%" properties.Model (100.0f * amount),
                                        timestamp,
                                        DamageBuildingPart(building.Id, part, amount))
                                    |> Some
                                | false, _ ->
                                    None)
                            |> List.ofSeq
                        else

                        // Report damages to ground forces
                        match binding.TryGetVehicleTargetType(war), war.TryGetRegionAt(position) with
                        | Some vehicle, Some region ->
                            let value = vehicle.GroundForceValue * amount
                            let coalition =
                                CountryId.FromMcuValue(enum binding.Country)
                                |> Option.bind (war.World.Countries.TryGetValue >> Option.ofPair)
                            match coalition with
                            | Some coalition ->
                                [AnnotatedCommand.Create(sprintf "%s damaged with %2.1f%%" vehicle.Description (100.0f * amount), timestamp, DestroyGroundForces(region.RegionId, coalition, value))]
                            | None ->
                                []
                        | _ ->
                            []
                    | None ->
                        []

                match event with
                | ObjectEvent(_, ObjectBound binding) ->
                    logger.Debug(sprintf "Reset health after %s" event)
                    { state with
                        HealthOf = state.HealthOf.Add(binding.Id, 1.0f)
                    }, []
                | ObjectEvent(timeStamp, ObjectDamaged damaged) ->
                    logger.Debug(sprintf "Decreasing health after %s" event)
                    let health =
                        state.HealthOf.TryFind(damaged.TargetId)
                        |> Option.defaultValue 1.0f
                    let health = health - damaged.Damage
                    { state with
                        HealthOf = state.HealthOf.Add(damaged.TargetId, health)
                    },
                    handleDamages(timeStamp, damaged.Damage, damaged.TargetId, damaged.Position)
                | ObjectEvent(timeStamp, ObjectKilled killed) ->
                    logger.Debug(sprintf "Wiping health after %s" event)
                    let health =
                        state.HealthOf.TryFind(killed.TargetId)
                        |> Option.defaultValue 1.0f
                    { state with
                        HealthOf = state.HealthOf.Add(killed.TargetId, 0.0f)
                    },
                    handleDamages(timeStamp, health, killed.TargetId, killed.Position)
                | PlayerEvent(_, PlayerEndsMission missionEnded) ->
                    logger.Debug(sprintf "Clearing health entry after %s" event)
                    { state with
                        HealthOf = state.HealthOf.Remove(missionEnded.VehicleId).Remove(missionEnded.PilotId)
                    }, []
                | _ ->
                    state, []

            member this.HandlePostEvent(_, (newState, cmds), _) = newState, cmds
        }

/// Track status of a player after they spawn, until they end the mission or leave the server
type PlayerMissionStatus =
    | Spawned of AirfieldId
    | InFlight
    | BackOnGround
    | Ejected
    | Ended

type PlayerFlightTracker =
    {
        PilotId : int
        VehicleId : int
        PilotData : Pilot
        MissionStatus : PlayerMissionStatus
        FlightRecord : FlightRecord
    }
with
    static member Create(pId, vId, afId, date, pilot, plane) =
        {
            PilotId = pId
            VehicleId = vId
            PilotData = pilot
            MissionStatus = Spawned afId
            FlightRecord = 
                {
                    Date = date
                    Length = System.TimeSpan(0L)
                    Plane = plane
                    PlaneHealth = 1.0f
                    AirKills = 0
                    Start = afId
                    TargetsDamaged = []
                    Return = AtAirfield afId
                    PilotHealth = 1.0f
                }
        }

    static member Controller =
        let logger = NLog.LogManager.GetCurrentClassLogger()
        { new StateMachineController<PlayerFlightTracker, PlayerFlightTracker * AnnotatedCommand list, Mappings * HealthTracker * IWarStateQuery> with
            member this.HandlePreEvent(state, event, (mappings, healths, war)) =
                let coalition =
                    state.PilotData.Country
                    |> war.World.Countries.TryGetValue
                    |> Option.ofPair

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
                            match war.TryGetStaticPlaneAt(binding.Typ, position) with
                            | Some(afId, plane) ->
                                yield (TargetType.ParkedPlane(afId, plane.Id, plane.Kind), ammo, amount)
                            | None ->
                                ()

                            // Flying planes
                            match war.TryGetPlane(binding.Typ) with
                            | Some plane ->
                                yield (TargetType.Air(plane.Id, plane.Kind), ammo, amount)
                            | None ->
                                ()

                            // Others
                            match binding.TryGetVehicleTargetType(war) with
                            | Some target ->
                                yield (target, ammo, amount)
                            | _ ->
                                ()
                        | false, _ ->
                            ()
                    ]

                let updateFlightRecord(timestamp, attackerId, targetId, damage, position, killDelta) =
                    // Update return status of the flight depending on position
                    let retStatus =
                        match state.MissionStatus with
                        | InFlight ->
                            if targetId = state.VehicleId || targetId = state.PilotId || attackerId = state.VehicleId then
                                let startCoalition =
                                    state.FlightRecord.Start
                                    |> war.World.Airfields.TryGetValue
                                    |> Option.ofPair
                                    |> Option.map (fun af -> af.Region)
                                    |> Option.bind war.GetOwner
                                match war.TryGetRegionAt(position) with
                                | Some region ->
                                    let owner = war.GetOwner(region.RegionId)
                                    let coalition = war.World.Countries.[state.PilotData.Country]
                                    match owner with
                                    | Some enemy when enemy = coalition.Other && startCoalition <> Some enemy -> CrashedInEnemyTerritory
                                    | _ -> CrashedInFriendlyTerritory None
                                | None ->
                                    CrashedInFriendlyTerritory None
                            else
                                state.FlightRecord.Return
                        | _ ->
                            state.FlightRecord.Return
                    let state =
                        { state with
                            FlightRecord = { state.FlightRecord with Return = retStatus }
                        }
                    // Credit attacker
                    if attackerId = state.VehicleId || attackerId = state.PilotId then
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
                            let airKillDelta =
                                match mappings.Bindings.TryFind(targetId) with
                                | Some binding ->
                                    match war.TryGetPlane(binding.Typ) with
                                    | Some _ -> killDelta
                                    | None -> 0
                                | _ ->
                                    0
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
                        },
                        []
                    // Hurt victim
                    elif targetId = state.PilotId then
                        let health = state.FlightRecord.PilotHealth - damage |> max 0.0f
                        let isKilled = health <= 0.0f || killDelta > 0
                        // In case killDelta > 0 and health is very low but not 0, force to 0
                        let health = if isKilled then 0.0f else health
                        let flight =
                            { state.FlightRecord with
                                PilotHealth = health
                                Return = if isKilled then KilledInAction else state.FlightRecord.Return
                            }
                        { state with
                            PilotData = { state.PilotData with Health = war.HealthStatusFromHealthLevel(timestamp, health) }
                            FlightRecord = flight
                        },
                        []
                    else
                        state, []

                let updateFlightRecordEnd(timeStamp, position, ejected, landed) =
                    let ownerOfStart =
                        state.FlightRecord.Start
                        |> war.World.Airfields.TryGetValue 
                        |> Option.ofPair
                        |> Option.map (fun af -> af.Region)
                        |> Option.bind war.GetOwner
                    let ownerOfSite = war.TryGetRegionAt(position) |> Option.map (fun r -> r.RegionId) |> Option.bind war.GetOwner
                    let landSite = war.TryGetNearestAirfield(position, ownerOfSite)
                    let coalitionOfPilot = war.World.Countries.[state.PilotData.Country]
                    let inEnemyTerritory = (Some coalitionOfPilot.Other = ownerOfSite) && (ownerOfSite <> ownerOfStart)
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
                    let flight =
                        { state.FlightRecord with
                            Length = war.Date + timeStamp - state.FlightRecord.Date
                            Return = retAf
                        }
                    let state =
                        { state with
                            FlightRecord = flight
                            MissionStatus = if ejected then Ejected else if landed then BackOnGround else state.MissionStatus
                        }
                    let liveMessages =
                        match coalition with
                        | None -> []
                        | Some coalition ->
                        let rank =
                            Pilots.tryComputeRank war.World.Ranks (war.GetPilot state.PilotData.Id)
                            |> Option.map (fun rank -> rank.RankAbbrev)
                            |> Option.defaultValue ""
                        let eventDescription =
                            match flight.Return with
                            | CrashedInEnemyTerritory -> "crashed in enemy territory"
                            | CrashedInFriendlyTerritory _ -> "crash-landed"
                            | AtAirfield afId -> sprintf "landed at %s" afId.AirfieldName
                            | KilledInAction -> "was killed in action"
                        let msg = sprintf "%s %s has %s" rank state.PilotData.FullName eventDescription
                        [
                            yield AnnotatedCommand.Create(msg, timeStamp).SetAudience(LiveMessageAudience.Coalition coalition)
                            match state.PilotData.Health with
                            | Pilots.Healthy -> ()
                            | Pilots.Dead ->
                                let msg = sprintf "The career of %s %s has ended" rank state.PilotData.PilotLastName
                                yield AnnotatedCommand.Create(msg, timeStamp).SetAudience(LiveMessageAudience.Coalition coalition)
                            | Pilots.Injured until ->
                                let msg = sprintf "%s %s is injured until at least %s" rank state.PilotData.PilotLastName (until.ToString(state.PilotData.Country.CultureInfo))
                                yield AnnotatedCommand.Create(msg, timeStamp).SetAudience(LiveMessageAudience.Coalition coalition)
                        ]
                    state,
                    (
                        AnnotatedCommand.Create(
                            sprintf "%s %s" state.PilotData.FullName (if ejected then "jumps" else "is back on the ground"),
                            timeStamp,
                            UpdatePilot state.PilotData)
                    ) :: liveMessages

                match event with
                | PlayerEvent(timeStamp, PlayerTakesObject taken) when taken.PilotId = state.PilotId ->
                    state, [AnnotatedCommand.Create(sprintf "%s enters a %s" state.PilotData.FullName planeName, timeStamp, UpdatePilot(state.PilotData))]

                | ObjectEvent(timeStamp, ObjectTakesOff takeOff) when takeOff.Id = state.VehicleId ->
                    logger.Debug(sprintf "Recording take off of %s after %s" state.PilotData.FullName event)
                    // Update take off location and time
                    match war.TryGetNearestAirfield(takeOff.Position, None) with
                    | Some (airfield, _) ->
                        { state with
                            FlightRecord =
                                { state.FlightRecord with
                                    Date = war.Date + timeStamp
                                    Start = airfield.AirfieldId
                                    Return = CrashedInFriendlyTerritory(Some airfield.AirfieldId)
                                }
                            MissionStatus = InFlight
                        },
                        let cmd =
                            AnnotatedCommand.Create(sprintf "%s takes off in %s from %s" state.PilotData.FullName planeName airfield.AirfieldId.AirfieldName, timeStamp, UpdatePilot(state.PilotData))
                                .SetAudienceOpt(coalition |> Option.map LiveMessageAudience.Coalition)
                        [cmd]
                    | None ->
                        { state with
                            FlightRecord =
                                { state.FlightRecord with
                                    Date = war.Date + timeStamp
                                    Return = CrashedInFriendlyTerritory None
                                }
                            MissionStatus = InFlight
                        },
                        [AnnotatedCommand.Create(sprintf "%s takes off in %s" state.PilotData.FullName planeName, timeStamp, UpdatePilot(state.PilotData))]

                | ObjectEvent(timeStamp, ObjectLands landing) when landing.Id = state.VehicleId ->
                    match state.MissionStatus with
                    | BackOnGround ->
                        logger.Debug(sprintf "Spurious landing event for %s: %s" state.PilotData.FullName event)
                        state, []
                    | _ ->
                        logger.Debug(sprintf "End flight of %s after landing %s" state.PilotData.FullName event)
                        let state, cmds = updateFlightRecordEnd(timeStamp, landing.Position, false, true)
                        state, cmds

                | ObjectEvent(timeStamp, ObjectDamaged damaged)
                        when Seq.allPairs [damaged.AttackerId; damaged.TargetId] [state.PilotId; state.VehicleId] |> Seq.exists (fun (x, y) -> x = y) ->
                    logger.Debug(sprintf "Recording damage for/to %s after damage %s" state.PilotData.FullName event)
                    updateFlightRecord(timeStamp, damaged.AttackerId, damaged.TargetId, damaged.Damage, damaged.Position, 0)

                | ObjectEvent(timeStamp, ObjectKilled killed)
                        when Seq.allPairs [killed.AttackerId; killed.TargetId] [state.PilotId; state.VehicleId] |> Seq.exists (fun (x, y) -> x = y) ->
                    logger.Debug(sprintf "Recording damage for/to %s after kill %s" state.PilotData.FullName event)
                    let oldHealth =
                        healths.HealthOf.TryGetValue(killed.TargetId)
                        |> Option.ofPair
                        |> Option.defaultValue 1.0f
                    updateFlightRecord(timeStamp, killed.AttackerId, killed.TargetId, oldHealth, killed.Position, 1)

                | BotEvent(timeStamp, BotEject eject)
                        when eject.BotId = state.PilotId || eject.ParentId = state.VehicleId ->
                    logger.Debug(sprintf "End flight of %s after ejection %s" state.PilotData.FullName event)
                    updateFlightRecordEnd(timeStamp, eject.Position, true, false)

                | PlayerEvent(timeStamp, PlayerEndsMission missionEnded)
                        when missionEnded.PilotId = state.PilotId ->
                    match state.MissionStatus with
                    | Ended ->
                        logger.Debug(sprintf "Spurious mission ended for %s %s" state.PilotData.FullName event)
                        state, []
                    | _ ->
                        logger.Debug(sprintf "Registering flight of %s after end of mission %s" state.PilotData.FullName event)
                        let addPlane =
                            match state.MissionStatus, state.FlightRecord.Return with
                            | BackOnGround, (CrashedInFriendlyTerritory(Some afId) | AtAirfield afId) ->
                                [
                                    AnnotatedCommand.Create(
                                        sprintf "%s returns a %s at %s" state.PilotData.FullName planeName afId.AirfieldName,
                                        timeStamp,
                                        AddPlane(afId, state.FlightRecord.Plane, state.FlightRecord.PlaneHealth))
                                        .SetAudienceOpt(coalition |> Option.map LiveMessageAudience.Coalition)
                                ]
                            | Spawned afId, _ ->
                                [
                                    AnnotatedCommand.Create(
                                        sprintf "%s cancels a flight with a %s at %s" state.PilotData.FullName planeName afId.AirfieldName,
                                        timeStamp,
                                        AddPlane(afId, state.FlightRecord.Plane, state.FlightRecord.PlaneHealth))
                                ]
                            | _ ->
                                []
                        let state = 
                            { state with
                                MissionStatus = Ended
                            }
                        let recordFlight =
                            match state.MissionStatus with
                            | Spawned _ ->
                                []
                            | _ ->
                                let cmd =
                                    AnnotatedCommand.Create(sprintf "End of mission for %s" state.PilotData.FullName, timeStamp, RegisterPilotFlight(state.PilotData.Id, state.FlightRecord, state.PilotData.Health))
                                        .SetAudience(LiveMessageAudience.All)
                                [cmd]
                        state, addPlane @ recordFlight

                | _ ->
                    state, []

            member this.HandlePostEvent(state, (state2, preCmds), (mappings, healths, war)) =
                state2, preCmds
        }

/// Tracks all ongoing player flights
type PlayerFlights =
    {
        FlightOfPilot : Map<int, PlayerFlightTracker>
    }
with
    static member Empty = { FlightOfPilot = Map.empty }

    static member Controller =
        let logger = NLog.LogManager.GetCurrentClassLogger()
        let pftCtrl = PlayerFlightTracker.Controller
        { new StateMachineController<PlayerFlights, PlayerFlights * AnnotatedCommand list, Mappings * HealthTracker * IWarStateQuery> with
            member this.HandlePreEvent(state, event, (mappings, healths, war)) =
                match event with
                | PlayerEvent(timeStamp, PlayerTakesObject taken) ->
                    logger.Debug(sprintf "Preparing player flight after %s" event)
                    // Emit RemovePlane command
                    let cmds =
                        match mappings.Bindings.TryGetValue(taken.VehicleId) with
                        | true, binding ->
                            match war.TryGetPlane(binding.Typ) with
                            | Some plane ->
                                match war.TryGetNearestAirfield(taken.Position, None) with
                                | Some (airfield, _) ->
                                    [AnnotatedCommand.Create(sprintf "Plane %s taken by player" plane.Name, timeStamp, RemovePlane(airfield.AirfieldId, plane.Id, 1.0f))]
                                | None ->
                                    []
                            | None ->
                                []
                        | false, _ ->
                            []

                    // Emit UpdatePlayer command
                    let cmds = cmds @ [AnnotatedCommand.Create("Seen player " + taken.Name, timeStamp, UpdatePlayer(taken.UserId, taken.Name))]

                    let state2, cmds =
                        // Choose pilot
                        match war.TryGetNearestAirfield(taken.Position, None), CountryId.FromMcuValue(enum taken.Country), war.TryGetPlane(taken.Typ) with
                        | Some(airfield, _), Some country, Some plane ->
                            let pilot = war.GetPlayerPilotFrom(taken.UserId, airfield.AirfieldId, country, taken.HasFemaleCrew)
                            let player = taken.Name
                            let rank =
                                Pilots.tryComputeRank war.World.Ranks pilot
                                |> Option.map (fun rank -> rank.RankAbbrev)
                                |> Option.defaultValue ""
                            let msg = sprintf "%s controls %s %s" player rank pilot.FullName
                            let msg = AnnotatedCommand.Create(msg, timeStamp).SetAudience(LiveMessageAudience.All)
                            let flight = PlayerFlightTracker.Create(taken.PilotId, taken.VehicleId, airfield.AirfieldId, war.Date + timeStamp, pilot, plane.Id)
                            let flight, flightCmds = pftCtrl.HandlePreEvent(flight, event, (mappings, healths, war))
                            { state with
                                FlightOfPilot = state.FlightOfPilot.Add(taken.PilotId, flight)
                            }, msg :: cmds @ flightCmds
                        | _ ->
                            state, cmds

                    state2, cmds

                | PlayerEvent(_, PlayerEndsMission missionEnded) ->
                    logger.Debug(sprintf "Preparing to end player flight after %s" event)
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

            member this.HandlePostEvent(state, (state2, preCmds), context) =
                let oldKeys = Set(state.FlightOfPilot.Keys)
                let newKeys = Set(state2.FlightOfPilot.Keys)
                let shared = Set.intersect oldKeys newKeys
                let added = newKeys - oldKeys
                let removed = oldKeys - newKeys

                // Remove
                let flights =
                    (state.FlightOfPilot, removed)
                    ||> Set.fold (fun flights key -> flights.Remove(key))

                // Dispatch and update
                let flights, postCmds =
                    ((flights, []), shared)
                    ||> Set.fold (fun (flights, cmds) key ->
                        let before = flights.[key]
                        let after = state2.FlightOfPilot.[key]
                        let updated, cmds2 = pftCtrl.HandlePostEvent(before, (after, []), context)
                        flights.Add(key, updated), cmds @ cmds2)

                // Add
                let flights =
                    (flights, added)
                    ||> Set.fold (fun flights key -> flights.Add(key, state2.FlightOfPilot.[key]))

                // Add post-generated commands to the commands from the Pre handler
                let cmds2 = preCmds @ postCmds

                // Result
                { state2 with
                    FlightOfPilot = flights
                }, cmds2
        }


