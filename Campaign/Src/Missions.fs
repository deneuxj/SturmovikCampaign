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

/// Missions are simulated events whose outcome changes the state of war.
/// Success rate is affected by player actions.
namespace Campaign.Missions

open System
open System.Numerics
open VectorExtension

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Util


type TargetType =
    | Truck | Train | Ship | Battleship | GunBoat | Artillery | Tank | ArmoredCar
    | Bridge of BuildingInstanceId * int
    | Building of BuildingInstanceId * int
    | ParkedPlane of AirfieldId * PlaneModelId
    | Air of PlaneModelId
with
    member this.GroundForceValue =
        match this with
        | Battleship -> 100.0f<MGF>
        | GunBoat -> 15.0f<MGF>
        | Artillery -> 10.0f<MGF>
        | Tank -> 25.0f<MGF>
        | ArmoredCar -> 5.0f<MGF>
        | _ -> 0.0f<MGF>

module TargetType =
    let (|GroundForceTarget|_|) (kind : TargetType) =
        let value = kind.GroundForceValue
        if value > 0.0f<MGF> then
            Some value
        else None

type Target =
    {
        Kind : TargetType
        Owner : CoalitionId option
        Pos : Vector2
        Altitude : float32<M>
    }

type AmmoType =
    Rocket | Bullets | Bomb

type ReturnType =
    CrashedInEnemyTerritory | CrashedInFriendlyTerritory | AtAirfield of AirfieldId

/// The results of a flight by a player, used to build success rates of missions.
type FlightRecord =
    {
        Date : DateTime
        Plane : PlaneModel
        Start : AirfieldId
        TargetsDestroyed : (Target * AmmoType) list
        Return : ReturnType
    }

type AltitudeLevel = LowAltitude | MediumAltitude | HighAltitude
with
    member this.Roof =
        match this with
        | LowAltitude -> 1500.0f
        | MediumAltitude -> 3000.0f
        | HighAltitude -> System.Single.PositiveInfinity

    member this.Ground =
        match this with
        | LowAltitude -> System.Single.NegativeInfinity
        | MediumAltitude -> LowAltitude.Roof
        | HighAltitude -> MediumAltitude.Roof

/// Domains of combat affected by experience bonuses
type ExperienceDomain =
    | AirSupremacy // Fighter attacks on fighters
    | Interception of AltitudeLevel // Fighter and ground attackers on bombers and ground attackers
    | Defense // Gunners on fighters
    | GroundAttack of PlaneType // Any plane on ground targets using gun and rockets
    | Bombing of PlaneType // Any plane on ground targets using bombs

/// Experience bonuses granted by successful flight records
type ExperienceBonus =
    {
        Start : AirfieldId
        Region : RegionId
        Domain : ExperienceDomain
        Bonus : float32
    }
with
    member this.Key =
        this.Start, this.Region, this.Domain

/// Mapping from start airfields, objective regions and experience domains to bonus values
type ExperienceBonuses =
    {
        Bonuses : Map<AirfieldId * RegionId * ExperienceDomain, float32>
    }
with
    member this.GetBonus(key) =
        this.Bonuses.TryFind(key)
        |> Option.defaultValue 0.0f

    member this.Update(bonus : ExperienceBonus) =
        let oldValue =
            this.GetBonus(bonus.Key)
        { this with
            Bonuses = this.Bonuses.Add(bonus.Key, oldValue + bonus.Bonus) }

/// Kind of targets on the ground
type GroundTargetType =
    | GroundForces of CoalitionId
    | BridgeTarget
    | BuildingTarget // Factories and other buildings inside regions but outside airfields
    | AirfieldTarget of AirfieldId // Hangars, fuel tanks, parked planes... on an airfield
with
    member this.Description =
        match this with
        | GroundForces coalition -> sprintf "ground forces of %s" (string coalition)
        | BridgeTarget -> "bridges"
        | BuildingTarget -> "buildings"
        | AirfieldTarget afId -> sprintf "%s airbase" afId.AirfieldName

type AirMissionType =
    | AreaProtection
    | GroundTargetAttack of GroundTargetType * AltitudeLevel
    | PlaneTransfer of Destination: AirfieldId
with
    member this.Description =
        match this with
        | AreaProtection -> "CAP"
        | GroundTargetAttack(target, HighAltitude) -> sprintf "high-altitude bombing of %s" target.Description
        | GroundTargetAttack(target, MediumAltitude) -> sprintf "dive bombing of %s" target.Description
        | GroundTargetAttack(target, LowAltitude) -> sprintf "strafing of %s" target.Description
        | PlaneTransfer(afId) -> sprintf "plane transfer to %s" afId.AirfieldName

type AirMission =
    {
        StartAirfield : AirfieldId
        Objective : RegionId
        MissionType : AirMissionType
        NumPlanes : int
        Plane : PlaneModelId
    }

type GroundMissionType =
    | GroundForcesTransfer of Coalition: CoalitionId * StartRegion: RegionId * Forces: float32<MGF>
    | GroundBattle of Initiator: CoalitionId

type GroundMission =
    {
        Objective : RegionId
        MissionType : GroundMissionType
    }

type MissionType =
    | AirMission of AirMission
    | GroundMission of GroundMission

type Mission =
    {
        Kind : MissionType
        Description : string
    }

type MissionSimulator(random : System.Random, war : WarState, missions : Mission list, duration : float32<H>) =
    let missions =
        List.indexed missions

    let airMissions =
        missions
        |> List.choose (
            function
            | i, { Kind = AirMission mission } -> Some (i, mission)
            | _, { Kind = GroundMission _ } -> None)

    let groundMissions =
        missions
        |> List.choose (
            function
            | _, { Kind = AirMission _ } -> None
            | i, { Kind = GroundMission mission } -> Some (i, mission))

    let numPlanes =
        airMissions
        |> List.map (fun (i, mission) -> i, float32 mission.NumPlanes)
        |> Seq.mutableDict

    let getFighterAttackRate() =
        random.NextDouble() * 0.2 + 0.4
        |> float32

    let getBomberDefenseRate() =
        random.NextDouble() * 0.2 + 0.0
        |> float32

    let getGroundForcesHitRate() =
        random.NextDouble() * 0.01 + 0.05
        |> float32

    member this.DoTakeOffs() =
        seq {
            for mId, mission in airMissions do
                let plane = war.World.PlaneSet.[mission.Plane].Name
                let numPlanesTookOff = 
                    min
                        numPlanes.[mId]
                        (war.GetNumPlanes(mission.StartAirfield, mission.Plane))
                assert(numPlanesTookOff >= 0.0f)
                numPlanes.[mId] <- numPlanesTookOff
                yield
                    Some(RemovePlane(mission.StartAirfield, mission.Plane, numPlanesTookOff)),
                    sprintf "%d %s take off from %s for %s" (int numPlanesTookOff) plane mission.StartAirfield.AirfieldName mission.MissionType.Description
        }

    member this.DoGroundForcesMovements() =
        seq {
            for mId, mission in groundMissions do
                match mission.MissionType with
                | GroundForcesTransfer(coalition, startRegion, forces) ->
                    let coalitionStart = war.GetOwner(startRegion)
                    let coalitionDestination = war.GetOwner(mission.Objective)
                    match coalitionStart with
                    | None ->
                        // Should not happen
                        ()
                    | Some coalitionStart ->
                        let verb =
                            if coalition = coalitionStart then
                                if Some coalitionStart <> coalitionDestination then
                                    "invade"
                                else
                                    "travel to"
                            else
                                if Some coalition = coalitionDestination then
                                    "retreat back to"
                                else
                                    "roam into"
                        // Max transport capacity by roads and rails.
                        let maxFlow =
                            if Some coalitionStart <> coalitionDestination then
                                war.ComputeRoadCapacity(startRegion, mission.Objective)
                            else
                                war.ComputeRoadCapacity(startRegion, mission.Objective) +
                                war.ComputeRailCapacity(startRegion, mission.Objective)
                        let desired = forces * war.World.GroundForcesTransportCost
                        let volume = min desired maxFlow
                        // Transport capacity usage info
                        if desired > maxFlow then
                            yield None,
                                sprintf "Transport from %s to %s is limited by transport capacity to %2.0f%%"
                                    (string startRegion)
                                    (string mission.Objective)
                                    (100.0f * maxFlow / desired)
                        else
                            yield None,
                                sprintf "Transport from %s to %s uses %2.0f%% of the transport capacity"
                                    (string startRegion)
                                    (string mission.Objective)
                                    (100.0f * desired / maxFlow)
                        // Displacement of forces
                        let forces = volume / war.World.GroundForcesTransportCost
                        yield
                            Some(MoveGroundForces(startRegion, mission.Objective, coalition, forces)),
                            sprintf "%0.0f worth of ground forces %s %s from %s"
                                forces
                                verb
                                (string mission.Objective)
                                (string startRegion)
                | GroundBattle _ ->
                    ()
        }

    member this.DoBattles() =
        let roundDuration = 1.0f<H> / 3.0f
        let numBattleRounds = int <| round(duration / roundDuration)

        // Get the max amount of supplies forces have available in a region for a round of battle
        // For the defenders, it's the industry in the rear regions, limited by the road and rail transport capacity
        // Same for the attackers, but with the additional limitation that only roads can be used into the attacked region.
        let getRoundSupplies(region, coalition) = 0.0f<M^3> // TODO

        // Get the efficiency factor that influences damage inflicted to the other sound in one round.
        // Depends on the available supplies
        let getEfficiency(forces, supplies) =
            if forces > 0.0f<MGF> then
                let needed = forces * war.World.GroundForcesCost * roundDuration
                supplies / needed
                |> max 0.5f
                |> min 1.0f
            else
                0.0f

        seq {
            let battles =
                groundMissions
                |> Seq.choose (
                    function
                    | _, { Objective = rid; MissionType = GroundBattle initiator } -> Some (rid, initiator)
                    | _ -> None)
            for rid, initiator in battles do
                let defenders = war.GetOwner(rid)
                match defenders with
                | Some defenders ->
                    let attackers = defenders.Other
                    let defendersSupplies = getRoundSupplies(rid, defenders)
                    let attackersSupplies = getRoundSupplies(rid, attackers)
                    yield None,
                        sprintf "%s (forces: %0.0f, supplies: %0.0f) attack %s (forces: %0.0f, supplies: %0.0f) in %s"
                            (string attackers)
                            (war.GetGroundForces(attackers, rid))
                            attackersSupplies
                            (string defenders)
                            (war.GetGroundForces(defenders, rid))
                            defendersSupplies
                            (string rid)
                    let rec work iterLeft =
                        seq {
                            let defenseForces = war.GetGroundForces(defenders, rid)
                            let attackForces = war.GetGroundForces(attackers, rid)
                            if iterLeft > 0 && defenseForces > 0.0f<MGF> && attackForces > 0.0f<MGF> then
                                let defenseEfficiency = 1.0f //getEfficiency(defenseForces, defendersSupplies)
                                let attackEfficiency = 1.0f //getEfficiency(attackForces, attackersSupplies)
                                let defenseLosses =
                                    attackForces * getGroundForcesHitRate() * attackEfficiency
                                    |> min defenseForces
                                let attackLosses =
                                    defenseForces * getGroundForcesHitRate() * defenseEfficiency
                                    |> min attackForces
                                yield
                                    Some(DestroyGroundForces(rid, defenders, defenseLosses)),
                                    sprintf "Defense of %s sustained %0.0f worth of damage"
                                        (string rid)
                                        defenseLosses
                                yield
                                    Some(DestroyGroundForces(rid, attackers, attackLosses)),
                                    sprintf "Attackers of %s sustained %0.0f worth of damage"
                                        (string rid)
                                        attackLosses
                                yield! work (iterLeft - 1)
                        }
                    yield! work numBattleRounds
                    // If the side that initiated the battle has a significant numerical advantage, the other side surrenders
                    let initiators, pursued =
                        war.GetGroundForces(initiator, rid), war.GetGroundForces(initiator.Other, rid)
                    if initiators > 2.0f * pursued then
                        yield None,
                            sprintf "%s victorious in %s" (string initiator) (string rid)
                        yield Some(DestroyGroundForces(rid, initiator.Other, pursued)),
                            sprintf "%0.0f forces from %s surrendered in %s" pursued (string initiator.Other) (string rid)
                        if defenders <> initiator then
                            yield Some(SetRegionOwner(rid, Some initiator)),
                                sprintf "%s took over %s" (string initiator) (string rid)
                | None ->
                    ()
        }

    member this.DoInterceptions() =
        let numInterceptorPasses = 3
        seq {
            for mId, mission in airMissions do
                let targetCoalition =
                    war.GetOwner(war.World.Airfields.[mission.StartAirfield].Region)
                let interceptors =
                    airMissions
                    // Different coalition
                    |> Seq.filter (fun (_, mission) ->
                        let intercepterCoalition =
                            war.GetOwner(war.World.Airfields.[mission.StartAirfield].Region)
                        targetCoalition <> intercepterCoalition)
                    // Same objective
                    |> Seq.filter (fun (_, mission) -> mission.Objective = mission.Objective)
                    // Is area protection
                    |> Seq.filter (function _, { MissionType = AreaProtection _ } -> true | _ -> false)
                for interId, interception in interceptors do
                    let interceptorsName = war.World.PlaneSet.[interception.Plane].Name
                    let interceptedName = war.World.PlaneSet.[mission.Plane].Name
                    for pass in 1..numInterceptorPasses do
                        let interceptorKillRate = getFighterAttackRate()
                        let defenderKillRate =
                            match war.World.PlaneSet.[mission.Plane].Kind with
                            | PlaneType.Fighter ->
                                let loadoutKillRateModifier =
                                    match mission.MissionType with
                                    | AreaProtection _ -> 1.0f
                                    | _ -> 0.5f
                                loadoutKillRateModifier * getFighterAttackRate()
                            | _ ->
                                getBomberDefenseRate()
                        let numInterceptors = numPlanes.[interId]
                        let numIntercepted = numPlanes.[mId]
                        let numInterceptorsShotDown =
                            numIntercepted * defenderKillRate
                            |> min numInterceptors
                        let numInterceptedShotDown =
                            numInterceptors * interceptorKillRate
                            |> min numIntercepted
                        let numInterceptors2 = numInterceptors - numInterceptorsShotDown
                        let numIntercepted2 = numIntercepted - numInterceptedShotDown
                        numPlanes.[interId] <- numInterceptors2
                        numPlanes.[mId] <- numIntercepted2
                        assert(numPlanes |> Seq.forall (fun kvp -> kvp.Value >= 0.0f))
                        if numInterceptorsShotDown > 0.0f then
                            yield
                                None,
                                sprintf "%0.1f %s were shot down or damaged by %s over %s"
                                    numInterceptorsShotDown
                                    interceptorsName
                                    interceptedName
                                    (string interception.Objective)
                        if numInterceptedShotDown > 0.0f then
                            yield
                                None,
                                sprintf "%0.1f %s were shot down or damaged by %s over %s"
                                    numInterceptedShotDown
                                    interceptedName
                                    interceptorsName
                                    (string interception.Objective)
        }

    member this.DoObjectives() =
        seq {
            for mId, mission in airMissions do
                let numPlanes = int <| numPlanes.[mId]
                assert(numPlanes >= 0)
                match mission.MissionType with
                | AreaProtection ->
                    // Effect of area protection already handled during interception phase
                    ()
                | GroundTargetAttack(targetType, _) ->
                    let region = war.World.Regions.[mission.Objective]
                    let plane = war.World.PlaneSet.[mission.Plane].Name
                    let numBridgePartsDamaged, volumeBuildingDamaged =
                        let planeBombs =
                            war.World.PlaneSet.[mission.Plane].BombCapacity
                        if planeBombs > 0.0f<K> then
                            planeBombs / 100.0f<K> |> int |> max 1,
                            planeBombs * 0.1f<M^3/K>
                        else
                            3, 5.0f<M^3>
                    let mkMessage (target : Target) =
                        let subject =
                            match target.Kind with
                            | TargetType.Bridge(bid, _) ->
                                let building = war.World.Bridges.[bid]
                                sprintf "Bridge %s" building.Properties.Model
                            | TargetType.Building(bid, _) ->
                                let building = war.World.Buildings.[bid]
                                sprintf "Building %s" building.Properties.Model
                            | TargetType.ParkedPlane(afId, parked) ->
                                sprintf "Parked plane %s at %s" (string parked) afId.AirfieldName
                            | Truck -> "Truck"
                            | Train -> "Train"
                            | Ship -> "Ship"
                            | Battleship -> "Battleship"
                            | GunBoat -> "Gun boat"
                            | Artillery -> "Piece of artillery"
                            | Tank -> "Tank"
                            | ArmoredCar -> "Armored car"
                            | Air plane ->
                                sprintf "In-flight plane %s" (string plane)
                        sprintf "%s hit by %s from %s in %s at %0.0f, %0.0f"
                            subject
                            plane
                            mission.StartAirfield.AirfieldName
                            (string mission.Objective)
                            target.Pos.X
                            target.Pos.Y

                    let targets =
                        // Pick parts of a building at random, biased towards undamaged parts
                        let getParts owner (building : BuildingInstance) =
                            building.Properties.SubParts
                            |> List.sortByDescending (fun part -> war.GetBuildingPartHealthLevel(building.Id, part))
                            |> Seq.filter (fun part -> war.GetBuildingPartHealthLevel(building.Id, part) > 0.0f)
                            |> Seq.map (fun part ->
                                {
                                    Kind = TargetType.Building(building.Id, part)
                                    Owner = owner
                                    Pos = building.Pos.Pos
                                    Altitude = 0.0f<M>
                                })

                        let getBuildingParts owner building =
                            let parts = getParts owner building
                            let partVolume = building.Properties.PartCapacity
                            let numParts = volumeBuildingDamaged / partVolume |> int |> max 1
                            assert(numParts >= 0)
                            Seq.truncate numParts parts

                        let getBridgeParts owner bridge =
                            assert(numBridgePartsDamaged >= 0)
                            getParts owner bridge
                            |> Seq.truncate numBridgePartsDamaged

                        match targetType with
                        | GroundForces owner ->
                            let kinds =
                                [| TargetType.ArmoredCar; TargetType.Artillery; TargetType.Tank |]
                            let forces = war.GetGroundForces(owner, mission.Objective)
                            let targets =
                                Seq.unfold (fun forces ->
                                    if forces < 0.0f<MGF> then
                                        None
                                    else
                                        let kind = kinds.[random.Next(kinds.Length)]
                                        Some(kind, forces - kind.GroundForceValue)) forces
                                |> Seq.cache
                                |> Seq.map (fun kind ->
                                    {
                                        Kind = kind
                                        Owner = Some owner
                                        Pos = Vector2.Zero
                                        Altitude = 0.0f<M>
                                    }
                                )
                            targets

                        | BridgeTarget ->
                            let owner = war.GetOwner(region.RegionId)
                            let targets =
                                war.World.Roads.Links @ war.World.Rails.Links
                                |> List.collect (fun link -> link.Bridges)
                                |> List.filter (fun bid -> war.World.Bridges.[bid].Pos.Pos.IsInConvexPolygon region.Boundary)
                                |> List.sortByDescending (war.GetBridgeFunctionalityLevel)
                                |> Seq.collect (fun bid ->
                                    let bridge = war.World.Bridges.[bid]
                                    getBridgeParts owner bridge)
                            targets

                        | BuildingTarget ->
                            let owner = war.GetOwner(region.RegionId)
                            let targets =
                                region.IndustryBuildings
                                |> List.sortByDescending (war.GetBuildingFunctionalityLevel)
                                |> Seq.collect (fun bid ->
                                    let building = war.World.Buildings.[bid]
                                    getBuildingParts owner building)
                            targets

                        | AirfieldTarget af ->
                            let owner = war.GetOwner(region.RegionId)
                            let buildingTargets =
                                war.World.Airfields.[af].Facilities
                                |> List.ofSeq
                                |> List.sortByDescending (war.GetBuildingFunctionalityLevel)
                                |> Seq.collect (fun bid ->
                                    let building = war.World.Buildings.[bid]
                                    getBuildingParts owner building)

                            let parkedPlanes =
                                war.GetNumPlanes(af)
                                |> Map.toSeq
                                |> Seq.collect (fun (planeId, qty) ->
                                    assert(qty >= 0.0f)
                                    Seq.init (int qty) (fun _ -> planeId))
                                |> Seq.map (fun planeId ->
                                    {
                                        Kind = TargetType.ParkedPlane(af, planeId)
                                        Owner = owner
                                        Pos = Vector2.Zero
                                        Altitude = 0.0f<M>
                                    })

                            Seq.selectFrom2 (fun _ _ -> random.Next(2) = 1) buildingTargets parkedPlanes
                            |> Seq.cache
                            |> Seq.map (function Choice1Of2 x | Choice2Of2 x -> x)

                    for target in targets |> Seq.truncate numPlanes do
                        let command =
                            match target with
                            | { Kind = TargetType.Building(bid, part) } ->
                                Some(DamageBuildingPart(bid, part, 1.0f))
                            | { Kind = TargetType.ParkedPlane(afId, planeId) } ->
                                Some(RemovePlane(afId, planeId, 1.0f))
                            | { Kind = TargetType.GroundForceTarget value; Owner = Some owner } ->
                                Some(DestroyGroundForces(mission.Objective, owner, value))
                            | _ ->
                                None
                        yield
                            command,
                            mkMessage target

                | PlaneTransfer afid ->
                    let plane = war.World.PlaneSet.[mission.Plane].Name
                    yield
                        Some(AddPlane(afid, mission.Plane, float32 numPlanes)),
                        sprintf "%d %s transfered to %s from %s" numPlanes plane afid.AirfieldName mission.StartAirfield.AirfieldName
        }

    member this.DoReturnToBase() =
        seq {
            for mId, mission in airMissions do
                let numPlanes = numPlanes.[mId] |> int |> max 0
                match mission.MissionType with
                | AreaProtection | GroundTargetAttack _ ->
                    let plane = war.World.PlaneSet.[mission.Plane].Name
                    let afid = mission.StartAirfield
                    yield
                        Some(AddPlane(afid, mission.Plane, float32 numPlanes)),
                        sprintf "%d %s landed back at %s after %s" numPlanes plane afid.AirfieldName mission.MissionType.Description
                | PlaneTransfer _ ->
                    // Transfered planes do not return to start base
                    ()
        }

    member this.DoAll() =
        seq {
            yield! this.DoGroundForcesMovements()
            yield! this.DoTakeOffs()
            yield! this.DoInterceptions()
            yield! this.DoObjectives()
            yield! this.DoBattles()
            yield! this.DoReturnToBase()
        }