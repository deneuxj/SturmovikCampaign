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
    | Truck | Train | Ship | Battleship | Artillery | Tank | ArmoredCar | Bridge
    | Building of BuildingProperties * int
    | ParkedPlane of PlaneType
    | Air of PlaneType

type Target =
    {
        Kind : TargetType
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
    | BridgeTarget
    | BuildingTarget // Factories and other buildings inside regions but outside airfields
    | AirfieldTarget of AirfieldId // Hangars, fuel tanks, parked planes... on an airfield

type AirMissionType =
    | AreaProtection
    | GroundTargetAttack of GroundTargetType * AltitudeLevel
    | PlaneTransfer of Destination: AirfieldId

type AirMission =
    {
        StartAirfield : AirfieldId
        Objective : RegionId
        MissionType : AirMissionType
        NumPlanes : int
        Plane : PlaneModelId
    }

type GroundMissionType =
    | GroundForcesTransfer of StartRegion : RegionId * Forces : float32<MGF>
    | GroundBattle

type GroundMission =
    {
        Objective : RegionId
        MissionType : GroundMissionType
    }

type Mission =
    | AirMission of AirMission
    | GroundMission of GroundMission

type MissionSimulator(random : System.Random, war : WarState, missions : Mission list, duration : float32<H>) =
    let airMissions =
        missions
        |> List.choose (
            function
            | AirMission mission -> Some mission
            | GroundMission _ -> None)

    let groundMissions =
        missions
        |> List.choose (
            function
            | AirMission _ -> None
            | GroundMission mission -> Some mission)

    let numPlanes =
        airMissions
        |> List.map (fun mission -> mission, float32 mission.NumPlanes)
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
            for mission in airMissions do
                let plane = war.World.PlaneSet.[mission.Plane].Name
                let numPlanes = numPlanes.[mission]
                yield
                    Some(RemovePlane(mission.StartAirfield, mission.Plane, float32 numPlanes)),
                    sprintf "%d %s take off from %s" (int numPlanes) plane mission.StartAirfield.AirfieldName
        }

    member this.DoGroundForcesMovements() =
        seq {
            for mission in groundMissions do
                match mission.MissionType with
                | GroundForcesTransfer(startRegion, forces) ->
                    let coalitionStart = war.GetOwner(startRegion)
                    let coalitionDestination = war.GetOwner(mission.Objective)
                    match coalitionStart with
                    | None ->
                        // Should not happen
                        ()
                    | Some coalitionStart ->
                        let verb =
                            if Some coalitionStart <> coalitionDestination then
                                "invade"
                            else
                                "move into"
                        let maxFlow =
                            if Some coalitionStart <> coalitionDestination then
                                war.ComputeRoadCapacity(startRegion, mission.Objective)
                            else
                                war.ComputeRoadCapacity(startRegion, mission.Objective) +
                                war.ComputeRailCapacity(startRegion, mission.Objective)
                        let volume =
                            forces * war.World.GroundForcesTransportCost
                            |> max maxFlow
                        let forces = volume / war.World.GroundForcesTransportCost
                        yield
                            Some(MoveGroundForces(startRegion, mission.Objective, coalitionStart, forces)),
                            sprintf "%0.0f worth of ground forces %s %s from %s"
                                forces
                                verb
                                (string mission.Objective)
                                (string startRegion)
                | GroundBattle ->
                    ()
        }

    member this.DoBattles() =
        let roundDuration = 1.0f<H> / 3.0f
        let numBattleRounds = int <| round(duration / roundDuration)

        // Get the max amount of supplies forces have available in a region for a round of battle
        // For the defenders, it's the local industry, and the industry of the friendly neighbours, limited by transport capacity
        // Same for the attackers, but without the local industry.
        let getRoundSupplies(region, coalition) =
            let isDefending = war.GetOwner(region) = Some coalition
            let inRegion =
                if isDefending then
                    war.World.Regions.[region].IndustryBuildings
                    |> Seq.sumBy war.GetBuildingCapacity
                else
                    0.0f<M ^ 3>
            let fromNeighbour ngh =
                let isFriendly = war.GetOwner(ngh) = Some coalition
                if isFriendly then
                    let available =
                        war.World.Regions.[ngh].IndustryBuildings
                        |> Seq.sumBy war.GetBuildingCapacity
                    let byRoad =
                        war.ComputeRoadCapacity(ngh, region)
                    let byRail =
                        war.ComputeRailCapacity(ngh, region)
                    let inFlow = (byRoad + byRail) * roundDuration
                    min inFlow available
                else
                    0.0f<M^3>
            let fromNeighbours =
                war.World.Regions.[region].Neighbours
                |> Seq.sumBy fromNeighbour
            inRegion + fromNeighbours

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
                |> Seq.filter (
                    function
                    | { MissionType = GroundBattle } -> true
                    | _ -> false)
            for battle in battles do
                let defenders = war.GetOwner(battle.Objective)
                match defenders with
                | Some defenders ->
                    let attackers = defenders.Other
                    let defendersSupplies = getRoundSupplies(battle.Objective, defenders)
                    let attackersSupplies = getRoundSupplies(battle.Objective, attackers)
                    yield None,
                        sprintf "%s (forces: %0.0f, supplies: %0.0f) attack %s (forces: %0.0f, supplies: %0.0f) in %s"
                            (string attackers)
                            (war.GetGroundForces(attackers, battle.Objective))
                            attackersSupplies
                            (string defenders)
                            (war.GetGroundForces(defenders, battle.Objective))
                            defendersSupplies
                            (string battle.Objective)
                    for round in 1..numBattleRounds do
                        let defenseForces = war.GetGroundForces(defenders, battle.Objective)
                        let defenseEfficiency = getEfficiency(defenseForces, defendersSupplies)
                        let attackForces = war.GetGroundForces(attackers, battle.Objective)
                        let attackEfficiency = getEfficiency(attackForces, attackersSupplies)
                        let defenseLosses =
                            attackForces * getGroundForcesHitRate() * attackEfficiency
                            |> min defenseForces
                        let attackLosses =
                            defenseForces * getGroundForcesHitRate() * defenseEfficiency
                            |> min attackForces
                        yield
                            Some(DestroyGroundForces(battle.Objective, defenders, defenseLosses)),
                            sprintf "Defense of %s sustained %0.0f worth of damage in round %d"
                                (string battle.Objective)
                                defenseLosses
                                round
                        yield
                            Some(DestroyGroundForces(battle.Objective, attackers, attackLosses)),
                            sprintf "Attackers of %s sustained %0.0f worth of damage in round %d"
                                (string battle.Objective)
                                attackLosses
                                round
                | None ->
                    ()
        }

    member this.DoInterceptions() =
        let numInterceptorPasses = 3
        seq {
            for mission in airMissions do
                let targetCoalition =
                    war.GetOwner(war.World.Airfields.[mission.StartAirfield].Region)
                let interceptors =
                    airMissions
                    // Different coalition
                    |> Seq.filter (fun mission ->
                        let intercepterCoalition =
                            war.GetOwner(war.World.Airfields.[mission.StartAirfield].Region)
                        targetCoalition <> intercepterCoalition)
                    // Same objective
                    |> Seq.filter (fun mission -> mission.Objective = mission.Objective)
                    // Is area protection
                    |> Seq.filter (function { MissionType = AreaProtection _ } -> true | _ -> false)
                for interception in interceptors do
                    for pass in 1..numInterceptorPasses do
                        let interceptorKillRate = getFighterAttackRate()
                        let defenderKillRate =
                            match mission.MissionType with
                            | AreaProtection _ -> getFighterAttackRate()
                            | _ -> getBomberDefenseRate()
                        let numInterceptors = numPlanes.[interception]
                        let numIntercepted = numPlanes.[mission]
                        let numInterceptors2 =
                            numInterceptors - numIntercepted * defenderKillRate
                            |> max 0.0f
                        let numIntercepted2 =
                            numIntercepted - numInterceptors * interceptorKillRate
                            |> max 0.0f
                        numPlanes.[interception] <- numInterceptors2
                        numPlanes.[mission] <- numIntercepted2
                    yield
                        None,
                        sprintf "%d %s interceptors from %s survive an encounter with the enemy over %s"
                            (int <| numPlanes.[interception])
                            (war.World.PlaneSet.[interception.Plane].Name)
                            (interception.StartAirfield.AirfieldName)
                            (string interception.Objective)
                    yield
                        None,
                        sprintf "%d %s from %s survive an interception with the enemy over %s"
                            (int <| numPlanes.[mission])
                            (war.World.PlaneSet.[mission.Plane].Name)
                            (mission.StartAirfield.AirfieldName)
                            (string mission.Objective)
        }

    member this.DoObjectives() =
        seq {
            for mission in airMissions do
                let numPlanes = int <| numPlanes.[mission]
                match mission.MissionType with
                | AreaProtection ->
                    // Effect of area protection already handled during interception phase
                    ()
                | GroundTargetAttack(targetType, _) ->
                    let region = war.World.Regions.[mission.Objective]
                    let plane = war.World.PlaneSet.[mission.Plane].Name
                    let mkMessage, targets =
                        match targetType with
                        | BridgeTarget ->
                            sprintf "Bridge destroyed by %s from %s in %s at %0.0f, %0.0f"
                                plane
                                mission.StartAirfield.AirfieldName
                                (string mission.Objective)
                                ,
                            war.World.Roads.Links @ war.World.Rails.Links
                            |> List.collect (fun link -> link.Bridges)
                            |> List.filter (fun bid -> war.World.Bridges.[bid].Pos.Pos.IsInConvexPolygon region.Boundary)
                            |> List.sortByDescending (war.GetBridgeFunctionalityLevel)
                            |> List.map (fun bid -> bid, war.World.Bridges.[bid])
                        | BuildingTarget ->
                            sprintf "Building destroyed by %s from %s in %s at %0.0f, %0.0f"
                                plane
                                mission.StartAirfield.AirfieldName
                                (string mission.Objective)
                                ,
                            region.IndustryBuildings
                            |> List.sortByDescending (war.GetBuildingFunctionalityLevel)
                            |> List.map (fun bid -> bid, war.World.Buildings.[bid])
                        | AirfieldTarget af ->
                            sprintf "Airfield building destroyed by %s from %s in %s at %0.0f, %0.0f"
                                plane
                                mission.StartAirfield.AirfieldName
                                (string mission.Objective),
                            war.World.Airfields.[af].Facilities
                            |> List.ofSeq
                            |> List.sortByDescending (war.GetBuildingFunctionalityLevel)
                            |> List.map (fun bid -> bid, war.World.Buildings.[bid])
                    for (bid, building) in targets |> Seq.truncate numPlanes do
                        for part in building.Properties.SubParts do
                            yield
                                Some(DamageBuildingPart(bid, part, 1.0f)),
                                mkMessage building.Pos.Pos.X building.Pos.Pos.Y
                | PlaneTransfer afid ->
                    let plane = war.World.PlaneSet.[mission.Plane].Name
                    yield
                        Some(AddPlane(afid, mission.Plane, float32 numPlanes)),
                        sprintf "%d %s transfered to %s" numPlanes plane afid.AirfieldName
        }

    member this.DoReturnToBase() =
        seq {
            for mission in airMissions do
                let numPlanes = numPlanes.[mission] |> int |> max 0
                match mission.MissionType with
                | AreaProtection | GroundTargetAttack _ ->
                    let plane = war.World.PlaneSet.[mission.Plane].Name
                    let afid = mission.StartAirfield
                    yield
                        Some(AddPlane(afid, mission.Plane, float32 numPlanes)),
                        sprintf "%d %s landed back at %s" numPlanes plane afid.AirfieldName
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