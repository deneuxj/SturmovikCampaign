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
    | AirfieldTarget // Hangars, fuel tanks, parked planes... on an airfield

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
        Model : PlaneModelId
    }

type GroundMissionType =
    | GroundForcesTransfer
    | GroundBattle

type GroundMission =
    {
        StartRegion : RegionId
        Objective : RegionId
        MissionType : GroundMissionType
        Forces : float32<MGF>
    }

type Mission =
    | AirMission of AirMission
    | GroundMission of GroundMission

type MissionSimulator(random : System.Random, war : WarState, missions : Mission list) =
    let airMissions =
        missions
        |> List.choose (
            function
            | AirMission mission -> Some mission
            | GroundMission _ -> None)

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

    member this.DoTakeOffs() =
        seq {
            for mission in airMissions do
                let plane = war.World.PlaneSet.[mission.Model].Name
                let numPlanes = numPlanes.[mission]
                yield
                    Some(RemovePlane(mission.StartAirfield, mission.Model, float32 numPlanes)),
                    sprintf "%d %s take off from %s" (int numPlanes) plane mission.StartAirfield.AirfieldName
        }

    member this.DoInterceptions() =
        seq {
            for targets in airMissions do
                let targetCoalition =
                    war.GetOwner(war.World.Airfields.[targets.StartAirfield].Region)
                let threats =
                    airMissions
                    // Different coalition
                    |> Seq.filter (fun mission ->
                        let intercepterCoalition =
                            war.GetOwner(war.World.Airfields.[mission.StartAirfield].Region)
                        targetCoalition <> intercepterCoalition)
                    // Same objective
                    |> Seq.filter (fun mission -> mission.Objective = targets.Objective)
                    // Is area protection
                    |> Seq.filter (function { MissionType = AreaProtection _ } -> true | _ -> false)
                for intercepters in threats do
                    for pass in 1..3 do
                        let interceptorRate = getFighterAttackRate()
                        let defenseRate =
                            match targets.MissionType with
                            | AreaProtection _ -> getFighterAttackRate()
                            | _ -> getBomberDefenseRate()
                        let numInterceptors = numPlanes.[intercepters]
                        let numIntercepted = numPlanes.[targets]
                        let numInterceptors2 =
                            numInterceptors - numIntercepted * defenseRate
                            |> max 0.0f
                        let numIntercepted2 =
                            numIntercepted - numInterceptors * interceptorRate
                        numPlanes.[intercepters] <- numInterceptors2
                        numPlanes.[targets] <- numIntercepted2
                    yield
                        None,
                        sprintf "%d %s from %s survive an encounter with the enemy over %s"
                            (int <| numPlanes.[intercepters])
                            (war.World.PlaneSet.[intercepters.Model].Name)
                            (intercepters.StartAirfield.AirfieldName)
                            (string intercepters.Objective)
                    yield
                        None,
                        sprintf "%d %s from %s survive an encounter with the enemy over %s"
                            (int <| numPlanes.[targets])
                            (war.World.PlaneSet.[targets.Model].Name)
                            (targets.StartAirfield.AirfieldName)
                            (string targets.Objective)
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
                    let mkMessage, targets =
                        match targetType with
                        | BridgeTarget ->
                            sprintf "Bridge destroyed in %s at %0.0f, %0.0f" (string mission.Objective),
                            war.World.Roads.Links @ war.World.Rails.Links
                            |> List.collect (fun link -> link.Bridges)
                            |> List.filter (fun bid -> war.World.Bridges.[bid].Pos.Pos.IsInConvexPolygon region.Boundary)
                            |> List.sortByDescending (war.GetBridgeFunctionalityLevel)
                            |> List.map (fun bid -> bid, war.World.Bridges.[bid])
                        | BuildingTarget ->
                            sprintf "Building destroyed in %s at %0.0f, %0.0f" (string mission.Objective),
                            region.IndustryBuildings
                            |> List.sortByDescending (war.GetBuildingFunctionalityLevel)
                            |> List.map (fun bid -> bid, war.World.Buildings.[bid])
                        | AirfieldTarget ->
                            sprintf "Airfield building destroyed in %s at %0.0f, %0.0f" (string mission.Objective),
                            war.World.Airfields.Values
                            |> Seq.filter (fun af -> af.Region = mission.Objective)
                            |> Seq.collect (fun af -> af.Facilities)
                            |> List.ofSeq
                            |> List.sortByDescending (war.GetBuildingFunctionalityLevel)
                            |> List.map (fun bid -> bid, war.World.Buildings.[bid])
                    for (bid, building) in targets |> Seq.truncate numPlanes do
                        for part in building.Properties.SubParts do
                            yield
                                Some(DamageBuildingPart(bid, part, 1.0f)),
                                mkMessage building.Pos.Pos.X building.Pos.Pos.Y
                | PlaneTransfer afid ->
                    let plane = war.World.PlaneSet.[mission.Model].Name
                    yield
                        Some(AddPlane(afid, mission.Model, float32 numPlanes)),
                        sprintf "%d %s transfered to %s" numPlanes plane afid.AirfieldName
        }

    member this.DoReturnToBase() =
        seq {
            for mission in airMissions do
                let numPlanes = int <| numPlanes.[mission]
                match mission.MissionType with
                | AreaProtection | GroundTargetAttack _ ->
                    let plane = war.World.PlaneSet.[mission.Model].Name
                    let afid = mission.StartAirfield
                    yield
                        Some(AddPlane(afid, mission.Model, float32 numPlanes)),
                        sprintf "%d %s landed back at %s" numPlanes plane afid.AirfieldName
                | PlaneTransfer _ ->
                    // Transfered planes do not return to start base
                    ()
        }

    member this.DoAll() =
        seq {
            yield! this.DoTakeOffs()
            yield! this.DoInterceptions()
            yield! this.DoObjectives()
            yield! this.DoReturnToBase()
        }