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

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription
open Campaign.NewWorldDescription
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
    | AirfieldResupply
    | BattleResupply

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