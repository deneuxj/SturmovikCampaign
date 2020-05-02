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

/// Creation of game mission files
module Campaign.MissionFileGeneration

open BasicTypes
open PlaneModel
open PlaneSet
open AiPlanes
open Campaign.WarState
open Campaign.MissionSelection

open System.Numerics
open Campaign.Missions
open WorldDescription

type GameType =
    | Coop
    | SinglePlayer
    | MultiPlayer

type PlayerSpawnType =
    | Airborne
    | Runway
    | Parking of WarmedUp: bool

type PlayerSpawnPlane =
    {
        Model : PlaneModel
        Mods : ModRange list
    }

type WaypointAction =
    | TakeOff
    | Land
    | Fly
    | AttackAir of Radius: float32<M> * Duration: float32<H>
    | AttackGround of Radius: float32<M>

type Waypoint =
    { Pos : OrientedPosition
      Action : WaypointAction
    }

type PlayerDirectedFlight =
    {
        Title : string
        Flight : string
        Rank : int
        Waypoints : Waypoint list
    }

type PlayerFlight =
    | Unconstrained of PlayerSpawnPlane list
    | Directed of PlayerDirectedFlight

type PlayerSpawn =
    {
        SpawnType : PlayerSpawnType
        Pos : OrientedPosition
        Flight : PlayerFlight
    }

type GroundBattleNumbers =
    {
        NumRocketArtillery : int
        NumArtillery : int
        NumAntiTankGuns : int
        NumTanks : int
    }

type GroundBattle =
    {
        Boundary : Vector2 list
        Pos : OrientedPosition
        Defending : CountryId
        Attacking : CountryId
        NumDefending : GroundBattleNumbers
        NumAttacking : GroundBattleNumbers
    }

type ConvoyMember =
    | Train
    | Truck
    | Tank
    | ArmoredCar
    | AntiAirTruck
    | StaffCar

type Convoy =
    {
        Members : ConvoyMember list
        Start : OrientedPosition
        Destination : OrientedPosition
    }

type MissionContent =
    {
        Boundary : Vector2 list
        GameType : GameType
        PlayerSpawns : PlayerSpawn list
        AntiAirNests : StaticDefenseOptimization.Nest list
        GroundBattles : GroundBattle list
        AiPatrols : AiPatrol list
        AiAttacks : AiAttack list
        ParkedPlanes : Map<AirfieldId, PlaneModelId>
    }

type AiStartPoint =
    | StartAtIngress of float32<M>
    | StartOverAirfield

type AiAttack with
    static member TryFromAirMission(state : WarState, mission : AirMission, getTargetPosition, ?maxFlightSize, ?aiStartPoint) =
        let aiStartPoint = defaultArg aiStartPoint (StartAtIngress 50000.0f<M>)
        let coalition = state.GetOwner(state.World.Airfields.[mission.StartAirfield].Region)
        match mission, coalition with
        | { MissionType = GroundTargetAttack(target, altitude) }, Some coalition ->
            let planeModel = state.World.PlaneSet.[mission.Plane]
            let numPlanes = min (defaultArg maxFlightSize 5) mission.NumPlanes
            let reserve = mission.NumPlanes - numPlanes
            let country = state.World.Countries |> Seq.find (fun kvp -> kvp.Value = coalition) |> fun x -> x.Key
            let targetPos : Vector2 = getTargetPosition(target, mission.Objective)
            let toTarget =
                let v = targetPos - state.World.Airfields.[mission.StartAirfield].Position
                v / v.Length()
            let startPos =
                match aiStartPoint with
                | StartAtIngress dist ->
                    targetPos - (dist / 1.0f<M>) * toTarget
                | StartOverAirfield ->
                    state.World.Airfields.[mission.StartAirfield].Position
            let altitude, roles =
                match altitude with
                | LowAltitude -> 1000.0f, [PlaneRole.GroundAttacker]
                | MediumAltitude -> 2500.0f, [PlaneRole.GroundAttacker; PlaneRole.LevelBomber]
                | HighAltitude -> 4500.0f, [PlaneRole.LevelBomber; PlaneRole.GroundAttacker]
            let role =
                roles
                |> List.tryFind (fun role -> planeModel.Payloads.ContainsKey role)
            role
            |> Option.map (fun role ->
                {
                    Attacker = planeModel
                    NumPlanes = numPlanes
                    AttackerReserve = reserve
                    HomeAirfield = mission.StartAirfield
                    Country = country
                    Start = startPos
                    Target = targetPos
                    Altitude = altitude
                    Landing = None
                    Role = role
                })
        | _ -> None

type AiPatrol with
    static member TryFromAirMission(state : WarState, mission : AirMission, getTargetPosition, ?maxFlightSize) =
        let maxFlightSize = defaultArg maxFlightSize 2
        let coalition = state.GetOwner(state.World.Airfields.[mission.StartAirfield].Region)
        match mission, coalition with
        | { MissionType = AreaProtection }, Some coalition ->
            let planeModel = state.World.PlaneSet.[mission.Plane]
            let numPlanes = min maxFlightSize mission.NumPlanes
            let reserve = mission.NumPlanes - numPlanes
            let country = state.World.Countries |> Seq.find (fun kvp -> kvp.Value = coalition) |> fun x -> x.Key
            let targetPos : Vector2 = getTargetPosition(mission.Objective)
            let roles, protectedRegion =
                if state.GetOwner(mission.Objective) = Some coalition then
                    [PlaneRole.Interceptor; PlaneRole.Patroller], Some mission.Objective
                else
                    [PlaneRole.Patroller; PlaneRole.Interceptor], None
            let role =
                roles
                |> List.tryFind (fun role -> planeModel.Payloads.ContainsKey role)
            role
            |> Option.map (fun role ->
                {
                    Plane = planeModel
                    NumPlanes = numPlanes
                    PlaneReserve = reserve
                    HomeAirfield = mission.StartAirfield
                    Country = country
                    Pos = targetPos
                    Altitude = 3500.0f
                    Role = role
                    ProtectedRegion = protectedRegion
                })
        | _ ->
            None

type GroundBattle with
    static member TryFromGroundMission(state : WarState, mission : GroundMission, getBattlePosition) =
        match mission with
        | { MissionType = GroundBattle initiator } ->
            let pos, boundary = getBattlePosition(initiator, mission.Objective)
            let computeNum (force : float32<MGF>) =
                { NumRocketArtillery = int(0.25f * force / TargetType.ArmoredCar.GroundForceValue)
                  NumArtillery = int(0.25f * force / TargetType.Artillery.GroundForceValue)
                  NumAntiTankGuns = int(0.25f * force / TargetType.Artillery.GroundForceValue)
                  NumTanks = int(0.25f * force / TargetType.Tank.GroundForceValue)
                }
            let getCountryInCoalition coalition = state.World.Countries |> Seq.find (fun kvp -> kvp.Value = coalition) |> fun kvp -> kvp.Key
            Some {
                Boundary = boundary
                Pos = pos
                Defending = getCountryInCoalition initiator.Other
                Attacking = getCountryInCoalition initiator
                NumDefending = computeNum(state.GetGroundForces(initiator.Other, mission.Objective))
                NumAttacking = computeNum(state.GetGroundForces(initiator, mission.Objective))
            }
        | _ ->
            None

let mkMultiplayerMissionContent (state : WarState) (selection : MissionSelection) =
    ()