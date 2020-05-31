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
open Campaign.SpacePartition
open VectorExtension
open Util

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
    static member TryFromAirMission(state : WarState, mission : AirMission, getPatrolPosition, ?maxFlightSize) =
        let maxFlightSize = defaultArg maxFlightSize 2
        let coalition = state.GetOwner(state.World.Airfields.[mission.StartAirfield].Region)
        match mission, coalition with
        | { MissionType = AreaProtection }, Some coalition ->
            let planeModel = state.World.PlaneSet.[mission.Plane]
            let numPlanes = min maxFlightSize mission.NumPlanes
            let reserve = mission.NumPlanes - numPlanes
            let country = state.World.Countries |> Seq.find (fun kvp -> kvp.Value = coalition) |> fun x -> x.Key
            let targetPos : Vector2 = getPatrolPosition(mission.Objective)
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
    static member TryFromGroundMission(state : WarState, mission : GroundMission, pos : OrientedPosition, area : Vector2 list) =
        match mission with
        | { MissionType = GroundBattle initiator } ->
            let computeNum (force : float32<MGF>) =
                { NumRocketArtillery = int(0.25f * force / TargetType.ArmoredCar.GroundForceValue)
                  NumArtillery = int(0.25f * force / TargetType.Artillery.GroundForceValue)
                  NumAntiTankGuns = int(0.25f * force / TargetType.Artillery.GroundForceValue)
                  NumTanks = int(0.25f * force / TargetType.Tank.GroundForceValue)
                }
            let getCountryInCoalition coalition = state.World.Countries |> Seq.find (fun kvp -> kvp.Value = coalition) |> fun kvp -> kvp.Key
            Some {
                Boundary = area
                Pos = pos
                Defending = getCountryInCoalition initiator.Other
                Attacking = getCountryInCoalition initiator
                NumDefending = computeNum(state.GetGroundForces(initiator.Other, mission.Objective))
                NumAttacking = computeNum(state.GetGroundForces(initiator, mission.Objective))
            }
        | _ ->
            None

type TargetLocator(random : System.Random, state : WarState, missions : Mission list) =
    let freeAreas : FreeAreas.FreeAreasNode option =
        let path =
            match state.World.Map.ToLowerInvariant() with
            | "rheinland-summer"
            | _ ->
                "rheinland.bin"
        use freeAreasFile =
            try
                System.IO.File.OpenRead(path)
            with _ -> failwithf "Could not open free areas data file '%s'" path
        let serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
        try
            serializer.Deserialize(freeAreasFile)
        with e -> failwithf "Failed to read free areas data file, error was: %s" e.Message

    let mapExtent =
        match state.World.Map.ToLowerInvariant() with
        | "rheinland-summer"
        | _ ->
            Vector2(30.0e3f, 30.0e3f), 324.0e3f, 400.0e3f

    let getLocationCandidates(region, shape) =
        match freeAreas with
        | Some root ->
            // Transform from mission editor coordinates to free areas coordinates, and the inverse
            let transform, transform' =
                // bin data uses coordinate system where x goes east and y goes north, from 0 to 400000 on both axes.
                let origin, sx, sy = mapExtent
                let t(v : Vector2) =
                    Vector2(400.0e3f * (v.Y - origin.Y) / sy, 400.e3f * (v.X - origin.X) / sx)
                let t'(v : Vector2) =
                    Vector2(origin.X + sx * v.Y / 400.0e3f, origin.Y + sy * v.X / 400.0e3f)
                t, t'
            let region2 = List.map transform region
            let shape2 = List.map transform shape
            let rank _ = 
                random.Next()
            let candidates =
                FreeAreas.findPositionCandidates rank root shape2 region2
                |> Seq.map transform'
                |> Seq.cache
            candidates
        | None ->
            Seq.empty

    let tryGetBattleLocation(regId : RegionId) =
        let region = state.World.Regions.[regId]
        let defender = state.GetOwner(regId).Value
        // A rectangular area 4km x 2km
        let battleShape =
            [
                Vector2(2000.0f, -1000.0f)
                Vector2(-2000.0f, -1000.0f)
                Vector2(-2000.0f, 1000.0f)
                Vector2(2000.0f, 1000.0f)
            ]
        // The areas rotated by increments of 45 degrees
        let battleShapes =
            [
                for rotation in 0.0f .. 45.0f .. 135.0f do
                    yield
                        rotation,
                        battleShape
                        |> List.map (fun v -> v.Rotate rotation)
            ]
        // Find an enemy region, and try to put the battle area between the capital of the defending region and the border
        let enemyRegions =
            region.Neighbours
            |> Seq.choose (fun regB -> state.GetOwner(regB) |> Option.attach regB)
            |> Seq.filter (fun (owner, _) -> owner = defender.Other)
            |> Seq.map (fun (owner, regId) -> owner, state.World.Regions.[regId])
        let frontline =
            enemyRegions
            |> Seq.choose (fun (owner, regB) -> NewWorldDescription.commonBorder(region, regB) |> Option.attach (owner, regB))
            |> Seq.sortByDescending (fun ((p1, p2), _) -> (p1 - p2).LengthSquared())
            |> Seq.tryHead
        let preferredArea =
            match frontline with
            | None -> region.Boundary
            | Some ((p1, p2), (enemy, enemyRegion)) ->
                // The ratio of attacking vs defending forces decides the proximity to the region capital.
                // The stronger the attackers, the closer to the capital they can be
                let axis = region.Position - enemyRegion.Position
                let refK = Vector2.Dot(p1 - enemyRegion.Position, axis) / axis.LengthSquared()
                let attackerForceRatio =
                    state.GetGroundForces(enemy, regId) / state.GetGroundForces(defender, regId)
                let scaleK =
                    if System.Single.IsNaN attackerForceRatio then refK
                    else
                        attackerForceRatio
                        |> min 2.0f
                        |> max 0.5f
                        |> fun x ->
                            let x = (x - 0.5f) / 1.5f
                            1.1f * refK * (1.0f - x) + 1.0f * x
                let scaleK = scaleK / refK
                let scaledBoundaryB =
                    enemyRegion.Boundary
                    |> List.map (fun v -> scaleK * (v - enemyRegion.Position) + enemyRegion.Position)
                intersectConvexPolygons(region.Boundary, scaledBoundaryB)
                |> Option.defaultValue region.Boundary
        let candidates =
            battleShapes
            |> Seq.map (fun shape ->
                getLocationCandidates(preferredArea, snd shape)
                |> Seq.map (fun v -> v, shape))
            |> Seq.interleave
            |> Seq.append (
                // Try again with the full region instead of the preferred area
                battleShapes
                |> Seq.map (fun shape ->
                    getLocationCandidates(region.Boundary, snd shape)
                    |> Seq.map (fun v -> v, shape))
                |> Seq.interleave)
                    
        Seq.tryHead candidates
        |> Option.map (fun (pos, (rotation, shape)) ->
            { OrientedPosition.Pos = pos
              Rotation = rotation
              Altitude = 0.0f },
            shape)

    let battleLocationCache = Seq.mutableDict []

    member this.TryGetBattleLocation =
        SturmovikMission.Cached.cached battleLocationCache tryGetBattleLocation


let mkMultiplayerMissionContent (state : WarState) (selection : MissionSelection) =
    ()