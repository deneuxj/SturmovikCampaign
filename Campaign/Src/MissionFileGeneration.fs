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
open StaticDefenseOptimization
open Campaign.SpacePartition
open VectorExtension
open Util
open SturmovikMission.Blocks.StaticDefenses.Types
open NewWorldDescription
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Battlefield
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks
open SturmovikMission.Blocks.VirtualConvoy
open SturmovikMission.Blocks.Train

let private logger = NLog.LogManager.GetCurrentClassLogger()

type AiStartPoint =
    | StartAtIngress of float32<M>
    | StartOverAirfield

type AiAttack with
    static member TryFromAirMission(state : WarState, mission : AirMission, targetPos : Vector2, ?maxFlightSize, ?aiStartPoint) =
        let aiStartPoint = defaultArg aiStartPoint (StartAtIngress 50000.0f<M>)
        let coalition = state.GetOwner(state.World.Airfields.[mission.StartAirfield].Region)
        match mission, coalition with
        | { MissionType = GroundTargetAttack(target, altitude) }, Some coalition ->
            let planeModel = state.World.PlaneSet.[mission.Plane]
            let numPlanes = min (defaultArg maxFlightSize 5) mission.NumPlanes
            let reserve = mission.NumPlanes - numPlanes
            let country = state.World.Countries |> Seq.find (fun kvp -> kvp.Value = coalition) |> fun x -> x.Key
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
    static member TryFromAirMission(state : WarState, mission : AirMission, targetPos : Vector2, ?maxFlightSize) =
        let maxFlightSize = defaultArg maxFlightSize 2
        let coalition = state.GetOwner(state.World.Airfields.[mission.StartAirfield].Region)
        match mission, coalition with
        | { MissionType = AreaProtection }, Some coalition ->
            let planeModel = state.World.PlaneSet.[mission.Plane]
            let numPlanes = min maxFlightSize mission.NumPlanes
            let reserve = mission.NumPlanes - numPlanes
            let country = state.World.Countries |> Seq.find (fun kvp -> kvp.Value = coalition) |> fun x -> x.Key
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

/// Compute groups of buildings or bridges in a region that are still 50% functional or more.
/// The result is not a partitition, i.e. each building may appear in more than one groups.
let computeHealthyBuildingClusters(radius, state : IWarStateQuery, buildings : BuildingInstance seq, regId : RegionId) =
    let region = state.World.Regions.[regId]
    // Find the largest cluster of healthy buildings, and use that
    let healthyBuildings =
        buildings
        |> Seq.filter (fun building -> building.Pos.Pos.IsInConvexPolygon region.Boundary)
        |> Seq.filter (fun building -> state.GetBuildingFunctionalityLevel(building.Id) > 0.5f)
    let clusters =
        healthyBuildings
        |> Seq.allPairs healthyBuildings
        |> Seq.filter (fun (b1, b2) -> b1.Id <> b2.Id && (b1.Pos.Pos - b2.Pos.Pos).Length() < radius)
        |> Seq.groupBy fst
        |> Seq.cache
    clusters
    |> Seq.sortByDescending (snd >> Seq.length)
    |> Seq.map (fun (rep, group) -> rep, group |> Seq.map snd)


type TargetLocator(random : System.Random, state : IWarStateQuery) =
    let freeAreas : FreeAreas.FreeAreasNode option =
        let path =
            match state.World.Map.ToLowerInvariant() with
            | "moscow-winter" | "moscow-autumn" -> "moscow.bin"
            | "kuban-spring" | "kuban-summer" | "kuban-autumn" -> "kuban.bin"
            | "stalingrad-winter" | "stalingrad-summer" | "stalingrad-autumn" -> "stalingrad.bin"
            | "rheinland-summer" | "rheinland-winter" | "rheinland-spring" -> "rheinland.bin"
            | unsupported ->
                failwithf "Unsupported map '%s'" unsupported
        use freeAreasFile =
            try
                System.IO.File.OpenRead(path)
            with _ -> failwithf "Could not open free areas data file '%s'" path
        let serializer = MBrace.FsPickler.FsPickler.CreateBinarySerializer()
        try
            serializer.Deserialize(freeAreasFile)
        with e -> failwithf "Failed to read free areas data file, error was: %s" e.Message

    let getGroundLocationCandidates(region, shape) =
        match freeAreas with
        | Some root ->
            let rank _ = 
                random.Next()
            let candidates =
                FreeAreas.findPositionCandidates rank root shape region
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
                getGroundLocationCandidates(preferredArea, snd shape)
                |> Seq.map (fun v -> v, shape))
            |> Seq.interleave
            |> Seq.append (
                // Try again with the full region instead of the preferred area
                battleShapes
                |> Seq.map (fun shape ->
                    getGroundLocationCandidates(region.Boundary, snd shape)
                    |> Seq.map (fun v -> v, shape))
                |> Seq.interleave)
                    
        Seq.tryHead candidates
        |> Option.map (fun (pos, (rotation, shape)) ->
            { OrientedPosition.Pos = pos
              Rotation = rotation
              Altitude = 0.0f },
            shape)

    let battleLocationCache = Seq.mutableDict []

    let tryGetBattleLocationCached = SturmovikMission.Cached.cached battleLocationCache tryGetBattleLocation

    let tryGetGroundTargetLocation (regId : RegionId, targetType : GroundTargetType) =
        match targetType with
        | GroundForces targettedCoalition ->
            match tryGetBattleLocationCached regId with
            | None ->
                // No battle going on, find a free location for a camp
                let campShape =
                    [
                        Vector2(1000.0f, 1000.0f)
                        Vector2(-1000.0f, 1000.0f)
                        Vector2(-1000.0f, -1000.0f)
                        Vector2(1000.0f, -1000.0f)
                    ]
                let region = state.World.Regions.[regId]
                let candidates = getGroundLocationCandidates(region.Boundary, campShape)
                let filters =
                    match state.GetOwner(regId) with
                    | Some owner when owner = targettedCoalition ->
                        // Attacking a defense position, preferably close to the region's capital
                        [
                            fun p -> (p - region.Position).Length() < 5000.0f
                            fun p -> (p - region.Position).Length() < 10000.0f
                            fun _ -> true
                        ]
                    | Some _ ->
                        // Attacking an invading army, preferably close to the border
                        let borderVertices =
                            region.Neighbours
                            |> Seq.filter (fun nghId -> state.GetOwner(nghId) = Some targettedCoalition)
                            |> Seq.choose (fun nghId -> NewWorldDescription.commonBorder(region, state.World.Regions.[nghId]))
                            |> Seq.collect (fun (p1, p2) -> [p1; p2])
                            |> Array.ofSeq
                        let closeToBorder dist p =
                            borderVertices
                            |> Array.exists (fun bv -> (bv - p).Length() < dist)
                        [
                            closeToBorder 5000.0f
                            closeToBorder 10000.0f
                            fun _ -> true
                        ]
                    | None ->
                        // Attacking a position in a neutral region. Should not happen, but accept any position anyway
                        [fun _ -> true]
                filters
                |> Seq.tryPick (fun filter -> candidates |> Seq.tryFind filter)
            | Some (p, _) ->
                // There's a battle going on, use its location
                Some p.Pos

        | AirfieldTarget afid ->
            Some (state.World.Airfields.[afid].Position)

        | BridgeTarget | BuildingTarget ->
            let region = state.World.Regions.[regId]
            // Find the largest cluster of healthy buildings, and use that
            let clusters =
                if targetType = BridgeTarget then
                    computeHealthyBuildingClusters(1000.0f, state, state.World.Bridges.Values, regId)
                else
                    computeHealthyBuildingClusters(1000.0f, state, state.World.Buildings.Values, regId)
            clusters
            |> Seq.tryHead
            |> Option.map(fun (building, _) -> building.Pos.Pos)

    let getAirfieldAALocations (afId : AirfieldId) =
        let airfield = state.World.Airfields.[afId]
        let shape = VectorExtension.mkCircle(Vector2.Zero, 50.0f)
        let area radius = VectorExtension.mkCircle(airfield.Position, radius)
        seq { 2000.0f .. 500.0f .. 10000.0f }
        |> Seq.collect (fun radius -> getGroundLocationCandidates(area radius, shape))
        |> Seq.filter (fun v -> not(v.IsInConvexPolygon(airfield.Boundary)))
        |> Seq.map (fun v -> v, VectorExtension.mkCircle(v, 50.0f))

    let groundTargetLocationCache = Seq.mutableDict []

    let tryGetGroundTargetLocationCached = SturmovikMission.Cached.cached groundTargetLocationCache tryGetGroundTargetLocation

    member this.TryGetBattleLocation regId = tryGetBattleLocationCached regId

    member this.TryGetGroundTargetLocation(regId, targetType) = tryGetGroundTargetLocationCached(regId, targetType)

    member this.GetAirfieldAA(afId) = getAirfieldAALocations afId

    member this.GetGroundLocationCandidates(area, shape) = getGroundLocationCandidates(area, shape)


type GameType =
    | Coop
    | SinglePlayer
    | MultiPlayer

type PlayerSpawnType =
    | Airborne
    | Runway
    | Parking of WarmedUp: bool
with
    member this.IntValue =
        match this with
        | Airborne -> 0
        | Runway -> 1
        | Parking -> 2

type PlayerSpawnPlane =
    {
        Model : PlaneModel
        AllowedMods : ModRange list
        AllowedPayloads : ModRange list
        AllowedSkins : ModRange list
        Mods : int64
        Payload : int
        Skin : int
    }
with
    static member Default(plane) =
        {
            Model = plane
            AllowedMods = ModRange.All
            AllowedPayloads = ModRange.All
            AllowedSkins = ModRange.All
            Mods = 0L
            Payload = 0
            Skin = 0
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

type Campaign.NewWorldDescription.Runway with
    member this.Chart =
        let chart = T.Airfield.Chart.Default
        let getRelativeXZ =
            let x = Vector2.FromYOri(float this.SpawnPos.Rotation)
            let z = x.Rotate(90.0f)
            fun (v : Vector2) ->
                let v2 = v - this.SpawnPos.Pos
                float(Vector2.Dot(v2, x)), float(Vector2.Dot(v2, z))
        let mkPoint(v, t) =
            let x, z = getRelativeXZ v
            T.Airfield.Chart.Point.Default
                .SetX(T.Float.N x)
                .SetY(T.Float.N z)
                .SetType(T.Integer.N t)
        let points =
            [
                match this.PathToRunway with
                | initial :: path ->
                    yield mkPoint(initial, 0)
                    for v in path do
                        yield mkPoint(v, 1)
                | [] ->
                    ()
                yield mkPoint(this.Start, 2)
                yield mkPoint(this.End, 2)
                match List.rev this.PathOffRunway with
                | last :: rpath ->
                    yield! List.rev [
                        yield mkPoint(last, 0)
                        for v in rpath do
                            yield mkPoint(v, 1)
                    ]
                | [] ->
                    ()
            ]
        chart
            .SetPoint(points)

type PlayerSpawn =
    {
        Airfield : AirfieldId
        SpawnType : PlayerSpawnType
        Pos : OrientedPosition
        Flight : PlayerFlight
    }
with
    member this.BuildMCUs(store : NumericalIdentifiers.IdStore, state : IWarStateQuery) =
        let subst = Mcu.substId(store.GetIdMapper())
        let af = state.World.Airfields.[this.Airfield]
        let coalition = state.GetOwner(af.Region)
        match coalition with
        | None ->
            // No spawning in neutral regions
            McuUtil.groupFromList []
        | Some coalition ->
            if state.GetGroundForces(coalition.Other, af.Region) > 0.1f * state.GetGroundForces(coalition, af.Region) then
                // No spawning in regions with significant enemy presence
                McuUtil.groupFromList []
            else
            match this.Flight with
            | Unconstrained planes ->
                let runway =
                    let windDir = Vector2.FromYOri(float state.Weather.Wind.Direction)
                    try
                        af.Runways
                        |> Seq.maxBy (fun runway -> Vector2.Dot(windDir, (runway.End - runway.Start).Rotate(runway.SpawnPos.Rotation)))
                        |> Some
                    with _ -> None
                let spawn =
                    T.Airfield.Default
                        .SetIndex(T.Integer.N 1)
                        .SetLinkTrId(T.Integer.N 2)
                        .SetReturnPlanes(T.Boolean.N true)
                        .SetRefuelFriendlies(T.Boolean.N true)
                        .SetRearmFriendlies(T.Boolean.N true)
                        .SetRepairFriendlies(T.Boolean.N true)
                        .SetMaintenanceRadius(T.Integer.N 3000)
                        .SetRefuelTime(T.Integer.N 30)
                        .SetRearmTime(T.Integer.N 30)
                        .SetRepairTime(T.Integer.N 30)
                        .SetModel(T.String.N @"graphics\airfields\fakefield.mgm")
                        .SetScript(T.String.N @"LuaScripts\WorldObjects\Airfields\fakefield.txt")
                        .SetCountry(T.Integer.N (int (state.World.GetAnyCountryInCoalition coalition).ToMcuValue))
                let spawn =
                    match runway with
                    | Some runway ->
                        spawn.SetChart(Some runway.Chart)
                    | None ->
                        spawn
                let spawn =
                    match this.SpawnType with
                    | Airborne ->
                        spawn
                            .SetXPos(T.Float.N(float this.Pos.Pos.X))
                            .SetZPos(T.Float.N(float this.Pos.Pos.Y))
                            .SetYPos(T.Float.N(float this.Pos.Altitude))
                            .SetYOri(T.Float.N(float this.Pos.Rotation))
                    | Runway ->
                        let pos, ori =
                            match runway with
                            | Some runway ->
                                runway.Start, (runway.End - runway.Start).YOri
                            | _ ->
                                this.Pos.Pos, this.Pos.Rotation
                        spawn
                            .SetXPos(T.Float.N(float pos.X))
                            .SetZPos(T.Float.N(float pos.Y))
                            .SetYOri(T.Float.N(float ori))
                    | Parking _ ->
                        let pos, ori =
                            match runway with
                            | Some runway ->
                                runway.SpawnPos.Pos, runway.SpawnPos.Rotation
                            | _ ->
                                this.Pos.Pos, this.Pos.Rotation
                        spawn
                            .SetXPos(T.Float.N(float pos.X))
                            .SetZPos(T.Float.N(float pos.Y))
                            .SetYOri(T.Float.N(float ori))
                let planes =
                    planes
                    |> List.map (fun plane ->
                        let modFilters = ModRange.ModFilters plane.AllowedMods
                        let payloadFilters = ModRange.ModFilters plane.AllowedPayloads
                        let skinFilters = ModRange.ModFilters plane.AllowedSkins
                        let afPlane = newAirfieldPlane(modFilters, payloadFilters, plane.Mods, plane.Payload, skinFilters, plane.Model.Name, -1)
                        afPlane
                            .SetAILevel(T.Integer.N 2)  // Normal
                            .SetStartInAir(T.Integer.N this.SpawnType.IntValue)
                            .SetModel(T.String.N plane.Model.ScriptModel.Model)
                            .SetScript(T.String.N plane.Model.ScriptModel.Script)
                    )
                let spawn =
                    spawn.SetPlanes(Some(T.Airfield.Planes.Default.SetPlane planes))
                let spawnEntity =
                    T.MCU_TR_Entity.Default.SetIndex(T.Integer.N 2).SetMisObjID(T.Integer.N 1).SetEnabled(T.Boolean.N true)
                let mcus =
                    [ spawn.CreateMcu(); spawnEntity.CreateMcu() ]
                for mcu in mcus do subst mcu
                McuUtil.groupFromList mcus
            | Directed _ ->
                failwith "TODO"

type GroundBattleNumbers =
    {
        NumRocketArtillery : int
        NumArtillery : int
        NumAntiTankGuns : int
        NumTanks : int
    }

type private AreaLocation =
    | DefenseBack
    | DefenseMiddle
    | AttackBack
    | AttackMiddle

type GroundBattle =
    {
        Region : RegionId
        Boundary : Vector2 list
        Pos : OrientedPosition
        Defending : CountryId
        Attacking : CountryId
        NumDefending : GroundBattleNumbers
        NumAttacking : GroundBattleNumbers
    }
with
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
                Region = mission.Objective
                Boundary = area
                Pos = pos
                Defending = getCountryInCoalition initiator.Other
                Attacking = getCountryInCoalition initiator
                NumDefending = computeNum(state.GetGroundForces(initiator.Other, mission.Objective))
                NumAttacking = computeNum(state.GetGroundForces(initiator, mission.Objective))
            }
        | _ ->
            None

    member this.CreateMCUs(random : System.Random, store, lcStore, region, startTrigger) =
        let defendingCoalition = this.Defending.Coalition
        // Get a random position within the bounding rectangle of the boundary
        let getRandomPos(areaLocation) =
            let dir = Vector2.FromYOri(float this.Pos.Rotation)
            let back =
                this.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - this.Pos.Pos, dir))
                |> Seq.min
            let front =
                this.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - this.Pos.Pos, dir))
                |> Seq.max
            let side = Vector2.FromYOri (float this.Pos.Rotation + 90.0)
            let left =
                this.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - this.Pos.Pos, side))
                |> Seq.min
            let right =
                this.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - this.Pos.Pos, side))
                |> Seq.max
            let r0 =
                let r = random.NextDouble() |> float32
                match areaLocation with
                | AttackBack -> 0.25f * r
                | DefenseBack -> 1.0f - 0.25f * r
                | AttackMiddle -> 0.25f * r + 0.25f
                | DefenseMiddle -> 0.75f - 0.25f * r
            let r1 = random.NextDouble() |> float32
            let dx = (back * (1.0f - r0) + front * r0) * dir
            let dz = (left * (1.0f - r1) + right * r1) * side
            this.Pos.Pos + dx + dz
        // Get a random position within the boundary
        let getRandomPos(areaLocation) =
            Seq.initInfinite (fun _ -> getRandomPos areaLocation)
            |> Seq.find (fun v -> v.IsInConvexPolygon(this.Boundary))
        // Build an attacking tank
        let buildTank (country : CountryId) (model : VehicleTypeData) =
            let tank = RespawningTank.Create(store, getRandomPos(AttackMiddle), getRandomPos(DefenseBack), country.ToMcuValue)
            let role = if country.Coalition = defendingCoalition then "D" else "A"
            tank.Tank.Name <- sprintf "B-%s-%s-%s" region role "medium"
            model.AssignTo(tank.Tank)
            tank |> Choice1Of2
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCannon (country : CountryId) (location, model : VehicleTypeData, wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(location), getRandomPos(AttackBack), country.ToMcuValue)
            let role = if country.Coalition = defendingCoalition then "D" else "A"
            arty.Canon.Name <- sprintf "B-%s-%s-%s" region role "medium"
            wallModel.AssignTo(arty.Wall)
            model.AssignTo(arty.Canon)
            arty |> Choice2Of2
        // Build moving and fixed tanks/guns
        let buildTanksAndSupport(country, composition : GroundBattleNumbers) =
            let attackers, support =
                let buildCannon = buildCannon country
                let buildTank = buildTank country
                let backPosition =
                    if country.Coalition = defendingCoalition then
                        DefenseBack
                    else
                        AttackBack
                seq {
                    for _ in 1 .. composition.NumTanks do
                        if country.Coalition = defendingCoalition then
                            match country with
                            | Russia -> (DefenseBack, vehicles.RussianMediumTank, vehicles.AntiTankPosition) |> buildCannon
                            | Italy | Germany -> (DefenseBack, vehicles.GermanMediumTank, vehicles.AntiTankPosition) |> buildCannon
                            | GreatBritain | UnitedStates -> (DefenseBack, vehicles.AmericanMediumTank, vehicles.AntiTankPosition) |> buildCannon
                        else
                            match country with
                            | Russia -> vehicles.RussianMediumTank |> buildTank
                            | Italy | Germany -> vehicles.GermanMediumTank |> buildTank
                            | GreatBritain | UnitedStates -> vehicles.AmericanMediumTank |> buildTank
                    for _ in 1 .. composition.NumAntiTankGuns do
                        match country with
                        | Russia -> (backPosition, vehicles.RussianAntiTankCanon, vehicles.AntiTankPosition) |> buildCannon
                        | Italy | Germany -> (backPosition, vehicles.GermanAntiTankCanon, vehicles.AntiTankPosition) |> buildCannon
                        | GreatBritain | UnitedStates -> (backPosition, vehicles.AmericanAntiTankCanon, vehicles.AntiTankPosition) |> buildCannon
                    for _ in 1 .. composition.NumArtillery do
                        match country with
                        | Russia -> (backPosition, vehicles.RussianArtillery, vehicles.ArtilleryPosition) |> buildCannon
                        | Italy | Germany -> (backPosition, vehicles.GermanArtillery, vehicles.ArtilleryPosition) |> buildCannon
                        | GreatBritain | UnitedStates -> (backPosition, vehicles.AmericanArtillery, vehicles.ArtilleryPosition) |> buildCannon
                    for _ in 1 .. composition.NumRocketArtillery do
                        match country with
                        | Russia -> (backPosition, vehicles.RussianRocketArtillery, vehicles.TankPosition) |> buildCannon
                        | Italy | Germany -> (backPosition, vehicles.GermanRocketArtillery, vehicles.TankPosition) |> buildCannon
                        | GreatBritain | UnitedStates -> (backPosition, vehicles.AmericanArtillery, vehicles.ArtilleryPosition) |> buildCannon
                }
                |> List.ofSeq
                |> List.partition (function Choice1Of2 _ -> true | _ -> false)
            let attackers = attackers |> List.map (function Choice1Of2 x -> x | _ -> failwith "Not a Choice1Of2")
            let support = support |> List.map (function Choice2Of2 x -> x | _ -> failwith "Not a Choice2Of2")
            attackers, support
        // Instantiate attacker blobks
        let attackers, support = buildTanksAndSupport(this.Attacking, this.NumAttacking)
        // Instantiate defender blocks
        let defenders, cannons = buildTanksAndSupport(this.Defending, this.NumDefending)

        // Icons
        let icon1 = BattleIcons.Create(store, lcStore, this.Pos.Pos, this.Pos.Rotation, this.NumDefending.NumTanks, this.NumAttacking.NumTanks, Defenders defendingCoalition.ToCoalition)
        let icon2 = BattleIcons.Create(store, lcStore, this.Pos.Pos, this.Pos.Rotation, this.NumAttacking.NumTanks, this.NumDefending.NumTanks, Attackers defendingCoalition.Other.ToCoalition)

        // Start
        for support in support do
            Mcu.addTargetLink startTrigger support.Start.Index
        for x in attackers do
            Mcu.addTargetLink startTrigger x.Start.Index
        for x in defenders do
            Mcu.addTargetLink startTrigger x.Start.Index
        for x in cannons do
            Mcu.addTargetLink startTrigger x.Start.Index

        // Result
        { new McuUtil.IMcuGroup with
            member x.Content = []
            member x.LcStrings = []
            member x.SubGroups = [
                for s in support do
                    yield s.All
                for a in attackers do
                    yield a.All
                for d in defenders do
                    yield d.All
                for d in cannons do
                    yield d.All
                yield icon1.All
                yield icon2.All
            ]
        }

type ConvoyMember =
    | Train
    | Truck
    | Tank
    | ArmoredCar
    | AntiAirTruck
    | StaffCar
with
    member this.VehicleData(country : CountryId) =
        match country, this with
        | (Russia | UnitedStates | GreatBritain), Train -> vehicles.RussianTrain
        | (Germany | Italy), Train -> vehicles.GermanTrain
        | Russia, Truck -> vehicles.RussianTruck
        | (UnitedStates | GreatBritain), Truck -> vehicles.AmericanTruck
        | (Germany | Italy), Truck -> vehicles.GermanTruck
        | Russia, Tank -> vehicles.RussianMediumTank
        | (UnitedStates | GreatBritain), Tank -> vehicles.AmericanMediumTank
        | (Germany | Italy), Tank -> vehicles.GermanMediumTank
        | (Russia | UnitedStates | GreatBritain), ArmoredCar -> vehicles.RussianLightArmor
        | (Germany | Italy), ArmoredCar -> vehicles.GermanLightArmor
        | Russia, AntiAirTruck -> vehicles.RussianMobileAA
        | (UnitedStates | GreatBritain), AntiAirTruck -> vehicles.AmericanMobileAA
        | (Germany | Italy), AntiAirTruck -> vehicles.GermanMobileAA
        | Russia, StaffCar -> vehicles.RussianCar
        | (UnitedStates | GreatBritain), StaffCar -> vehicles.AmericanCar
        | (Germany | Italy), StaffCar -> vehicles.GermanCar

type Convoy =
    {
        Country : CountryId
        Members : ConvoyMember list
        Start : OrientedPosition
        StartPositions : OrientedPosition list
        Destination : OrientedPosition
    }
with
    member this.CreateMCUs(store, lcStore, columnName, startTrigger) =
        let pathVertices : Factory.PathVertex list=
            [ this.Start; this.Destination ]
            |> List.map (fun p ->
                {
                    Factory.Pos = p.Pos
                    Ori = p.Rotation
                    Radius = 50
                    Speed = 50
                    Priority = 1
                    SpawnSide = Types.SpawnSide.Center
                    Role = Factory.PathVertexRole.Intermediate
                }
            )
        match this.Members with
        | [Train] ->
            let train = TrainWithNotification.Create(store, lcStore, true, pathVertices, [], this.Country.ToMcuValue, columnName)
            Mcu.addTargetLink startTrigger train.TheTrain.Start.Index
            train :> IMcuGroup
        | _ ->
            let columnContent =
                this.Members
                |> List.map (fun x -> x.VehicleData(this.Country))
            let column = Factory.VirtualConvoy.CreateColumn(store, lcStore, pathVertices, [], columnContent, this.Country.ToMcuValue, this.Country.Coalition.ToCoalition, columnName, 0)
            Mcu.addTargetLink startTrigger column.Api.Start.Index
            column :> IMcuGroup

type MissionGenSettings =
    {
        MaxAntiAirCannons : int
        MaxAiPatrolPlanes : int
        OutFilename : string
    }

/// Create the MCUs for all static blocks or bridges within a convex hull, creating entities for those that need entities (typically objectives for AI ground attackers).
let inline private mkStaticMCUs (store : NumericalIdentifiers.IdStore, state : IWarStateQuery, blocks, hull, hasEntity, filterDamages) =
    let blockAt =
        blocks
        |> Seq.map (fun block -> OrientedPosition.FromMission block, block)
        |> Seq.distinctBy fst
        |> Seq.mutableDict
    // Apply damages
    for (bId, part, health) in state.BuildingDamages |> Seq.filter filterDamages do
        let building = state.World.GetBuildingInstance(bId)
        match blockAt.TryGetValue(building.Pos) with
        | true, block ->
            let damages = CommonMethods.getDamaged block
            let damages = CommonMethods.setItem part (T.Float.N(float health)) damages
            let block = CommonMethods.setDamaged damages block
            let block = CommonMethods.setDurability (T.Integer.N(building.Properties.Durability)) block
            blockAt.[building.Pos] <- block
        | false, _ ->
            // No block at position. Maybe the mission file was edited after the campaign started. Bad, but not worth dying with an exception.
            logger.Warn(sprintf "Failed to set damage on building or bridge at %s. No building found at that position in the campaign mission file." (string building.Pos))
            ()
    // Cull everything not in the hull
    let blocks =
        blockAt
        |> Seq.filter (fun kvp -> kvp.Key.Pos.IsInConvexPolygon hull)
        |> List.ofSeq
    // Set country
    let blocks =
        blocks
        |> List.map (fun kvp ->
            let pos = kvp.Key
            let country =
                state.World.Regions.Values
                |> Seq.tryFind (fun region -> pos.Pos.IsInConvexPolygon region.Boundary)
                |> Option.bind (fun region -> state.GetOwner region.RegionId)
                |> Option.map (fun coalition -> state.World.GetAnyCountryInCoalition coalition)
            let block =
                match country with
                | Some country -> CommonMethods.setCountry (T.Integer.N (int country.ToMcuValue)) kvp.Value
                | None -> kvp.Value
            pos, block
        )
    // Create entities when needed, and create unique IDs
    let mcus =
        let subst = Mcu.substId <| store.GetIdMapper()
        [
            for pos, block in blocks do
                if hasEntity pos.Pos then
                    let subst = Mcu.substId <| store.GetIdMapper()
                    let mcu1 = CommonMethods.createMcu block
                    let mcu2 = newEntity (mcu1.Index + 1)
                    Mcu.connectEntity (mcu1 :?> Mcu.HasEntity) mcu2
                    for mcu in [mcu1; upcast mcu2] do
                        subst mcu
                        pos.Pos.AssignTo mcu.Pos
                        mcu.Pos.Y <- float pos.Altitude
                        mcu.Ori.Y <- float pos.Rotation
                    yield mcu1
                    yield upcast mcu2
                else
                    let mcu =
                        block
                        |> CommonMethods.setLinkTrId (T.Integer.N 0)
                        |> CommonMethods.createMcu
                    subst mcu
                    yield mcu
        ]
    // Result
    McuUtil.groupFromList mcus

/// Set the list of planes in the options of a mission file
let private addMultiplayerPlaneConfigs (world : NewWorldDescription.World) (options : T.Options) =
    let configs =
        world.PlaneSet.Values
        |> Seq.map (fun model -> T.String.N (model.ScriptModel.Script))
    options.SetMultiplayerPlaneConfig(List.ofSeq configs)

/// Data needed to create a multiplayer "dogfight" mission
type MultiplayerMissionContent =
    {
        Briefing : string
        Boundary : Vector2 list
        PlayerSpawns : PlayerSpawn list
        AntiAirNests : Nest list
        GroundBattles : GroundBattle list
        AiPatrols : AiPatrol list
        AiAttacks : AiAttack list
        Convoys : Convoy list
        ParkedPlanes : (PlaneModelId * OrientedPosition * CountryId) list
    }
with
    /// Get the AI patrols of a coalition
    member this.AiPatrolsOf(coalition, state : IWarStateQuery) =
        this.AiPatrols
        |> List.filter (fun patrol -> state.GetOwner(state.World.Airfields.[patrol.HomeAirfield].Region) = Some coalition)

    /// Get the AI attacks of a coalition
    member this.AiAttacksOf(coalition, state : IWarStateQuery) =
        this.AiAttacks
        |> List.filter (fun patrol -> state.GetOwner(state.World.Airfields.[patrol.HomeAirfield].Region) = Some coalition)

    /// Create the groups suitable for a multiplayer "dogfight" misison
    member this.BuildMission(random, settings : MissionGenSettings, state : IWarStateQuery) =
        let strategyMissionData = T.GroupData.Parse(Parsing.Stream.FromFile (state.World.Scenario + ".Mission"))
        let options = Seq.head strategyMissionData.ListOfOptions
        let store = NumericalIdentifiers.IdStore()
        let lcStore = NumericalIdentifiers.IdStore()
        lcStore.SetNextId 3
        let getId = store.GetIdMapper()
        let missionBegin = newMissionBegin (getId 1)

        let inTargetedArea(pos : Vector2) =
            this.AiAttacks
            |> Seq.exists(fun attack -> (attack.Target - pos).Length() < 10000.0f)

        // Mission name, briefing, author
        let optionStrings =
            { new McuUtil.IMcuGroup with
                  member x.Content = []
                  member x.LcStrings =
                    [ (0, "Dynamic online campaign " + state.World.Scenario)
                      (1, this.Briefing)
                      (2, "auto-generated-coconut-campaign")
                    ]
                  member x.SubGroups = []
            }
        // Weather and player planes
        let options =
            (Weather.setOptions random state.Weather state.Date options)
                .SetMissionType(T.Integer.N 2) // deathmatch
                |> addMultiplayerPlaneConfigs state.World

        // Static buildings and blocks
        let buildings =
            let isBuilding (bId : BuildingInstanceId, _, _) =
                state.World.Buildings.ContainsKey bId
            mkStaticMCUs(store, state, strategyMissionData.GetGroup("Static").ListOfBlock, this.Boundary, inTargetedArea, isBuilding)

        // Bridges
        let bridges =
            let isBridge (bId : BuildingInstanceId, _, _) =
                state.World.Bridges.ContainsKey bId
            let bridges =
                [ "BridgesHW" ; "BridgesRW" ]
                |> Seq.map strategyMissionData.GetGroup
                |> Seq.collect (fun g -> g.ListOfBridge)
            mkStaticMCUs(store, state, bridges, this.Boundary, inTargetedArea, isBridge)

        // Spawns
        let spawns =
            this.PlayerSpawns
            |> List.map (fun ps -> ps.BuildMCUs(store, state))

        // AA
        let retainedAA =
            this.AntiAirNests
            |> StaticDefenseOptimization.select random settings.MaxAntiAirCannons
            |> StaticDefenseOptimization.instantiateAll store lcStore random missionBegin
            |> List.map (fun grp -> grp :> IMcuGroup)

        // Ground patrols
        let battles =
            this.GroundBattles
            |> List.map (fun battle -> battle.CreateMCUs(random, store, lcStore, string battle.Region, missionBegin))

        // Patrols
        let allPatrols =
            let axisGroup, axisPatrols =
                this.AiPatrolsOf(Axis, state)
                |> AiPlanes.AiPatrol.ToConstrainedPatrolBlocks (settings.MaxAiPatrolPlanes, store, lcStore, Vector2(1000.0f, 0.0f))
            let alliesGroup, alliesPatrols =
                this.AiPatrolsOf(Allies, state)
                |> AiPlanes.AiPatrol.ToConstrainedPatrolBlocks (settings.MaxAiPatrolPlanes, store, lcStore, Vector2(2000.0f, 0.0f))
            for block in axisPatrols @ alliesPatrols do
                Mcu.addTargetLink missionBegin block.Start.Index
            [ axisGroup; alliesGroup ]

        // Attacks
        let allAttacks =
            let axisAttacks =
                this.AiAttacksOf(Axis, state)
                |> List.map (fun attack -> attack.ToPatrolBlock(store, lcStore))
            let alliesAttacks =
                this.AiAttacksOf(Allies, state)
                |> List.map (fun attack -> attack.ToPatrolBlock(store, lcStore))
            let mkAttackStarts (attacks : (_ * GroundAttack.Attacker list) list) =
                // Spawn "wing" immediately after "leader"
                for (_, blocks) in attacks do
                    for b1, b2 in Seq.pairwise blocks do
                        Mcu.addTargetLink b1.Spawned b2.Start.Index
                // Spawn pairs one minute after previous pair
                for (_, block1), (_, block2) in Seq.pairwise attacks do
                    match block1, block2 with
                    | b1 :: _, b2 :: _ ->
                        b2.StartDelay <- 60.0
                        Mcu.addTargetLink b1.Spawned b2.Start.Index
                    | _, _ ->
                        failwith "Expected at least one block in each attack list"
                // Start first pair one minute after mission start
                match attacks with
                | (_, hd :: _) :: _ ->
                    hd.StartDelay <- 60.0
                    Mcu.addTargetLink missionBegin hd.Start.Index
                | _ -> ()
            mkAttackStarts axisAttacks
            mkAttackStarts alliesAttacks
            axisAttacks @ alliesAttacks |> List.map fst

        // Convoys
        let convoys : IMcuGroup list =
            this.Convoys
            |> List.mapi (fun i convoy -> convoy.CreateMCUs(store, lcStore, sprintf "convoy%02d" (i + 1), missionBegin))

        // Parked planes
        let mkParkedPlane(model : PlaneModel, pos : OrientedPosition, country) =
            let modelScript = model.StaticScriptModel
            let mcus =
                let durability =
                    match model.Kind with
                    | PlaneType.Fighter -> 8000
                    | PlaneType.Attacker -> 11000
                    | PlaneType.Bomber | PlaneType.Transport -> 12000
                let block, entity = newBlockWithEntityMcu store country modelScript.Model modelScript.Script durability
                [ block; upcast entity ]
            for mcu in mcus do
                pos.Pos.AssignTo mcu.Pos
                mcu.Ori.Y <- float pos.Rotation
            McuUtil.groupFromList mcus

        let parkedPlanes =
            this.ParkedPlanes
            |> List.map (fun (plane, pos, country) -> mkParkedPlane(state.World.PlaneSet.[plane], pos, int country.ToMcuValue))

        // Mission end triggered by server input
        let serverInputMissionEnd = MissionEnd.MissionEnd.Create(store)

        // Result
        let allGroups =
            [
                yield optionStrings
                yield buildings
                yield bridges
                yield! spawns
                yield McuUtil.groupFromList [ missionBegin ]
                yield! retainedAA
                yield! battles
                yield! allPatrols
                yield! allAttacks
                yield! convoys
                yield! parkedPlanes
                yield serverInputMissionEnd.All
            ]

        // Create directories in path to file, if needed
        let outDir = System.IO.Path.GetDirectoryName(settings.OutFilename)
        if not(System.IO.Directory.Exists(outDir)) then
            try
                System.IO.Directory.CreateDirectory(outDir)
                |> ignore
            with _ -> ()

        // Write file
        McuOutput.writeMissionFiles "eng" settings.OutFilename options allGroups

/// Create the descriptions of the groups to include in a mission file depending on a selected subset of missions.
let mkMultiplayerMissionContent (random : System.Random) briefing (state : WarState) (missions : MissionSelection) =
    let locator = TargetLocator(random, state)
    let warmedUp = true

    // All regions that are involved in some mission
    let primaryRegions = missions.Regions(state.World)
    // All regions, including the primary ones, that are within 50km of the primary regions
    let nearRegions =
        let primaryRegionsVertices =
            primaryRegions
            |> Seq.collect (fun regId -> state.World.Regions.[regId].Boundary)
        state.World.Regions.Values
        |> Seq.map (fun region ->
            region,
            region.Boundary
            |> Seq.allPairs primaryRegionsVertices
            |> Seq.map (fun (v1, v2) -> (v1 - v2).Length())
            |> Seq.min)
        // Sort by increasing distance to the primary regions
        |> Seq.sortBy snd
        |> Seq.cache
    // Count number of airfields in each coalition in the regions in the prefixes of nearRegions
    let numAfs =
        nearRegions
        |> Seq.scan (fun (numAxisAfs, numAlliesAfs) (region, _) ->
            let airfieldsInRegion =
                state.World.Airfields.Values
                |> Seq.filter (fun af ->
                    af.Region = region.RegionId &&
                    not af.Runways.IsEmpty &&
                    state.GetNumPlanes(af.AirfieldId) |> Seq.sumBy (fun kvp -> kvp.Value) > 10.0f)
                |> Seq.length
            match state.GetOwner(region.RegionId) with
            | Some Axis -> numAxisAfs + airfieldsInRegion, numAlliesAfs
            | Some Allies -> numAxisAfs, numAlliesAfs + airfieldsInRegion
            | None -> numAxisAfs, numAlliesAfs) (0, 0)
    // Take all regions within 50km, and until each coalition has at least 5 airfields
    let gameRegions =
        Seq.zip nearRegions numAfs
        |> Seq.takeWhile (fun ((region, dist), (numAxisAfs, numAlliesAfs)) ->
            dist < 50000.0f || numAxisAfs < 5 || numAlliesAfs < 5)
        |> Seq.map (fun ((x, _), _) -> x)
        // Include all the entry regions
        |> Seq.append (state.World.Regions.Values |> Seq.filter (fun region -> region.IsEntry))
        |> Seq.distinct
        |> List.ofSeq
    // Play area is the convex hull of all these regions
    let boundary =
        gameRegions
        |> Seq.collect (fun region -> region.Boundary)
        |> List.ofSeq
        |> convexHull

    // Player spawns
    let spawns =
        [
            let within =
                state.World.Airfields.Values
                |> Seq.filter (fun af -> af.Position.IsInConvexPolygon boundary && not af.Runways.IsEmpty)
            let wind = Vector2.FromYOri(state.Weather.Wind.Direction)
            for af in within do
                let runway =
                    af.Runways
                    |> List.maxBy(fun runway ->
                        let direction =
                            runway.End - runway.Start
                        let direction = direction / direction.Length()
                        Vector2.Dot(direction, wind))
                let planes =
                    state.GetNumPlanes(af.AirfieldId)
                    |> Map.toSeq
                    |> Seq.choose (fun (planeId, num) ->
                        if num >= 1.0f then
                            let plane = state.World.PlaneSet.[planeId]
                            Some (PlayerSpawnPlane.Default plane)
                        else
                            None)
                    |> List.ofSeq
                let spawn =
                    {
                        Airfield = af.AirfieldId
                        SpawnType = Parking warmedUp
                        Pos = runway.SpawnPos
                        Flight = Unconstrained planes
                    }
                yield spawn
        ]

    // AA nests
    let aaNests =
        [
            let gunsPerNest = 5
            // Resource planning aims to avoid running dry before next resupply, which is assumed to be one day of combat
            let combatTimeBeforeResuply = 12.0f<H>
            let aaCost = float32 gunsPerNest * TargetType.Artillery.GroundForceValue * state.World.GroundForcesCost * state.World.ResourceVolume * combatTimeBeforeResuply
            // Airfields
            for afId in spawns |> Seq.map (fun spawn -> spawn.Airfield) do
                let country = 
                    state.GetOwner(state.World.Airfields.[afId].Region)
                    |> Option.map state.World.GetAnyCountryInCoalition
                    |> Option.defaultWith (fun () -> failwith "Spawn in neutral region")
                let numNests = int(state.GetAirfieldCapacity(afId) / aaCost) |> max 1
                let positions =
                    locator.GetAirfieldAA(afId)
                    |> Seq.truncate numNests
                for _, shape in positions do
                    let nest =
                        { Priority = 2.0f
                          Number = gunsPerNest
                          Boundary = shape
                          Rotation = 0.0f
                          Settings = CanonGenerationSettings.Strong
                          Specialty = DefenseSpecialty.AntiAirCanon
                          IncludeSearchLights = true
                          IncludeFlak = true
                          Country = country.ToMcuValue
                        }
                    yield nest

            // Bridges on the paths from regions to the neighbours
            let getCriticialBridges (network : Network) (regionA, regionB) =
                let nodes = network.Nodes |> List.filter (fun node -> node.Region = regionA || node.Region = regionB)
                let links =
                    network.Links
                    |> List.filter (fun link -> link.Bridges |> Seq.forall (fun bid -> state.GetBridgeFunctionalityLevel(bid) > 0.5f))
                let network =
                    { network with
                        Nodes = nodes
                        Links = links
                    }
                let sources =
                    nodes
                    |> Seq.filter (fun node -> node.Region = regionA && node.HasTerminal)
                    |> Seq.map (fun node -> node.Id)
                    |> Set
                let goals =
                    nodes
                    |> Seq.filter (fun node -> node.Region = regionB && node.HasTerminal)
                    |> Seq.map (fun node -> node.Id)
                    |> Set
                network.GetQuickAccess().FindPath(sources, goals)
                |> Option.map (fun links ->
                    links
                    |> Seq.collect (fun link -> link.Bridges))
                |> Option.defaultValue Seq.empty

            let allCriticalBridges =
                gameRegions
                |> Seq.collect (fun region -> Seq.allPairs [region.RegionId] region.Neighbours)
                |> Seq.distinctBy (fun (regionA, regionB) -> min regionA regionB, max regionA regionB)
                |> Seq.collect (fun regs -> Seq.append (getCriticialBridges state.World.Roads regs) (getCriticialBridges state.World.Rails regs))
                |> Seq.distinct

            for BuildingInstanceId pos in allCriticalBridges do
                let area = VectorExtension.mkCircle(pos.Pos, 2000.0f)
                let shape = VectorExtension.mkCircle(Vector2.Zero, 50.0f)
                let p =
                    locator.GetGroundLocationCandidates(area, shape)
                    |> Seq.tryHead
                match p with
                | Some p -> 
                    let country = 
                        state.World.Regions.Values
                        |> Seq.tryFind (fun region -> pos.Pos.IsInConvexPolygon region.Boundary)
                        |> Option.bind (fun region -> state.GetOwner(region.RegionId))
                        |> Option.map state.World.GetAnyCountryInCoalition
                    match country with
                    | Some country ->
                        let nest =
                            { Priority = 0.0f
                              Number = gunsPerNest
                              Boundary = shape |> List.map ((+) p)
                              Rotation = 0.0f
                              Settings = CanonGenerationSettings.Default
                              Specialty = DefenseSpecialty.AntiAirMg
                              IncludeSearchLights = true
                              IncludeFlak = true
                              Country = country.ToMcuValue
                            }
                        yield nest
                    | None ->
                        ()
                | None ->
                    ()

            // Factories
            for coalition, region in state.World.Regions |> Seq.choose (fun kvp -> state.GetOwner(kvp.Key) |> Option.attach kvp.Value) do
                let clusters =
                    computeHealthyBuildingClusters(3000.0f, state, state.World.Buildings.Values, region.RegionId)
                    |> List.ofSeq
                let country = state.World.GetAnyCountryInCoalition(coalition)
                let rec work (covered : Set<BuildingInstanceId>) clusters =
                    match clusters with
                    | [] -> Seq.empty
                    | (rep : BuildingInstance, group) :: clusters ->
                        if covered.Contains (rep.Id) then
                            work covered clusters
                        else
                            seq {
                                let shape = VectorExtension.mkCircle(Vector2.Zero, 50.0f)
                                let location =
                                    seq { 2000.0f .. 500.0f .. 5000.0f }
                                    |> Seq.map (fun radius -> VectorExtension.mkCircle(rep.Pos.Pos, radius))
                                    |> Seq.collect (fun area -> locator.GetGroundLocationCandidates(area, shape))
                                    |> Seq.tryHead
                                match location with
                                | None ->
                                    yield! work covered clusters
                                | Some p ->
                                    let nest =
                                        { Priority = 0.0f
                                          Number = gunsPerNest
                                          Boundary = shape |> List.map ((+) p)
                                          Rotation = 0.0f
                                          Settings = CanonGenerationSettings.Default
                                          Specialty = DefenseSpecialty.AntiAirCanon
                                          IncludeSearchLights = true
                                          IncludeFlak = true
                                          Country = country.ToMcuValue
                                        }
                                    yield nest
                                    let covered =
                                        group
                                        |> Seq.map (fun (b : BuildingInstance) -> b.Id)
                                        |> Set
                                        |> Set.union covered
                                    yield! work covered clusters
                            }
                yield! work Set.empty clusters

            // Ground troops
            for m in missions.GroundMissions do
                let region = state.World.Regions.[m.Objective]
                match m.MissionType with
                | GroundBattle _ ->
                    // AA guns at ground battles
                    match state.GetOwner(m.Objective) with
                    | Some owner ->
                        match locator.TryGetBattleLocation(m.Objective) with
                        | Some(oriPos, _) ->
                            let posNeg = oriPos.Pos - Vector2.FromYOri(float oriPos.Rotation) * 3000.0f
                            let posPos = oriPos.Pos + Vector2.FromYOri(float oriPos.Rotation) * 3000.0f
                            // Position of region's owners is the one closest to the region's capital
                            let posOwner, posInvader =
                                if (posNeg - region.Position).Length() < (posPos - region.Position).Length() then
                                    posNeg, posPos
                                else
                                    posPos, posNeg
                            let countryOwner = state.World.GetAnyCountryInCoalition(owner).ToMcuValue
                            let countryInvader = state.World.GetAnyCountryInCoalition(owner.Other).ToMcuValue
                            let shape = VectorExtension.mkCircle(Vector2.Zero, 500.0f)
                            let mkNest(p, country) =
                                {
                                    Priority = 0.0f
                                    Number = gunsPerNest
                                    Boundary = shape |> List.map ((+) p)
                                    Rotation = 0.0f
                                    Settings = CanonGenerationSettings.Default
                                    Specialty = DefenseSpecialty.AntiAirMg
                                    IncludeSearchLights = true
                                    IncludeFlak = true
                                    Country = country
                                }
                            yield mkNest(posOwner, countryOwner)
                            yield mkNest(posInvader, countryInvader)
                        | None -> ()
                    | None -> ()
                | _ -> ()
        ]

    // Ground battles
    let battles =
        [
            for mission in missions.GroundMissions do
                match mission.MissionType with
                | GroundBattle initiator ->
                    match locator.TryGetBattleLocation(mission.Objective) with
                    | Some(p, area) ->
                        let owner = state.GetOwner(mission.Objective)
                        let aimedTowardsCapital = Vector2.Dot(state.World.Regions.[mission.Objective].Position - p.Pos, Vector2.FromYOri(float p.Rotation)) > 0.0f
                        let p =
                            // Make sure direction is so that initiators move along the battle direction, and region owners stand closer to the region capital
                            if aimedTowardsCapital && Some initiator = owner then
                                p
                            else
                                { p with Rotation = (p.Rotation + 180.0f) % 360.0f }
                        match GroundBattle.TryFromGroundMission(state, mission, p, area) with
                        | Some battle -> yield battle
                        | None -> ()
                    | None -> ()
                | _ -> ()
        ]

    // patrols
    let patrols =
        [
            // Home cover is 15km from the attacker's home airfield
            yield
                missions.HomeCover
                |> Option.bind (fun mission ->
                    let afPos = state.World.Airfields.[missions.MainMission.StartAirfield].Position
                    let objPos = state.World.Regions.[missions.MainMission.Objective].Position
                    let dir = objPos - afPos
                    let dir = dir / dir.Length()
                    let offset = dir * 15000.0f
                    let targetPos = afPos + offset
                    AiPatrol.TryFromAirMission(state, mission, targetPos))
            // Target cover is over the target
            yield
                missions.TargetCover
                |> Option.bind (fun cover ->
                    let targetType =
                        match missions.MainMission.MissionType with
                        | GroundTargetAttack(t, _) -> t
                        | _ -> GroundTargetType.BuildingTarget
                    let targetPos = locator.TryGetGroundTargetLocation(missions.MainMission.Objective, targetType)
                    targetPos
                    |> Option.bind (fun targetPos -> AiPatrol.TryFromAirMission(state, cover, targetPos)))
            // Interception is 15kw away from the target
            yield
                missions.Interception
                |> Option.bind (fun interception ->
                    let afPos = state.World.Airfields.[missions.MainMission.StartAirfield].Position
                    let objPos = state.World.Regions.[missions.MainMission.Objective].Position
                    let dir = objPos - afPos
                    let dir = dir / dir.Length()
                    let offset = dir * 15000.0f
                    let targetPos = objPos - offset
                    AiPatrol.TryFromAirMission(state, interception, targetPos))
            // Home attack is 30km from attacker's home airfield
            yield
                missions.HomeAttack
                |> Option.bind (fun mission ->
                    let afPos = state.World.Airfields.[missions.MainMission.StartAirfield].Position
                    let objPos = state.World.Regions.[missions.MainMission.Objective].Position
                    let dir = objPos - afPos
                    let dir = dir / dir.Length()
                    let offset = dir * 30000.0f
                    let targetPos = afPos + offset
                    AiPatrol.TryFromAirMission(state, mission, targetPos))
            // Other patrols are over the objective's capital
            for mission in missions.OtherMissions do
                match mission.Kind with
                | AirMission ({ MissionType = AreaProtection; Objective = objective } as airMission) ->
                    let objPos = state.World.Regions.[objective].Position
                    yield AiPatrol.TryFromAirMission(state, airMission, objPos)
                | _ -> ()
        ]
        |> List.choose id

    // Air attackers
    let attacks =
        [
            let mainTargetType =
                match missions.MainMission.MissionType with
                | GroundTargetAttack(t, _) -> t
                | _ -> GroundTargetType.BuildingTarget
            yield
                locator.TryGetGroundTargetLocation(missions.MainMission.Objective, mainTargetType)
                |> Option.bind (fun targetPos -> AiAttack.TryFromAirMission(state, missions.MainMission, targetPos))
        ]
        |> List.choose id

    // Result
    {
        Briefing = briefing
        Boundary = boundary
        PlayerSpawns = spawns
        AntiAirNests = aaNests
        GroundBattles = battles
        AiPatrols = patrols
        AiAttacks = attacks
        Convoys = []
        ParkedPlanes = []
    }

