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

/// Preparation of data needed for game mission files generation
module Campaign.MissionFilePreparation

open System.Numerics

open VectorExtension
open Util

open SturmovikMission.Blocks.StaticDefenses.Types

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel
open Campaign.Common.AiPlanes
open Campaign.Common.Buildings
open Campaign.Common.Targets

open Campaign.MissionGen.StaticDefenseOptimization
open Campaign.MissionGen.MissionFileGeneration

open Campaign.WarState
open Campaign.MissionSelection
open Campaign.Missions
open Campaign.SpacePartition
open Campaign.NewWorldDescription


let private logger = NLog.LogManager.GetCurrentClassLogger()

type AiStartPoint =
    | StartAtIngress of float32<M>
    | StartOverAirfield

type AiAttack with
    static member TryFromAirMission(state : IWarStateQuery, mission : AirMission, targetPos : Vector2, ?maxFlightSize, ?aiStartPoint) =
        let aiStartPoint = defaultArg aiStartPoint StartOverAirfield
        let coalition = state.GetOwner(state.World.Airfields.[mission.StartAirfield].Region)
        match mission, coalition with
        | { MissionType = Strafing(target) }, Some coalition
        | { MissionType = Bombing(target) }, Some coalition ->
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
            let roles =
                match mission.MissionType with
                | Strafing _ -> [PlaneRole.GroundAttacker]
                | Bombing _ -> [PlaneRole.GroundAttacker; PlaneRole.LevelBomber]
                | _ -> []
            let role =
                roles
                |> List.tryFind (fun role -> planeModel.Payloads.ContainsKey role)
            role
            |> Option.map (fun role ->
                let altitude =
                    match role with
                    | PlaneRole.LevelBomber -> 5000.0f
                    | _ -> 3500.0f
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
    static member TryFromAirMission(state : IWarStateQuery, mission : AirMission, targetPos : Vector2, ?maxFlightSize) =
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
            | "rheinland-summer" | "rheinland-winter" | "rheinland-spring" | "rheinland-autumn" -> "rheinland.bin"
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
                Vector2(2000.0f, 1000.0f)
                Vector2(-2000.0f, 1000.0f)
                Vector2(-2000.0f, -1000.0f)
                Vector2(2000.0f, -1000.0f)
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


type Campaign.MissionGen.MissionFileGeneration.GroundBattle
with
    static member TryFromGroundMission(state : IWarStateQuery, mission : GroundMission, pos : OrientedPosition, area : Vector2 list) =
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
                Boundary = area |> List.map (fun v -> pos.Pos + v.Rotate(pos.Rotation))
                Pos = pos
                Defending = getCountryInCoalition initiator.Other
                Attacking = getCountryInCoalition initiator
                NumDefending = computeNum(state.GetGroundForces(initiator.Other, mission.Objective))
                NumAttacking = computeNum(state.GetGroundForces(initiator, mission.Objective))
            }
        | _ ->
            None

type PreparationSettings = {
    MaxTrainsPerSide : int
    MaxTruckColumnsPerSide : int
    MissionLength : System.TimeSpan
}

/// Create the descriptions of the groups to include in a mission file depending on a selected subset of missions.
let mkMultiplayerMissionContent (random : System.Random) (settings : PreparationSettings) briefing (state : IWarStateQuery) (missions : MissionSelection) =
    let locator = TargetLocator(random, state)
    let warmedUp = true

    // Play area is the convex hull of all regions
    let boundary =
        state.World.Regions.Values
        |> Seq.collect (fun region -> region.Boundary)
        |> List.ofSeq
        |> convexHull

    let hasLowLight = state.HasLowLight(settings.MissionLength)

    let wind = Vector2.FromYOri(state.Weather.Wind.Direction)

    // Player spawns
    let spawns =
        [
            let within =
                state.World.Airfields.Values
                |> Seq.filter (fun af -> af.Position.IsInConvexPolygon boundary && af.IsActive)
            for af in within do
                let runway =
                    af.PickAgainstWind(wind)
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
                          IncludeSearchLights = hasLowLight
                          IncludeFlak = true
                          Country = country.ToMcuValue
                        }
                    yield nest

            // Bridges on the paths from regions to the neighbours
            let getCriticalBridges (network : Network) (regionA, regionB) =
                // Nodes in other regions
                let nodesToRemove =
                    network.Nodes
                    |> Seq.choose (fun node ->
                        match node.Region with
                        | Some region ->
                            if region <> regionA && region <> regionB then
                                Some node.Id
                            else
                                None
                        | None ->
                            None)
                    |> Set.ofSeq
                let network = network.RemoveNodes(nodesToRemove)
                // Find shortest path from any terminal node in region A to any terminal node in region B
                let sources =
                    network.Nodes
                    |> Seq.filter (fun node -> node.Region = Some regionA && node.HasTerminal)
                    |> Seq.map (fun node -> node.Id)
                    |> Set
                let goals =
                    network.Nodes
                    |> Seq.filter (fun node -> node.Region = Some regionB && node.HasTerminal)
                    |> Seq.map (fun node -> node.Id)
                    |> Set
                network.GetQuickAccess().FindPath(sources, goals)
                |> Option.map (fun links ->
                    links
                    |> List.collect (fun link -> link.Bridges))
                |> Option.defaultValue []

            let allCriticalBridges =
                state.World.Regions.Values
                |> Seq.collect (fun region -> Seq.allPairs [region.RegionId] region.Neighbours)
                |> Seq.distinctBy (fun (regionA, regionB) -> min regionA regionB, max regionA regionB)
                |> Seq.collect (fun regs -> List.append (getCriticalBridges state.World.Roads regs) (getCriticalBridges state.World.Rails regs))
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
                              IncludeSearchLights = hasLowLight
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
                                          IncludeSearchLights = hasLowLight
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
                                    IncludeSearchLights = hasLowLight
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
                        | Strafing t | Bombing t -> t
                        | _ -> GroundTargetType.BuildingTarget
                    let targetPos = locator.TryGetGroundTargetLocation(missions.MainMission.Objective, targetType)
                    targetPos
                    |> Option.bind (fun targetPos -> AiPatrol.TryFromAirMission(state, cover, targetPos)))
            // Interception is 15km away from the target
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
                | Bombing t | Strafing t -> t
                | _ -> GroundTargetType.BuildingTarget
            yield
                locator.TryGetGroundTargetLocation(missions.MainMission.Objective, mainTargetType)
                |> Option.bind (fun targetPos -> AiAttack.TryFromAirMission(state, missions.MainMission, targetPos))
        ]
        |> List.choose id

    // Parked planes
    let parkedPlanes =
        /// Match planes with parking positions. Assumes that the list of planes and the list of positions
        /// are both sorted in descending wing span and radius, respectively
        let rec assign country (planes : (PlaneModel * int) list, spots : ParkingSpot list) =
            [
                match planes, spots with
                | [], _ | _, [] ->
                    // No planes left to park, or no spots left
                    ()
                | (_, 0) :: planes, _ ->
                    // Count of current plane model has reached 0, move to next one
                    yield! assign country (planes, spots)
                | (plane, num) :: planes, spot :: spots2 ->
                    if plane.WingSpan > 1.0f<M> * spot.Radius then
                        // No more sufficiently wide spots available, skip to next plane model
                        yield! assign country (planes, spots)
                    else
                        // Match, yield result
                        yield (plane.Id, spot.Pos, country)
                        // Decrease number of planes of the current model, move to next spot.
                        yield! assign country ((plane, num - 1) :: planes, spots2)
            ]
        [
            for airfield in state.World.Airfields.Values do
                match state.GetOwner(airfield.Region) with
                | Some coalition ->
                    let planes =
                        state.GetNumPlanes(airfield.AirfieldId)
                        |> Map.toSeq
                        |> Seq.map (fun (plane, num) -> state.World.PlaneSet.[plane], int num)
                        |> Seq.sortByDescending (fun (plane, num) -> plane.WingSpan)
                        |> List.ofSeq
                    let spots =
                        airfield.Facilities
                        |> List.collect (fun ((BuildingInstanceId pos) as bid) ->
                            state.World.GetBuildingInstance(bid).Properties.ParkingSpots
                            |> List.map (fun spot ->
                                { spot with
                                    Pos =
                                        let rot = spot.Pos.Rotation + pos.Rotation
                                        { spot.Pos with
                                            Rotation = rot
                                            Pos = spot.Pos.Pos.Rotate(rot) + pos.Pos
                                        }
                                }
                            )
                        )
                        |> List.sortByDescending(fun spot -> spot.Radius)
                    let alongRunway =
                        if airfield.IsActive then
                            let runway = airfield.PickAgainstWind(wind)
                            runway.PathToRunway
                            |> Seq.pairwise
                            |> Seq.collect (fun (v1, v2) ->
                                let ori = (v2 - v1).YOri
                                let dir = Vector2.FromYOri (float ori)
                                let len = (v2 - v1).Length()
                                let stepSize = 1.0f / (len / 25.0f)
                                seq {
                                    for t in stepSize / 2.0f .. stepSize .. 1.0f - stepSize do
                                        yield {
                                            Pos = {
                                                Pos = v1 + t * dir * len
                                                Rotation = ori
                                                Altitude = 0.0f
                                            }
                                            Radius = 12.5f
                                        }
                                }
                            )
                            |> List.ofSeq
                        else
                            []
                    let country = state.World.GetAnyCountryInCoalition(coalition)
                    yield! assign country (planes, spots @ alongRunway)
                | None ->
                    ()
        ]

    // Trains
    logger.Debug("Starting to prepare train convoys")
    let trains =
        let terminalsInRegion =
            state.World.Rails.Nodes
            |> Seq.filter (fun node -> node.HasTerminal)
            |> Seq.choose (fun node -> node.Region |> Option.map (fun region -> region, node))
            |> Seq.groupBy fst
            |> Seq.map (fun (region, nodes) -> region, nodes |> Seq.map snd |> List.ofSeq)
            |> dict
        let rails = state.World.Rails.GetQuickAccess()
        [
            for startRegion in state.World.Regions.Values do
                for destRegionId in startRegion.Neighbours do
                    match state.GetOwner(startRegion.RegionId), state.GetOwner(destRegionId) with
                    | Some owner, Some owner2 when owner = owner2 ->
                        match terminalsInRegion.TryGetValue(startRegion.RegionId), terminalsInRegion.TryGetValue(destRegionId) with
                        | (true, starts), (true, dests) ->
                            logger.Debug(sprintf "Considering train between %s and %s" (string startRegion.RegionId) (string destRegionId))
                            match state.TryGetTrainPath(starts, dests, Some owner) with
                            | Some links ->
                                let path =
                                    links
                                    |> Seq.map (fun link ->
                                        let pos = rails.GetNode(link.NodeA).Pos
                                        let dir = rails.GetNode(link.NodeB).Pos - pos
                                        {
                                            Pos = pos
                                            Rotation = dir.YOri
                                            Altitude = 0.0f
                                        }
                                    )
                                    |> List.ofSeq
                                logger.Debug("Found train path")
                                yield {
                                    Country = state.World.GetAnyCountryInCoalition(owner)
                                    Members = [ ConvoyMember.Train ]
                                    Path = path
                                    StartPositions = [ path.Head ]
                                }
                            | None ->
                                logger.Debug("No path found for train")
                                ()
                        | _ -> ()
                    | _ -> ()
        ]
        |> List.groupBy (fun convoy -> state.World.Countries.[convoy.Country])
        |> List.collect (fun (coalition, convoys) ->
            convoys
            |> Array.ofList
            |> Array.shuffle (System.Random(state.Seed))
            |> List.ofArray
            |> List.truncate settings.MaxTrainsPerSide)
    logger.Debug(sprintf "Generated a total of %d trains" trains.Length)

    // Result
    {
        Date = state.Date
        Briefing = briefing
        Boundary = boundary
        PlayerSpawns = spawns
        AntiAirNests = aaNests
        GroundBattles = battles
        AiPatrols = patrols
        AiAttacks = attacks
        Convoys = trains
        ParkedPlanes = parkedPlanes
    }
