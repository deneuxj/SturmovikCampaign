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
open Campaign.Common.GroundUnit

open Campaign.MissionGen.StaticDefenseOptimization
open Campaign.MissionGen.MissionFileGeneration

open Campaign.WarState
open Campaign.MissionSelection
open Campaign.Missions
open Campaign.SpacePartition
open Campaign.NewWorldDescription
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider


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
            let country = state.World.GetAnyCountryInCoalition(coalition)
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
                |> List.tryFind (fun role -> planeModel.PayloadOfRole role |> Option.isSome)
            role
            |> Option.map (fun role ->
                let altitude =
                    match mission.MissionType, role with
                    | Bombing _, PlaneRole.LevelBomber -> 5000.0f
                    | Bombing _, PlaneRole.GroundAttacker -> 3500.0f
                    | Bombing _, _ -> 2000.0f
                    | Strafing _, _ -> 2000.0f
                    | _ -> 3500.0f
                {
                    Attacker = planeModel
                    NumPlanes = numPlanes
                    AttackerReserve = reserve
                    HomeAirfield = mission.StartAirfield
                    Country = country
                    Coalition = coalition
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
            let country = state.World.GetAnyCountryInCoalition(coalition)
            let roles, protectedRegion =
                if state.GetOwner(mission.Objective) = Some coalition then
                    [PlaneRole.Interceptor; PlaneRole.Patroller], Some mission.Objective
                else
                    [PlaneRole.Patroller; PlaneRole.Interceptor], None
            let role =
                roles
                |> List.tryFind (fun role -> planeModel.PayloadOfRole role |> Option.isSome)
            role
            |> Option.map (fun role ->
                let altitude =
                    match role with
                    | PlaneRole.Patroller -> 3500.0f
                    | PlaneRole.Interceptor -> 5000.0f
                    | _ -> 3500.0f
                {
                    Plane = planeModel
                    NumPlanes = numPlanes
                    PlaneReserve = reserve
                    HomeAirfield = mission.StartAirfield
                    Country = country
                    Coalition = coalition
                    Pos = targetPos
                    Altitude = altitude
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
            | "stalingrad-1942" | "stalingrad-summer-1942" | "stalingrad-autumn-1942" -> "stalingrad.bin"
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
        // Remove airfields from free areas, to avoid putting AA protecting e.g. industry on runways
        |> (fun topNode ->
            (topNode, state.World.Airfields.Values)
            ||> Seq.fold (fun node af ->
                node
                |> Option.bind (fun node -> FreeAreas.subtract(100.0f * 100.0f, node, af.Boundary))
            )
        )

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
        airfield.Boundary
        |> Seq.map (fun v -> v, VectorExtension.mkCircle(v, 100.0f))

    let groundTargetLocationCache = Seq.mutableDict []

    let tryGetGroundTargetLocationCached = SturmovikMission.Cached.cached groundTargetLocationCache tryGetGroundTargetLocation

    member this.TryGetBattleLocation regId = tryGetBattleLocationCached regId

    member this.TryGetGroundTargetLocation(regId, targetType) = tryGetGroundTargetLocationCached(regId, targetType)

    member this.GetAirfieldAA(afId) = getAirfieldAALocations afId

    member this.GetGroundLocationCandidates(area, shape) = getGroundLocationCandidates(area, shape)


type Campaign.MissionGen.MissionFileGeneration.GroundBattle
with
    static member TryFromGroundMission(state : IWarStateQuery, mission : GroundMission, pos : OrientedPosition, area : Vector2 list, limits : GroundBattleNumbers) =
        match mission with
        | { MissionType = GroundBattle initiator } ->
            let computeNum (force : float32<MGF>) =
                { NumRocketArtillery = int(0.25f * force / TargetType.ArmoredCar.GroundForceValue) |> min limits.NumRocketArtillery
                  NumArtillery = int(0.25f * force / TargetType.Artillery.GroundForceValue) |> min limits.NumArtillery
                  NumAntiTankGuns = int(0.25f * force / TargetType.Artillery.GroundForceValue) |> min limits.NumAntiTankGuns
                  NumTanks = int(0.25f * force / TargetType.Tank.GroundForceValue) |> min limits.NumTanks
                }
            let getCountryInCoalition coalition = state.World.GetAnyCountryInCoalition coalition
            Some {
                Region = mission.Objective
                Boundary = area |> List.map (fun v -> pos.Pos + v.Rotate(pos.Rotation))
                Pos = pos
                Defending = getCountryInCoalition initiator.Other
                DefendingCoalition = initiator.Other
                Attacking = getCountryInCoalition initiator
                NumDefending = computeNum(state.GetGroundForces(initiator.Other, mission.Objective))
                NumAttacking = computeNum(state.GetGroundForces(initiator, mission.Objective))
            }
        | _ ->
            None

type PreparationSettings = {
    GroundBattleLimits : GroundBattleNumbers
    MaxTrainsPerSide : int
    MaxTruckColumnsPerSide : int
    MissionLength : System.TimeSpan
}

/// Create the descriptions of the groups to include in a mission file depending on a selected subset of missions.
let mkMultiplayerMissionContent (random : System.Random) (settings : PreparationSettings) briefing (state : IWarStateQuery) (missions : MissionSelection option) =
    let strategyMissionData = T.GroupData.Parse(Parsing.Stream.FromFile (state.World.Scenario + ".Mission"))
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

    let groundMissions =
        missions
        |> Option.map (fun m -> m.GroundMissions)
        |> Option.defaultValue Seq.empty

    // Player spawns
    let spawns =
        [
            let within =
                state.World.Airfields.Values
                |> Seq.filter (fun af -> af.Position.IsInConvexPolygon boundary && af.IsActive)
            for af in within do
                let minLength =
                    try
                        state.GetNumPlanes(af.AirfieldId)
                        |> Map.toSeq
                        |> Seq.map (fun (planeId, qty) -> if qty >= 1.0f then float32 state.World.PlaneSet.[planeId].MinRunwayLength else 0.0f)
                        |> Seq.max
                    with _ -> 0.0f
                let runway =
                    af.PickAgainstWind(wind, minLength)
                let coalition =
                    state.GetOwner(af.Region)
                let planes =
                    state.GetNumPlanes(af.AirfieldId)
                    |> Map.toSeq
                    |> Seq.collect (fun (planeId, num) ->
                        if num >= 1.0f then
                            let planes =
                                match state.World.PlaneAlts.TryGetValue planeId with
                                | true, planes -> planes
                                | false, _ -> [state.World.PlaneSet.[planeId]]
                            planes
                            |> Seq.choose (fun plane ->
                                if Some plane.Coalition = coalition then
                                    let forbiddenMods =
                                        plane.WeaponModsCosts
                                        |> List.filter (fun (wmod, cost) -> (float32 cost) > state.GetCoalitionBudget(plane.Coalition))
                                    let allowedMods =
                                        match forbiddenMods with
                                        | [] ->
                                            [ModRange.Interval(1, plane.LastWeaponMod)]
                                        | _ ->
                                            let sorted =
                                                forbiddenMods
                                                |> List.map fst
                                                |> List.sort
                                            ([1, plane.LastWeaponMod], sorted)
                                            ||> List.fold (fun ranges wmod ->
                                                ranges
                                                |> List.collect (fun (a, b) ->
                                                    if a < wmod && wmod < b then
                                                        [(a, wmod - 1); (wmod + 1, b)]
                                                    elif a = wmod && wmod = b then
                                                        []
                                                    elif a = wmod then
                                                        [wmod + 1, b]
                                                    elif wmod = b then
                                                        [a, wmod - 1]
                                                    else
                                                        [a, b]))
                                            |> List.choose (fun (a, b) ->
                                                if a < b then
                                                    Some(Interval(a, b))
                                                elif a = b then
                                                    Some(One(a))
                                                else
                                                    None)
                                    Some ( { PlayerSpawnPlane.Default plane with AllowedMods = allowedMods } )
                                else
                                    None)
                        else
                            Seq.empty)
                    |> List.ofSeq
                let spawn =
                    {
                        Airfield = af.AirfieldId
                        SpawnType = Parking warmedUp
                        Pos = runway.SpawnPos
                        RunwayName = runway.Name
                        Flight = Unconstrained planes
                    }
                yield spawn
        ]

    // Vehicle parks
    let camps =
        [
            let random = System.Random(state.Seed)
            // Vehicles to choose from for the random picking.
            let vehicles = [| (ArmoredCar, 3); (Truck, 2); (StaffCar, 1); (AntiAirTruck, 1); (Tank, 1) |]
            let totalWeight = vehicles |> Array.sumBy snd
            let vehicles =
                (vehicles.[0], vehicles |> Seq.skip 1)
                ||> Seq.scan (fun (v, n) (v2, n2) ->
                    (v2, n + n2)
                )
                |> List.ofSeq
            let avgCost =
                vehicles
                |> Seq.sumBy (fun (v, _) -> v.AsTargetType.GroundForceValue)
                |> fun x -> x / (float32 vehicles.Length)
            let getRandomVehicle() =
                let idx =
                    random.Next(totalWeight)
                vehicles
                |> List.tryFind (fun (v, w) -> (idx + 1) <= w)
                |> Option.defaultValue vehicles.[0]
                |> fst
            let regionsWithBattles =
                missions
                |> Option.map (fun missions ->
                    missions.GroundMissions
                    |> Seq.choose (
                        function
                        | { MissionType = GroundBattle _ } as battle -> Some battle.Objective
                        | _ -> None)
                    |> Set.ofSeq)
                |> Option.defaultValue Set.empty
            let regionsWithoutBattles =
                state.World.Regions.Values
                |> Seq.filter (fun region -> not(regionsWithBattles.Contains region.RegionId))

            // Find best match in ground units for a given coalition and convoy member
            let desiredRoles =
                [
                    StaffCar, [ GroundRole.Command; GroundRole.Support ]
                    ArmoredCar, [ GroundRole.MachineGun ]
                    Tank, [ GroundRole.AntiTank; GroundRole.MachineGun ]
                    AntiAirTruck, [ GroundRole.AntiAirMachineGun ]
                    Truck, [ GroundRole.Support ]
                ]
                |> Map.ofList
            let groundUnitCache = Seq.mutableDict []
            let getGroundUnit =
                SturmovikMission.Cached.cached
                    groundUnitCache
                    (fun (coalition : CoalitionId, convoyMember : ConvoyMember) ->
                        state.World.GroundUnitsOfCountryList
                        |> List.tryPick (fun (country, groundUnits) ->
                            state.World.Countries.TryGetValue(country)
                            |> Option.ofPair
                            |> Option.bind (fun coalition2 ->
                                if coalition = coalition2 then
                                    let isMainCountry = (country = state.World.GetAnyCountryInCoalition coalition)
                                    groundUnits
                                    // Get GroundUnit data
                                    |> List.choose (state.World.GroundUnits.TryGetValue >> Option.ofPair)
                                    // Retain only the units with the proper roles, rank by desired role
                                    |> List.choose (fun groundUnit ->
                                        Seq.allPairs (Seq.indexed desiredRoles.[convoyMember]) groundUnit.Roles
                                        |> Seq.tryPick (fun ((rank, desired), offered) ->
                                            if offered = desired then
                                                Some (groundUnit, rank)
                                            else
                                                None)
                                    )
                                    // Prefer mobile units, preferably static blocks (unless AA), units of the main country of the coalition, weakest first
                                    |> List.sortBy (fun (groundUnit, roleRank) ->
                                        roleRank,
                                        (if groundUnit.IsMobile then 0 else 1),
                                        (if convoyMember = AntiAirTruck then
                                            if groundUnit.DynamicScriptModel.IsSome then 0 else 1
                                         else
                                            if groundUnit.StaticScriptModel.IsSome then 0 else 1),
                                        (if isMainCountry then 0 else 1),
                                        groundUnit.Durability
                                    )
                                    |> List.tryHead
                                    |> Option.map (fun (gu, _) -> gu.Id, country)
                                else
                                    None
                            )
                        )
                    )

            for region in regionsWithoutBattles do
                for coalition in [ Axis; Allies ] do
                    let forces =
                        state.GetGroundForces(coalition, region.RegionId)
                        |> min (20.0f * TargetType.Tank.GroundForceValue)
                    if forces > 10.0f * TargetType.ArmoredCar.GroundForceValue then
                        // Find an area
                        let radius = 0.500f
                        let shape = mkCircle(Vector2.Zero, radius)
                        let locations = locator.GetGroundLocationCandidates (region.Boundary, shape)
                        let yori = float32(random.NextDouble() * 350.0)
                        match Seq.tryHead locations with
                        | Some location ->
                            let vehicles =
                                Seq.initInfinite (fun _ ->
                                    let convoyMember = getRandomVehicle()
                                    let groundUnitAndCountry = getGroundUnit(coalition, convoyMember)
                                    match groundUnitAndCountry with
                                    | Some x ->
                                        Some(convoyMember, x)
                                    | None ->
                                        logger.Warn(sprintf "Failed to find ground unit for %s" (string convoyMember))
                                        None)
                                |> Seq.cache
                                |> Seq.choose id
                            let vehicles =
                                ((forces, None), vehicles)
                                ||> Seq.scan (fun (forces, _) (convoyMember, guc) ->
                                    forces - convoyMember.AsTargetType.GroundForceValue, Some guc
                                )
                                |> Seq.takeWhile (fun (forces, _) -> forces >= 0.0f<MGF>)
                                |> Seq.choose snd
                                
                            if not(Seq.isEmpty vehicles) then
                                yield { Coalition = coalition; Pos = { Pos = location; Rotation = yori; Altitude = 0.0f }; Vehicles = vehicles }
                        | None ->
                            ()
        ]

    // AA nests
    let aaNests =
        [
            let gunsPerNest = 5
            let nestCost = float32 gunsPerNest * TargetType.Artillery.GroundForceValue * state.World.GroundForcesCost * state.World.ResourceVolume * (1.0f<H> * float32 settings.MissionLength.TotalHours)
            let nestCost = nestCost / 1.0f<M^3>
            // Airfields
            for afId in spawns |> Seq.map (fun spawn -> spawn.Airfield) do
                let airfield = state.World.Airfields.[afId]
                let country = 
                    state.GetOwner(airfield.Region)
                    |> Option.map state.World.GetAnyCountryInCoalition
                    |> Option.defaultWith (fun () -> failwith "Spawn in neutral region")
                let positions =
                    locator.GetAirfieldAA(afId)
                for _, shape in positions do
                    let nest =
                        { Priority = 5.0f
                          Number = gunsPerNest
                          Boundary = shape
                          Rotation = 0.0f
                          Settings =
                            if state.World.Regions.[airfield.Region].IsEntry then
                                CanonGenerationSettings.StrongRespawning
                            else
                                CanonGenerationSettings.Skilled15min
                          Specialty = DefenseSpecialty.AntiAirCanon
                          IncludeSearchLights = hasLowLight
                          IncludeFlak = true
                          Country = country.ToMcuValue
                          Coalition = state.World.Countries.[country]
                          Group = airfield.Region :> System.IComparable
                          Cost = nestCost
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
                    let region = state.World.FindRegionAt p
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
                              Coalition = state.World.Countries.[country]
                              Group = region.RegionId :> System.IComparable
                              Cost = nestCost
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
                                    seq { 500.0f .. 250.0f .. 1500.0f }
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
                                          Coalition = state.World.Countries.[country]
                                          Group = region.RegionId :> System.IComparable
                                          Cost = nestCost
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
            for m in groundMissions do
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
                            let countryOwner = state.World.GetAnyCountryInCoalition(owner)
                            let countryInvader = state.World.GetAnyCountryInCoalition(owner.Other)
                            let shape = VectorExtension.mkCircle(Vector2.Zero, 500.0f)
                            let mkNest(p, country : CountryId) =
                                {
                                    Priority = 2.0f
                                    Number = gunsPerNest
                                    Boundary = shape |> List.map ((+) p)
                                    Rotation = 0.0f
                                    Settings = CanonGenerationSettings.Default
                                    Specialty = DefenseSpecialty.AntiAirMg
                                    IncludeSearchLights = hasLowLight
                                    IncludeFlak = true
                                    Country = country.ToMcuValue
                                    Coalition = state.World.Countries.[country]
                                    Group = region.RegionId :> System.IComparable
                                    Cost = nestCost
                                }
                            yield mkNest(posOwner, countryOwner)
                            yield mkNest(posInvader, countryInvader)
                        | None -> ()
                    | None -> ()
                | _ -> ()

            // Camps
            for camp in camps do
                let center = camp.Pos.Pos
                let country = state.World.GetAnyCountryInCoalition(camp.Coalition)
                let region = state.World.FindRegionAt(center)
                let d = 0.7f * 500f + 150.0f
                let x =
                    if random.NextDouble() > 0.5 then
                        center.X + d
                    else
                        center.X - d
                let y =
                    if random.NextDouble() > 0.5 then
                        center.Y + d
                    else
                        center.Y - d
                let location = Vector2(x, y)
                yield
                    {
                        Priority = 1.0f
                        Number = gunsPerNest
                        Boundary = mkCircle(location, 150.0f)
                        Rotation = 0.0f
                        Settings = CanonGenerationSettings.Default
                        Specialty = DefenseSpecialty.AntiAirMg
                        IncludeSearchLights = hasLowLight
                        IncludeFlak = true
                        Country = country.ToMcuValue
                        Coalition = state.World.Countries.[country]
                        Group = region.RegionId :> System.IComparable
                        Cost = nestCost
                    }
        ]

    // Ground battles
    let battles =
        [
            for mission in groundMissions do
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
                        match GroundBattle.TryFromGroundMission(state, mission, p, area, settings.GroundBattleLimits) with
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
                missions
                |> Option.bind (fun missions ->
                    let afPos = state.World.Airfields.[missions.MainMission.StartAirfield].Position
                    let objPos = state.World.Regions.[missions.MainMission.Objective].Position
                    let dir = objPos - afPos
                    let dir = dir / dir.Length()
                    let offset = dir * 15000.0f
                    let targetPos = afPos + offset
                    missions.HomeCover
                    |> Option.bind (fun mission -> AiPatrol.TryFromAirMission(state, mission, targetPos)))
            // Target cover is over the target
            yield
                missions
                |> Option.bind (fun missions ->
                    let targetType =
                        match missions.MainMission.MissionType with
                        | Strafing t | Bombing t -> t
                        | _ -> GroundTargetType.BuildingTarget
                    let targetPos = locator.TryGetGroundTargetLocation(missions.MainMission.Objective, targetType)
                    targetPos
                    |> Option.bind (fun targetPos ->
                        missions.HomeCover
                        |> Option.bind (fun cover -> AiPatrol.TryFromAirMission(state, cover, targetPos))))
            // Interception is 15km away from the target
            yield
                missions
                |> Option.bind (fun missions ->
                    let afPos = state.World.Airfields.[missions.MainMission.StartAirfield].Position
                    let objPos = state.World.Regions.[missions.MainMission.Objective].Position
                    let dir = objPos - afPos
                    let dir = dir / dir.Length()
                    let offset = dir * 15000.0f
                    let targetPos = objPos - offset
                    missions.Interception
                    |> Option.bind (fun interception -> AiPatrol.TryFromAirMission(state, interception, targetPos)))
            // Home attack is 30km from attacker's home airfield
            yield
                missions
                |> Option.bind (fun missions ->
                    let afPos = state.World.Airfields.[missions.MainMission.StartAirfield].Position
                    let objPos = state.World.Regions.[missions.MainMission.Objective].Position
                    let dir = objPos - afPos
                    let dir = dir / dir.Length()
                    let offset = dir * 30000.0f
                    let targetPos = afPos + offset
                    missions.HomeAttack
                    |> Option.bind (fun mission -> AiPatrol.TryFromAirMission(state, mission, targetPos)))
            // Other patrols are over the objective's capital
            for mission in missions |> Option.map (fun m -> m.OtherMissions) |> Option.defaultValue [] do
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
            match missions with
            | Some missions ->
                let mainTargetType =
                    match missions.MainMission.MissionType with
                    | Bombing t | Strafing t -> t
                    | _ -> GroundTargetType.BuildingTarget
                yield
                    locator.TryGetGroundTargetLocation(missions.MainMission.Objective, mainTargetType)
                    |> Option.bind (fun targetPos -> AiAttack.TryFromAirMission(state, missions.MainMission, targetPos))
            | None ->
                ()
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
                    let exposed =
                        strategyMissionData.GetGroup("Airfields").ListOfMCU_Waypoint
                        |> Seq.map (fun wp ->
                            {
                                Pos = OrientedPosition.FromMission wp
                                Radius = float32(wp.GetArea().Value)
                            })
                        |> Seq.filter (fun spot -> spot.Pos.Pos.IsInConvexPolygon airfield.Boundary)
                        |> List.ofSeq
                    let inFacilities =
                        airfield.Facilities
                        |> List.collect (fun ((BuildingInstanceId pos) as bid) ->
                            if state.GetBuildingFunctionalityLevel(bid) > 0.5f then
                                state.World.GetBuildingInstance(bid).Properties.ParkingSpots
                                |> List.map (fun spot ->
                                    { spot with
                                        Pos =
                                            let rot = spot.Pos.Rotation + pos.Rotation
                                            { spot.Pos with
                                                Rotation = rot
                                                Pos = spot.Pos.Pos.Rotate(rot) + pos.Pos
                                                Altitude = pos.Altitude
                                            }
                                    }
                                )
                            else
                                []
                        )
                    let alongRunway =
                        if airfield.IsActive then
                            let runway = airfield.PickAgainstWind(wind, 0.0f)
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
                                                Altitude = runway.SpawnPos.Altitude
                                            }
                                            Radius = 12.5f
                                        }
                                }
                            )
                            |> List.ofSeq
                        else
                            []
                    let country = state.World.GetAnyCountryInCoalition(coalition)
                    let spots =
                        inFacilities @ alongRunway @ exposed
                        |> List.sortByDescending(fun spot -> spot.Radius)
                    yield! assign country (planes, spots)
                | None ->
                    ()
        ]

    // Common to convoys and trains
    let mkTerminalsInRegion (network : Network) =
        network.Nodes
        |> Seq.filter (fun node -> node.HasTerminal)
        |> Seq.choose (fun node -> node.Region |> Option.map (fun region -> region, node))
        |> Seq.groupBy fst
        |> Seq.map (fun (region, nodes) -> region, nodes |> Seq.map snd |> List.ofSeq)
        |> dict

    let getPaths (network : Network) =
        let terminalsInRegion = mkTerminalsInRegion network
        let qa = network.GetQuickAccess()
        [
            for startRegion in state.World.Regions.Values do
                for destRegionId in startRegion.Neighbours do
                    match state.GetOwner(startRegion.RegionId), state.GetOwner(destRegionId) with
                    | Some owner, Some owner2 when owner = owner2 ->
                        match terminalsInRegion.TryGetValue(startRegion.RegionId), terminalsInRegion.TryGetValue(destRegionId) with
                        | (true, starts), (true, dests) ->
                            logger.Debug(sprintf "Considering path between %s and %s" (string startRegion.RegionId) (string destRegionId))
                            match state.TryFindPath(network, starts, dests, Some owner) with
                            | Some links ->
                                let path =
                                    links
                                    |> Seq.map (fun link ->
                                        let pos = qa.GetNode(link.NodeA).Pos
                                        let dir = qa.GetNode(link.NodeB).Pos - pos
                                        {
                                            Pos = pos
                                            Rotation = dir.YOri
                                            Altitude = 0.0f
                                        }
                                    )
                                    |> List.ofSeq
                                logger.Debug("Found path")
                                yield {| Country = state.World.GetAnyCountryInCoalition(owner); Path = path |}
                            | None ->
                                logger.Debug("No path found")
                                ()
                        | _ -> ()
                    | _ -> ()
        ]

    let organizeConvoys limit (convoys : Convoy list) =
        convoys
        |> List.groupBy (fun convoy -> state.World.Countries.[convoy.Country])
        |> List.map (fun (coalition, convoys) ->
            convoys
            |> Array.ofList
            |> Array.shuffle (System.Random(state.Seed))
            |> List.ofArray
            |> List.truncate limit)

    // Trains
    logger.Debug("Starting to prepare train convoys")
    let trains =
        [
            for x in getPaths state.World.Rails do
                yield {
                    Country = x.Country
                    Coalition = state.World.Countries.[x.Country]
                    Members = [ ConvoyMember.Train ]
                    Path = x.Path
                    StartPositions = [ x.Path.Head ]
                }
        ]
        |> organizeConvoys settings.MaxTrainsPerSide
    logger.Debug(sprintf "Generated a total of %d trains" (trains |> List.sumBy List.length))

    // Trucks
    logger.Debug("Starting to prepare truck convoys")
    let trucks =
        [
            let rnd = System.Random(state.Seed)
            for x in getPaths state.World.Roads do
                let convoy =
                    List.init 2 (fun _ ->
                        AntiAirTruck :: List.init 3 (fun _ -> Truck))
                    |> List.concat
                    |> Array.ofList
                    |> Array.shuffle rnd
                    |> List.ofArray
                    |> List.truncate (x.Path.Length - 1)
                yield {
                    Country = x.Country
                    Coalition = state.World.Countries.[x.Country]
                    Members = convoy
                    Path = x.Path
                    StartPositions = x.Path |> List.truncate (convoy.Length + 1) |> List.rev
                }
        ]
        |> organizeConvoys settings.MaxTrainsPerSide
    logger.Debug(sprintf "Generated a total of %d truck convoys" (trucks |> List.sumBy List.length))

    // Fires
    let damagedBuildings =
        state.World.Buildings.Values
        |> Seq.choose (fun b ->
            let funct = state.GetBuildingFunctionalityLevel(b.Id)
            if funct < 0.5f then
                Some(b, funct)
            else
                None)
        |> Seq.sortBy snd

    let spacedOutBuildings =
        ([], damagedBuildings)
        ||> Seq.fold (fun xs (b, funct) ->
            if xs.Length >= 100 then
                xs
            elif xs |> List.exists (fun (b2 : BuildingInstance, _) -> (b.Pos.Pos - b2.Pos.Pos).Length() < 10000.0f) then
                xs
            else
                (b, funct) :: xs
        )

    let fires : BuildingFire list =
        spacedOutBuildings
        |> List.map (fun (b, funct) ->
            {
                Pos = { b.Pos with Rotation = float32 state.Weather.Wind.Direction }
                Intensity =
                    let x = (1.0f - funct) * float32 b.Properties.Durability
                    if x >= 20000.0f then
                        SturmovikMission.Blocks.FireLoop.CityFire
                    elif x >= 10000.0f then
                        SturmovikMission.Blocks.FireLoop.CityFireSmall
                    else
                        SturmovikMission.Blocks.FireLoop.VillageSmoke
            }
        )

    // Result
    {
        Date = state.Date
        Briefing = state.Date.ToString("d MMM yyyy HH:mm") + "<br>" + state.Weather.Description + "<br>" + briefing
        Boundary = boundary
        PlayerSpawns = spawns
        AntiAirNests = aaNests
        GroundBattles = battles
        AiPatrols = patrols
        AiAttacks = attacks
        Convoys = trains @ trucks
        ParkedPlanes = parkedPlanes
        Camps = camps
        BuildingFires = fires
    }

