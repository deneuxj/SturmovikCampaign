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

namespace Campaign.MissionGenerator

open System.Numerics
open VectorExtension

open Campaign.BasicTypes
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.Missions
open Campaign.PlaneModel

open Util

type AirfieldId = Campaign.WorldDescription.AirfieldId

/// A step in the sequence of rounds making up a campaign.
type ScenarioStep =
    | Stalemate of string
    | Victory of CoalitionId * string
    | Ongoing of StepData

/// Content of a round, and a generator function to produce the next step, given the updated state of the war.
and StepData =
    {
        Briefing : string
        Missions : Mission list
        Next : WarState -> ScenarioStep
    }

/// Resources available at an airfield.
/// Decreased whenever a flight is planned taking off from that airfield
type AirfieldStatus =
    {
        Resources : float32<M^3>
        Planes : Map<PlaneModelId, float32>
    }
with
    member this.TryCheckout((plane, resourcesPerPlane : float32<M^3>, numPlanes)) =
        let qty = this.Planes.TryFind(plane) |> Option.defaultValue 0.0f
        let qty = qty - numPlanes
        let rsc = this.Resources - resourcesPerPlane * numPlanes
        if qty >= 0.0f && rsc >= 0.0f<M^3> then
            Some {
                this with
                    Resources = rsc
                    Planes = this.Planes.Add(plane, qty)
            }
        else
            None

/// Resources available at airfields
type Airfields =
    {
        Airfields : Map<AirfieldId, AirfieldStatus>
    }
with
    static member Create(war : WarState) =
        {
            Airfields =
                war.World.Airfields.Keys
                |> Seq.map (fun afid ->
                    afid,
                    { Resources = war.GetAirfieldCapacity(afid); Planes = war.GetNumPlanes(afid) } )
                |> Map.ofSeq
        }

    member this.TryCheckout(afid, data) =
        let afs =
            this.Airfields.[afid]
        let afs = afs.TryCheckout(data)
        match afs with
        | Some afs ->
            Some {
                this with
                    Airfields = this.Airfields.Add(afid, afs)
            }
        | None ->
            None

type TargetAdapter<'Target> =
    abstract GetPos : 'Target -> Vector2
    abstract MkGroundTarget : 'Target -> GroundTargetType
    abstract GetRegion : 'Target -> Campaign.WorldDescription.RegionId

type AirfieldTargetAdapter() =
    interface TargetAdapter<Airfield> with
        member __.GetPos(af) = af.Position
        member __.MkGroundTarget(af) = AirfieldTarget af.AirfieldId
        member __.GetRegion(af) = af.Region

type RegionTargetAdapter() =
    interface TargetAdapter<Region> with
        member __.GetPos(region) = region.Position
        member __.MkGroundTarget(region) = BuildingTarget
        member __.GetRegion(region) = region.RegionId

type GroundForcesTargetAdapter(side) =
    interface TargetAdapter<Region> with
        member __.GetPos(region) = region.Position
        member __.MkGroundTarget(region) = GroundForces side
        member __.GetRegion(region) = region.RegionId

type MissionPlanningResult =
    | TooFewTargets of string
    | Plan of string * Mission list * Airfields
with
    member this.LacksResources =
        match this with
        | TooFewTargets _ | Plan(_, _ :: _, _) -> false
        | Plan(_, [], _) -> true

module MissionPlanningResult =
    let getMissions originalBudget =
        function
        | TooFewTargets _ -> [], originalBudget
        | Plan(_, missions, budget) -> missions, budget

    let hasMissions =
        function
        | TooFewTargets _ | Plan(_, [],_) -> false
        | Plan(_, _ :: _, _) -> true

    let firstWithNonEmptyPlan (xs : MissionPlanningResult seq) =
        let proj =
            function
            | Plan(_, [], _) -> 0
            | TooFewTargets _ -> 1
            | Plan(_, _ :: _, _) -> 2
        let pred =
            function
            | Plan(_, _ :: _, _) -> true
            | _ -> false
        xs
        |> Seq.maxByUntil proj pred

module Planning =
    let chain planners =
        fun budget ->
            planners
            |> Seq.fold (fun (missions, budget) planner ->
                let plan = planner budget
                let missions2, budget = MissionPlanningResult.getMissions budget plan
                (missions @ missions2, budget)
            ) ([], budget)
            |> fun (m, b) -> Plan("Chain of missions", m, b)

    let andThen also first =
        fun budget ->
            match first budget with
            | Plan(description, ((_ :: _) as missions), budget) ->
                let missions2, budget =
                    (chain also) budget
                    |> MissionPlanningResult.getMissions budget
                Plan(description, missions @ missions2, budget)
            | nogo -> nogo

    let orElse alternatives =
        fun budget ->
            alternatives
            |> Seq.map (fun f -> f budget)
            |> MissionPlanningResult.firstWithNonEmptyPlan

module Bodenplatte =
    let totalPlanes : Map<_, float32> -> float32 =
        Map.toSeq >> Seq.sumBy (snd >> floor)

    let typicalRange = 1.0e5f<M>
    let planeRunCost = 0.2f<M^3> / typicalRange
    let bombDensity = 250.0f<K/M^3>

    type Campaign.Missions.AirMission with
        member this.CheckoutData(world : World) =
            let distance =
                1.0f<M> * (world.Regions.[this.Objective].Position - world.Airfields.[this.StartAirfield].Position).Length()
            let bombs =
                match this.MissionType with
                | GroundTargetAttack _ -> world.PlaneSet.[this.Plane].BombCapacity / bombDensity
                | AreaProtection -> 0.0f<M^3>
                | PlaneTransfer _ -> 0.0f<M^3>
            (this.Plane, distance * planeRunCost + bombs, float32 this.NumPlanes)

    let maxPlanesAtAirfield = 100.0f

    let minActiveAirfieldResources = planeRunCost * 10.0f * typicalRange

    let minRegionBuildingCapacity = 1000.0f<M^3>

    let idMe262 = PlaneModelId "me262"
    let idBf109g14 = PlaneModelId "bf109g14"
    let idFw190a8 = PlaneModelId "fw190a8"
    let idBf109k4 = PlaneModelId "bf109k4"
    let idFw190d9 = PlaneModelId "fw190d9"

    let idP51 = PlaneModelId "p51"
    let idP47 = PlaneModelId "p47"
    let idSpitfire = PlaneModelId "spitfireMkIXe"
    let idTempest = PlaneModelId "Tempest MkV s2"
    let idP38 = PlaneModelId "p38"

    /// Get the list of attackers of a coalition, preferred ones first
    let attackersOf =
        function
        | Axis -> [ idMe262; idFw190d9; idFw190a8; idBf109k4; idBf109g14]
        | Allies -> [ idP38; idP47; idP51 ]

    /// Get the list of interceptors of a coalition, preferred ones first
    let interceptorsOf =
        function
        | Axis -> [ idFw190d9; idBf109k4; idFw190a8; idBf109g14 ]
        | Allies -> [ idP38; idP51; idTempest; idP47; idSpitfire ]

    /// Get the list of fighters of a coalition, preferred ones first
    let fightersOf =
        function
        | Axis -> [ idBf109g14; idFw190a8; idBf109k4; idFw190d9 ]
        | Allies -> [ idP51; idSpitfire; idTempest; idP47; idP38 ]

    let allPlanesOf coalition =
        [attackersOf; interceptorsOf; fightersOf]
        |> List.collect (fun f -> f coalition)
        |> List.distinct

    /// Get the total number of planes of give types at an airfield
    let sumPlanes (atAirfield : Map<PlaneModelId, float32>) (planes : PlaneModelId seq) =
        planes
        |> Seq.fold (fun s plane ->
            atAirfield.TryFind(plane)
            |> Option.defaultValue 0.0f
            |> (+) s) 0.0f

    let airfieldPos (af : Airfield) =
        af.Boundary.Head

    /// Set the number of planes at each airfield controlled by a coalition.
    let initAirfields (factor : float32) (friendly : CoalitionId) (war : WarState) =
        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy

        let planes =
            allPlanesOf friendly
            |> Seq.map (fun planeId -> war.World.PlaneSet.[planeId])
            |> Seq.filter (fun plane -> plane.Kind <> PlaneType.Transport)
            |> Seq.sortByDescending (fun plane -> plane.Cost)
            |> List.ofSeq

        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> war.GetOwner(af.Region) = Some friendly)
            |> Seq.sortBy (fun af -> distanceToEnemy.[af.Region])
            |> List.ofSeq

        // Non-transport planes: Set according to airfield resources and respective cost
        let planeRunCost = typicalRange * planeRunCost
        match planes with
        | [] -> ()
        | [plane] ->
            for af in airfields do
                let numPlanes =
                    war.GetAirfieldCapacity(af.AirfieldId) / planeRunCost
                war.SetNumPlanes(af.AirfieldId, plane.Id, factor * numPlanes)
        | expansive :: regular ->
            // Rear airfields get regular planes and the expansive plane
            // Front airfields get regular planes and the cheap plane
            let regular = List.rev regular
            let cheap, regular = List.head regular, List.tail regular
            let numAirfields = List.length airfields
            let numFrontAirfields = numAirfields / 2 |> max 1
            let numRearAirfields = numAirfields - numFrontAirfields |> max 1
            airfields
            |> List.iteri (fun i af ->
                let planes =
                    regular
                let planes =
                    if i < numFrontAirfields then
                        cheap :: planes
                    else
                        planes
                let planes =
                    if i >= numAirfields - numRearAirfields then
                        expansive :: planes
                    else
                        planes
                let totalInvCost =
                    planes
                    |> List.sumBy (fun plane -> 1.0f / plane.Cost)
                let numPlanes =
                    war.GetAirfieldCapacity(af.AirfieldId) / planeRunCost
                    |> min maxPlanesAtAirfield
                for plane in planes do
                    let qty = numPlanes * (1.0f / plane.Cost) / totalInvCost |> max 1.0f
                    war.SetNumPlanes(af.AirfieldId, plane.Id, factor * qty)
                )

        // Transport planes at the rearmost airfield
        match List.tryLast airfields with
        | Some rear ->
            let transport =
                allPlanesOf friendly
                |> List.filter (fun planeId -> war.World.PlaneSet.[planeId].Kind = PlaneType.Transport)
            for plane in transport do
                war.SetNumPlanes(rear.AirfieldId, plane, 20.0f)
        | None ->
            ()

    /// Try to make missions to attack generic enemy ground targets, with sufficient cover over target, and over home airfield
    let tryMakeAirRaids (war : WarState) (adapter : TargetAdapter<'Target>) (description : string) (raidTargets : 'Target list) (friendly : CoalitionId) (budget : Airfields)  =
        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy

        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> war.GetOwner(af.Region) = Some friendly)
            |> Seq.sortBy (fun af -> distanceToEnemy.[af.Region])
            |> List.ofSeq

        let readyAirfields =
            airfields
            |> List.filter (fun af ->
                war.GetGroundForces(enemy, af.Region) = 0.0f<MGF> &&
                war.GetAirfieldCapacity(af.AirfieldId) >= minActiveAirfieldResources &&
                totalPlanes(war.GetNumPlanes(af.AirfieldId)) >= 5.0f)

        match readyAirfields, raidTargets with
        | [], _ -> Plan("No planes for " + description, [], budget)
        | _, [] ->
            TooFewTargets description
        | _ :: _, _ :: _ ->
            let raids =
                let isInPlaneRange (plane : PlaneModelId) (start : Airfield) (target : Vector2) =
                    let maxRange = war.World.PlaneSet.[plane].MaxRange
                    (start.Position - target).Length() * 1.0f<M> < maxRange
                // For each attack plane type, and for each potential target airfield,
                // Try to plan an attack flight, a target cover flight with fighters,
                // and a home cover flight with intercepters.
                // Prioritized solutions to maximize number of attack planes, then number of fighters/interceptors,
                // then fighter plane model, and finally interceptor plane model.
                [
                    let mutable budget = budget
                    for target in raidTargets do
                        let targetRegion = adapter.GetRegion target
                        let targetPos = adapter.GetPos target
                        let missionAlternatives (budget : Airfields) mkMissionFrom destination =
                            readyAirfields 
                            |> Seq.choose (fun af ->
                                let mission : AirMission = mkMissionFrom af
                                match budget.TryCheckout(af.AirfieldId, mission.CheckoutData war.World) with
                                | None -> None
                                | Some budget ->
                                    if isInPlaneRange mission.Plane af destination then
                                        Some(mission, budget)
                                    else
                                        None)
                        let groupAndBudget =
                            seq {
                                for attacker in attackersOf friendly do
                                for numAttackers in [15; 10; 5] do
                                let mkAttackerMission af =
                                    { StartAirfield = af.AirfieldId
                                      Objective = targetRegion
                                      MissionType = GroundTargetAttack(adapter.MkGroundTarget target, LowAltitude)
                                      Plane = attacker
                                      NumPlanes = numAttackers
                                    }
                                for attackerMission, budget in missionAlternatives budget mkAttackerMission targetPos do
                                let attackerStart = war.World.Airfields.[attackerMission.StartAirfield]
                                for numFighters in [8; 4; 2] do
                                for fighter in fightersOf friendly do
                                let mkFighterMission af =
                                    { StartAirfield = af.AirfieldId
                                      Objective = targetRegion
                                      MissionType = AreaProtection
                                      Plane = fighter
                                      NumPlanes = numFighters }
                                for fighterMission, budget in missionAlternatives budget mkFighterMission targetPos do
                                for numInterceptors in [4; 3; 2] do
                                for interceptor in interceptorsOf friendly do
                                let mkInterceptorMission af =
                                    { StartAirfield = af.AirfieldId
                                      Objective = attackerStart.Region
                                      MissionType = AreaProtection
                                      Plane = interceptor
                                      NumPlanes = numInterceptors }
                                for interceptorMission, budget in missionAlternatives budget mkInterceptorMission attackerStart.Position do
                                    yield [
                                        attackerMission, description
                                        fighterMission, "Target cover"
                                        interceptorMission, "Home base cover"
                                    ], budget
                            }
                            |> Seq.tryHead
                        match groupAndBudget with
                        | Some(group, x) ->
                            budget <- x
                            yield! group
                        | None ->
                            ()
                ]
                |> List.map (fun (m, description) ->
                    { 
                        Kind = AirMission m
                        Description =
                            sprintf "%s: %d %s from %s to %s"
                                description
                                m.NumPlanes
                                (string m.Plane)
                                m.StartAirfield.AirfieldName
                                (string m.Objective)
                    })
            match raids with
            | [] ->
                Plan ("No planes in range for " + description, [], budget)
            | _::_ ->
                // Compute again resource consumption by missions on each start airfield
                let budget =
                    raids
                    |> Seq.fold (fun budget mission ->
                        match mission with
                        | { Kind = AirMission mission } ->
                            budget
                            |> Option.bind (fun (budget : Airfields) ->
                                budget.TryCheckout(mission.StartAirfield, mission.CheckoutData war.World))
                        | _ -> budget
                    ) (Some budget)
                Plan (description, raids, Option.get budget)

    /// Try to make missions to attack enemy airfields, with sufficient cover over target, and over home airfield
    let tryMakeAirfieldRaids (war : WarState) (friendly : CoalitionId) (budget : Airfields)  =
        let distanceToFriendly = war.ComputeDistancesToCoalition friendly
        let enemy = friendly.Other

        let enemyAirfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> war.GetOwner(af.Region) = Some enemy)
            |> Seq.sortBy (fun af -> distanceToFriendly.[af.Region])
            |> List.ofSeq

        let raidTargets =
            enemyAirfields
            |> List.filter (fun af ->
                let numPlanes = totalPlanes(war.GetNumPlanes(af.AirfieldId))
                let resources = war.GetAirfieldCapacity(af.AirfieldId)
                numPlanes >= 1.0f && resources >= minActiveAirfieldResources)

        tryMakeAirRaids war (AirfieldTargetAdapter()) "Airfield strike" raidTargets friendly budget

    /// Try to make missions to attack enemy industry, with cover over target, and over home airfield
    let tryMakeIndustryRaids (war : WarState) (friendly : CoalitionId) (budget : Airfields) =
        let distanceToFriendly = war.ComputeDistancesToCoalition friendly
        let enemy = friendly.Other

        let enemyRegions =
            war.World.Regions.Values
            |> Seq.filter (fun region ->
                war.GetOwner(region.RegionId) = Some enemy &&
                distanceToFriendly.[region.RegionId] > 1 &&
                war.GetRegionBuildingCapacity(region.RegionId) > minRegionBuildingCapacity)
            |> Seq.sortByDescending (fun region -> distanceToFriendly.[region.RegionId])
            |> List.ofSeq

        tryMakeAirRaids war (RegionTargetAdapter()) "Industry raid" enemyRegions friendly budget

    /// Try to plan missions to defend ground forces in friendly territory.
    let tryMakeDefensiveGroundForcesRaids (war : WarState) (friendly : CoalitionId) (budget : Airfields) =
        let threatenedRegions =
            war.World.Regions.Values
            |> Seq.filter (fun region ->
                war.GetOwner(region.RegionId) = Some friendly &&
                war.GetGroundForces(friendly, region.RegionId) < 5.0f * war.GetGroundForces(friendly.Other, region.RegionId))
            |> Seq.sortByDescending (fun region ->
                war.GetGroundForces(friendly.Other, region.RegionId) / war.GetGroundForces(friendly, region.RegionId),
                war.GetGroundForces(friendly.Other, region.RegionId))
            |> List.ofSeq

        match threatenedRegions with
        | [] ->
            Plan("No regions need defensive air support", [], budget)
        | _ :: _ ->
            tryMakeAirRaids war (GroundForcesTargetAdapter(friendly.Other)) "Close air support" threatenedRegions friendly budget

    /// Try to plan missions to attack ground forces in enemy territory
    let tryMakeOffensiveGroundForcesRaids (war : WarState) (onlySupport : bool) (friendly : CoalitionId) (budget : Airfields) =
        let weakRegions =
            war.World.Regions.Values
            |> Seq.filter (fun region ->
                war.GetOwner(region.RegionId) = Some friendly.Other &&
                war.GetGroundForces(friendly.Other, region.RegionId) > 0.0f<MGF> &&
                (not onlySupport || war.GetGroundForces(friendly, region.RegionId) > 0.0f<MGF>))
            |> Seq.sortByDescending (fun region ->
                war.GetGroundForces(friendly.Other, region.RegionId) / if onlySupport then war.GetGroundForces(friendly, region.RegionId) else 1.0f<MGF>)
            |> List.ofSeq

        match onlySupport, weakRegions with
        | true, [] ->
            Plan("No invasion to cover", [], budget)
        | false, [] ->
            TooFewTargets "Enemy ground troops destroyed"
        | _, _::_ ->
            tryMakeAirRaids war (GroundForcesTargetAdapter(friendly.Other)) "Close air support" weakRegions friendly budget

    /// Try to make CAP missions over regions targetted by enemy air missions.
    let tryMakeCovers (war : WarState) (friendly : CoalitionId) (enemyMissions : AirMission list) (budget : Airfields) =
        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy

        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> war.GetOwner(af.Region) = Some friendly)
            |> Seq.sortBy (fun af -> distanceToEnemy.[af.Region])
            |> List.ofSeq

        let readyAirfields =
            airfields
            |> List.filter (fun af ->
                war.GetGroundForces(enemy, af.Region) = 0.0f<MGF> &&
                war.GetAirfieldCapacity(af.AirfieldId) >= minActiveAirfieldResources &&
                totalPlanes(war.GetNumPlanes(af.AirfieldId)) >= 2.0f)

        let targettedRegions =
            enemyMissions
            |> List.map (fun mission -> mission.Objective)
            |> Set.ofList
            |> List.ofSeq

        match readyAirfields, targettedRegions with
        | [], _ -> Plan("No planes for air defense", [], budget)
        | _, [] -> Plan("No enemy plane incursions", [], budget)
        | _ :: _, _ :: _ ->
            let mutable budget = budget
            let missions =
                [
                    for target in targettedRegions do
                        let mission =
                            let planes = interceptorsOf friendly @ fightersOf friendly
                            let numbers = [8; 4; 2]
                            readyAirfields
                            |> Seq.allPairs planes
                            |> Seq.allPairs numbers
                            |> Seq.tryPick (fun (planes, (plane, start)) ->
                                let plane = war.World.PlaneSet.[plane]
                                let distance =
                                    (war.World.Regions.[target].Position - start.Boundary.Head).Length() * 1.0f<M>
                                if distance <= plane.MaxRange then
                                    let mission =
                                        {
                                            StartAirfield = start.AirfieldId
                                            Objective = target
                                            MissionType = AreaProtection
                                            NumPlanes = planes
                                            Plane = plane.Id
                                        }
                                    let budget2 =
                                        budget.TryCheckout(start.AirfieldId, mission.CheckoutData war.World)
                                    match budget2 with
                                    | Some budget2 ->
                                        budget <- budget2
                                        Some mission
                                    | None ->
                                        None
                                else
                                    None
                            )
                        yield! Option.toList mission
                ]
                |> List.map (fun m ->
                    { Kind = AirMission m
                      Description = "Combat air patrol" })
            Plan("Air defense", missions, budget)

    /// Try to send planes closer to the frontline.
    let tryTransferPlanesForward (war : WarState) (friendly : CoalitionId) (budget : Airfields) =
        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> war.GetOwner(af.Region) = Some friendly)
            |> List.ofSeq

        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy
        let planeRunCost = typicalRange * planeRunCost + 500.0f<K>/bombDensity
        let mutable budget = budget
        let missions =
            [
                let forward =
                    Seq.allPairs airfields airfields
                    |> Seq.where (fun (af1, af2) -> distanceToEnemy.[af1.Region] > distanceToEnemy.[af2.Region])
                for af1, af2 in forward do
                    let mutable excessPlanes =
                        let minAmount =
                            if distanceToEnemy.[af1.Region] > 4 then
                                5.0f
                            else
                                15.0f
                        let afid = af1.AirfieldId
                        let af = budget.Airfields.[afid]
                        totalPlanes af.Planes - minAmount
                    let mutable sustainable =
                        let afid = af2.AirfieldId
                        let af = budget.Airfields.[afid]
                        (af.Resources - totalPlanes af.Planes * planeRunCost) / planeRunCost
                    let allButTransport =
                        allPlanesOf friendly
                        |> List.filter (fun planeId -> war.World.PlaneSet.[planeId].Kind <> PlaneType.Transport)
                    for plane in allButTransport do
                        if excessPlanes > 0.0f && sustainable > 0.0f then
                            let numPlanes =
                                sumPlanes budget.Airfields.[af1.AirfieldId].Planes [plane]
                                |> min excessPlanes
                                |> int
                                |> min 25
                            if numPlanes > 0 then
                                let mission =
                                    {
                                        StartAirfield = af1.AirfieldId
                                        Objective = af2.Region
                                        MissionType = PlaneTransfer af2.AirfieldId
                                        NumPlanes = numPlanes
                                        Plane = plane
                                    }
                                let budget2 = budget.TryCheckout(af1.AirfieldId, mission.CheckoutData war.World)
                                match budget2 with
                                | Some b ->
                                    budget <- b
                                    let numPlanesF = (float32 numPlanes)
                                    excessPlanes <- excessPlanes - numPlanesF
                                    sustainable <- sustainable - numPlanesF
                                    yield mission, sprintf "Transfer %d %s from %s to %s" numPlanes (string plane) af1.AirfieldId.AirfieldName af2.AirfieldId.AirfieldName
                                | None ->
                                    ()
            ]
            |> List.map (fun (m, description) ->
                { Kind = AirMission m
                  Description = description })
        Plan ("Transfers", missions, budget)

    let rec oneSideStrikes (side : CoalitionId) comment depth (war : WarState) =
        let tryMakeAirfieldRaids = tryMakeAirfieldRaids war side
        let tryMakeIndustryRaids = tryMakeIndustryRaids war side
        let tryMakeOtherDefensiveGroundForcesRaids = tryMakeDefensiveGroundForcesRaids war side.Other
        let tryMakeGroundForcesHarassment = tryMakeOffensiveGroundForcesRaids war false side
        let tryMakeGroundForcesSupport = tryMakeOffensiveGroundForcesRaids war true side
        let tryMakeGroundForcesDefense = tryMakeDefensiveGroundForcesRaids war side
        let tryTransferPlanesForward = tryTransferPlanesForward war side

        let sideAttacks =
            Planning.orElse [
                tryMakeAirfieldRaids |> Planning.andThen [ tryMakeGroundForcesDefense; tryMakeGroundForcesSupport; tryMakeIndustryRaids; tryMakeGroundForcesHarassment ]
                tryMakeIndustryRaids |> Planning.andThen [tryMakeGroundForcesDefense; tryMakeGroundForcesSupport; tryMakeGroundForcesHarassment ]
                tryMakeGroundForcesHarassment |> Planning.andThen [ tryMakeGroundForcesSupport ]
            ] |> Planning.andThen [tryTransferPlanesForward]

        let budget = Airfields.Create war
        match sideAttacks budget with
        | TooFewTargets comment ->
            Victory(side, comment)
        | Plan(comment, [], _) ->
            let comment2 = sprintf "%s loses initiative because of %s" (string side) comment
            if depth > 0 then
                oneSideStrikes side.Other comment2 (depth - 1) war
            else
                Stalemate (sprintf "%s and %s" comment comment2)
        | Plan(description, missions, budget) ->
            let raids =
                missions
                |> List.choose (function { Kind = AirMission x } -> Some x | _ -> None)
            let covers, budget =
                Planning.chain [ tryMakeCovers war side.Other raids; tryMakeOtherDefensiveGroundForcesRaids; tryTransferPlanesForward ] budget
                |> MissionPlanningResult.getMissions budget
            let momentum =
                if depth = 0 then
                    "keeping the pressure on"
                else
                    "striking against"
            Ongoing {
                Briefing = sprintf "%s. %s is %s %s assets on the ground with %s" comment (string side) momentum (string side.Other) description
                Missions = missions @ covers
                Next = oneSideStrikes side.Other (sprintf "%s takes the initiative" (string side.Other)) 1
            }

    let start war = oneSideStrikes Axis "Axis opens the hostilities" 1 war