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
    | Stalemate
    | Victory of CoalitionId
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

type MissionPlanningResult =
    | TooFewTargets
    | Plan of Mission list * Airfields
with
    member this.LacksResources =
        match this with
        | TooFewTargets | Plan(_ :: _, _) -> false
        | Plan([], _) -> true

module MissionPlanningResult =
    let getMissions originalBudget =
        function
        | TooFewTargets -> [], originalBudget
        | Plan(missions, budget) -> missions, budget

module Bodenplatte =
    let totalPlanes : Map<_, float32> -> float32 =
        Map.toSeq >> Seq.sumBy snd

    let planeRunCost = 10.0f<M^3>

    type Campaign.Missions.AirMission with
        member this.CheckoutData =
            (this.Plane, planeRunCost, float32 this.NumPlanes)

    let maxPlanesAtAirfield = 100.0f

    let minActiveAirfieldResources = planeRunCost * 10.0f

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
        | Axis -> [ idMe262 ]
        | Allies -> [ idP38; idP47]

    /// Get the list of interceptors of a coalition, preferred ones first
    let interceptorsOf =
        function
        | Axis -> [ idFw190d9; idBf109k4 ]
        | Allies -> [ idP38; idP51; idP47 ]

    /// Get the list of fighters of a coalition, preferred ones first
    let fightersOf =
        function
        | Axis -> [ idBf109g14; idFw190a8 ]
        | Allies -> [ idP51; idSpitfire; idTempest ]

    let allPlanesOf coalition =
        [attackersOf; interceptorsOf; fightersOf]
        |> List.collect (fun f -> f coalition)

    /// Get the total number of planes of give types at an airfield
    let sumPlanes (atAirfield : Map<PlaneModelId, float32>) (planes : PlaneModelId seq) =
        planes
        |> Seq.fold (fun s plane ->
            atAirfield.TryFind(plane)
            |> Option.defaultValue 0.0f
            |> (+) s) 0.0f

    /// Set the number of planes at each airfield controlled by a coalition.
    let initAirfields (friendly : CoalitionId) (war : WarState) =
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
        match planes with
        | [] -> ()
        | [plane] ->
            for af in airfields do
                let numPlanes =
                    war.GetAirfieldCapacity(af.AirfieldId) / planeRunCost
                war.SetNumPlanes(af.AirfieldId, plane.Id, numPlanes)
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
                let totalCost =
                    planes
                    |> List.sumBy (fun plane -> plane.Cost)
                let numPlanes =
                    war.GetAirfieldCapacity(af.AirfieldId) / planeRunCost
                    |> min maxPlanesAtAirfield
                for plane in planes do
                    let qty = numPlanes * plane.Cost / totalCost |> max 1.0f
                    war.SetNumPlanes(af.AirfieldId, plane.Id, qty)
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

    /// Try to make missions to attack enemy airfields, with sufficient cover over target, and over home airfield
    let tryMakeAirRaids (friendly : CoalitionId) (budget : Airfields) (war : WarState) =
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
                war.GetAirfieldCapacity(af.AirfieldId) >= minActiveAirfieldResources)

        let distanceToFriendly = war.ComputeDistancesToCoalition friendly

        let enemyAirfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> war.GetOwner(af.Region) = Some enemy)
            |> Seq.sortBy (fun af -> distanceToFriendly.[af.Region])
            |> List.ofSeq

        let raidTargets =
            enemyAirfields
            |> List.filter (fun af ->
                let numPlanes =
                    totalPlanes(war.GetNumPlanes(af.AirfieldId))
                let resources = war.GetAirfieldCapacity(af.AirfieldId)
                numPlanes >= 10.0f || resources >= minActiveAirfieldResources)

        match raidTargets with
        | [] ->
            TooFewTargets
        | _ :: _ ->
            let raids =
                /// Check that distances between start airfields and objectives are within range,
                /// and then return the missions of which a raid is composed:
                /// An airfield attack, CAP over target, CAP over attackers' home base
                let planOneRaid (target : Airfield) attackers targetCover homeCover =
                    let getAirfieldsDistance (af1 : Airfield) (af2 : Airfield) =
                        (af1.Boundary.Head - af2.Boundary.Head).Length() * 1.0f<M>
                    let canFlyToTarget (af, (plane : PlaneModel, _, home)) =
                        getAirfieldsDistance af home <= plane.MaxRange
                    let planeOf (plane, _, _) = plane
                    let numPlanesOf (_, num, _) = num
                    let airfieldOf (_, _, af) = af
                    if [(target, attackers); (target, targetCover); (airfieldOf attackers, homeCover)] |> List.forall canFlyToTarget then
                        [
                            {
                                StartAirfield = (airfieldOf attackers).AirfieldId
                                Objective = target.Region
                                MissionType =
                                    GroundTargetAttack(
                                        AirfieldTarget target.AirfieldId,
                                        LowAltitude
                                    )
                                NumPlanes = numPlanesOf attackers
                                Plane = (planeOf attackers).Id
                            }
                            {
                                StartAirfield = (airfieldOf targetCover).AirfieldId
                                Objective = target.Region
                                MissionType = AreaProtection
                                NumPlanes = numPlanesOf targetCover
                                Plane = (planeOf targetCover).Id
                            }
                            {
                                StartAirfield = (airfieldOf homeCover).AirfieldId
                                Objective = target.Region
                                MissionType = AreaProtection
                                NumPlanes = numPlanesOf homeCover
                                Plane = (planeOf homeCover).Id
                            }
                        ]
                    else
                        []
                let mutable budget = budget
                // For each attack plane type, and for each potential target airfield,
                // Try to plan an attack flight, a target cover flight with fighters,
                // and a home cover flight with intercepters.
                // Prioritized solutions to maximize number of attack planes, then number of fighters/interceptors,
                // then fighter plane model, and finally interceptor plane model.
                [
                    for attacker in attackersOf friendly do
                        for target in raidTargets do
                            let missions =
                                interceptorsOf friendly
                                |> Seq.allPairs (fightersOf friendly)
                                |> Seq.allPairs [8; 4; 2]
                                |> Seq.allPairs [10; 5]
                                |> Seq.allPairs readyAirfields
                                |> Seq.where (fun (start, (numAttackers, _)) ->
                                    sumPlanes budget.Airfields.[start.AirfieldId].Planes [attacker] >= float32 numAttackers)
                                |> Seq.allPairs readyAirfields
                                |> Seq.where (fun (start, (_, (_, (numFighters, (fighter, _))))) ->
                                    sumPlanes budget.Airfields.[start.AirfieldId].Planes [fighter] >= float32 numFighters)
                                |> Seq.allPairs readyAirfields
                                |> Seq.where (fun (start, (_, (_, (_, (numFighters, (_, interceptor)))))) ->
                                    sumPlanes budget.Airfields.[start.AirfieldId].Planes [interceptor] >= float32 numFighters)
                                |> Seq.tryPick (fun (start1, (start2, (start3, (numAttackers, (numFighters, (fighter, interceptor)))))) ->
                                    let attackers = (war.World.PlaneSet.[attacker], numAttackers, start3)
                                    let targetCover = (war.World.PlaneSet.[fighter], numFighters, start2)
                                    let homeCover = (war.World.PlaneSet.[interceptor], numFighters, start1)
                                    match planOneRaid target attackers targetCover homeCover with
                                    | [] -> None
                                    | missions ->
                                        let remaining =
                                            missions
                                            |> List.fold (fun budget mission ->
                                                budget
                                                |> Option.bind (fun (budget : Airfields) ->
                                                    budget.TryCheckout(mission.StartAirfield, mission.CheckoutData))
                                            ) (Some budget)
                                        match remaining with
                                        | Some x ->
                                            budget <- x
                                            Some missions
                                        | None ->
                                            None)
                                |> Option.defaultValue []
                            yield! missions
                ]
                |> List.map AirMission
            Plan (raids, budget)

    /// Try to make CAP missions over regions targetted by enemy air missions.
    let tryMakeCovers (friendly : CoalitionId) (budget : Airfields) (war : WarState) (enemyMissions : AirMission list) =
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
                war.GetAirfieldCapacity(af.AirfieldId) >= minActiveAirfieldResources)

        let targettedRegions =
            enemyMissions
            |> List.map (fun mission -> mission.Objective)
            |> Set.ofList
            |> List.ofSeq

        match targettedRegions with
        | [] -> TooFewTargets
        | _ :: _ ->
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
                                        budget.TryCheckout(start.AirfieldId, mission.CheckoutData)
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
                |> List.map AirMission
            Plan(missions, budget)

    /// Try to send planes closer to the frontline.
    let tryTransferPlanesForward (friendly : CoalitionId) (budget : Airfields) (war : WarState) =
        let airfields =
            war.World.Airfields.Values
            |> Seq.filter (fun af -> war.GetOwner(af.Region) = Some friendly)
            |> List.ofSeq

        let enemy = friendly.Other
        let distanceToEnemy = war.ComputeDistancesToCoalition enemy
        let mutable budget = budget
        let missions =
            [
                let forward =
                    Seq.allPairs airfields airfields
                    |> Seq.where (fun (af1, af2) -> distanceToEnemy.[af1.Region] > distanceToEnemy.[af2.Region])
                for af1, af2 in forward do
                    let mutable excessPlanes =
                        let afid = af1.AirfieldId
                        let af = budget.Airfields.[afid]
                        (totalPlanes af.Planes * planeRunCost - af.Resources) / planeRunCost
                    let mutable sustainable =
                        let afid = af2.AirfieldId
                        let af = budget.Airfields.[afid]
                        (af.Resources - totalPlanes af.Planes * planeRunCost) / planeRunCost
                    for plane in attackersOf friendly @ fightersOf friendly @ interceptorsOf friendly do
                        if excessPlanes > 0.0f && sustainable > 0.0f then
                            let numPlanes =
                                sumPlanes budget.Airfields.[af1.AirfieldId].Planes [plane]
                                |> min excessPlanes
                                |> int
                                |> max 4
                            if numPlanes > 0 then
                                let mission =
                                    {
                                        StartAirfield = af1.AirfieldId
                                        Objective = af2.Region
                                        MissionType = PlaneTransfer af2.AirfieldId
                                        NumPlanes = numPlanes
                                        Plane = plane
                                    }
                                let budget2 = budget.TryCheckout(af1.AirfieldId, mission.CheckoutData)
                                match budget2 with
                                | Some b ->
                                    budget <- b
                                    let numPlanes = (float32 numPlanes)
                                    excessPlanes <- excessPlanes - numPlanes
                                    sustainable <- sustainable - numPlanes
                                    yield mission
                                | None ->
                                    ()
            ]
            |> List.map AirMission
        Plan (missions, budget)

    let rec oneSideStrikes (side : CoalitionId) depth (war : WarState) =
        let budget = Airfields.Create war
        match tryMakeAirRaids side budget war with
        | TooFewTargets ->
            Victory side
        | Plan([], _) ->
            if depth > 0 then
                oneSideStrikes side.Other (depth - 1) war
            else
                Stalemate
        | Plan(missions, budget) ->
            let raids =
                missions
                |> List.choose (function AirMission x -> Some x | _ -> None)
            let covers, budget =
                tryMakeCovers side.Other budget war raids
                |> MissionPlanningResult.getMissions budget
            let sideTransfers, budget =
                tryTransferPlanesForward side budget war
                |> MissionPlanningResult.getMissions budget
            let otherTransfers, budget =
                tryTransferPlanesForward side.Other budget war
                |> MissionPlanningResult.getMissions budget
            Ongoing {
                Briefing = sprintf "%s is launching strikes agains %s assets on the ground" (string side) (string side.Other)
                Missions = missions @ covers @ sideTransfers @ otherTransfers
                Next = oneSideStrikes side.Other 1
            }

    let start war = oneSideStrikes Axis 1 war