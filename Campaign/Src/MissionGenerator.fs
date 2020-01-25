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

type ScenarioStep =
    | Stalemate
    | Victory of CoalitionId
    | Ongoing of StepData

and StepData =
    {
        Briefing : string
        Missions : Mission list
        Next : WarState -> ScenarioStep
    }

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

module Bodenplatte =
    let totalPlanes : Map<_, float32> -> float32 =
        Map.toSeq >> Seq.sumBy snd

    let planeRunCost = 10.0f<M^3>

    type Campaign.Missions.AirMission with
        member this.CheckoutData =
            (this.Plane, planeRunCost, float32 this.NumPlanes)

    let minPlanesAtAirfield = 10.0f

    let idMe262 = PlaneModelId "me262"
    let idBf109g14 = PlaneModelId "bf109g14"
    let idFw190a8 = PlaneModelId "fw190a8"
    let idBf109k4 = PlaneModelId "bf109k4"
    let idFw190d9 = PlaneModelId "fw190d9"

    let idP51 = PlaneModelId "p51d"
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

    /// Get the total number of planes of give types at an airfield
    let sumPlanes (atAirfield : Map<PlaneModelId, float32>) (planes : PlaneModelId seq) =
        planes
        |> Seq.fold (fun s plane ->
            atAirfield.TryFind(plane)
            |> Option.defaultValue 0.0f
            |> (+) s) 0.0f

    /// Try to make missions to attack enemy airfields, with sufficient cover over target, and over home airfield
    let tryMakeAirRaid (friendly : CoalitionId) (budget : Airfields) (war : WarState) =
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
                war.GetAirfieldCapacity(af.AirfieldId) >= minPlanesAtAirfield * planeRunCost)

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
                numPlanes >= minPlanesAtAirfield && resources >= minPlanesAtAirfield * planeRunCost)

        match raidTargets with
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
        | [] ->
            TooFewTargets
