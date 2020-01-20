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
    | Ongoing of (WarState -> StepData)

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

    let attackersOf =
        function
        | Axis -> [ idMe262 ]
        | Allies -> [ idP38; idP47]

    let interceptorsOf =
        function
        | Axis -> [ idFw190d9; idBf109k4 ]
        | Allies -> [ idP51; idP47; idP38 ]

    let fightersOf =
        function
        | Axis -> [ idFw190a8; idBf109g14 ]
        | Allies -> [ idP51; idSpitfire; idTempest ]

    let sumPlanes (atAirfield : Map<PlaneModelId, float32>) (planes : PlaneModelId seq) =
        planes
        |> Seq.fold (fun s plane ->
            atAirfield.TryFind(plane)
            |> Option.defaultValue 0.0f
            |> (+) s) 0.0f

    let rec airRaid (friendly : CoalitionId) (war : WarState) =
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
        | [] ->
            // No more suitable targets. Try launch an invasion instead
            groundInvasion friendly war
        | _ :: _ ->
            let raids =
                seq {
                    let mutable available = Airfields.Create(war)
                    let rec work plane planesPerMission (starts : Airfield list) (targets : Airfield list) =
                        seq {
                            match starts, targets with
                            | [], _ | _, [] -> ()
                            | af :: starts, af2 :: targets ->
                                let distance =
                                    let pos = List.head af.Boundary
                                    let pos2 = List.head af2.Boundary
                                    (pos - pos2).Length() * 1.0f<M>
                                if distance <= war.World.PlaneSet.[plane].MaxRange then
                                    let numPlanes =
                                        totalPlanes(war.GetNumPlanes(af2.AirfieldId))
                                    let resources = war.GetAirfieldCapacity(af2.AirfieldId)
                                    if numPlanes >= minPlanesAtAirfield && resources >= minPlanesAtAirfield * planeRunCost then
                                        let mission : AirMission =
                                            {
                                                StartAirfield = af.AirfieldId
                                                Objective = af2.Region
                                                MissionType = GroundTargetAttack(AirfieldTarget af2.AirfieldId, LowAltitude)
                                                NumPlanes = int planesPerMission
                                                Plane = plane
                                            }
                                        let av2 = available.TryCheckout(af.AirfieldId, mission.CheckoutData)
                                        match av2 with
                                        | Some av2 ->
                                            available <- av2
                                            yield mission
                                        | None ->
                                            ()
                                yield! work plane planesPerMission starts targets
                        }
                    for planesPerMission in [25; 10; 5] do
                        for plane in attackersOf friendly do
                            yield! work plane (float32 planesPerMission) readyAirfields raidTargets
                }
            if Seq.isEmpty raids then
                // No raids can be made, try to ferry planes closer to the front
                ferryPlanes friendly war
            else
                let raidsAndCovers =
                    seq {
                        let mutable available = Airfields.Create(war)
                        let raidIt = raids.GetEnumerator()
                        let rec work plane planesPerMission starts =
                            seq {
                                if raidIt.MoveNext() then
                                    match starts with
                                    | [] -> ()
                                    | af :: starts ->
                                        let raid = raidIt.Current
                                        let distance =
                                            let pos = List.head af.Boundary
                                            let pos2 = war.World.Regions.[raid.Objective].Position
                                            (pos - pos2).Length() * 1.0f<M>
                                        if distance <= war.World.PlaneSet.[plane].MaxRange then
                                            let mission : AirMission =
                                                {
                                                    StartAirfield = af.AirfieldId
                                                    Objective = raid.Objective
                                                    MissionType = AreaProtection
                                                    NumPlanes = int planesPerMission
                                                    Plane = plane
                                                }
                                            let av2 =
                                                available.TryCheckout(raid.StartAirfield, raid.CheckoutData)
                                                |> Option.bind (fun av -> av.TryCheckout(af.AirfieldId, mission.CheckoutData))
                                            match av2 with
                                            | Some av2 ->
                                                available <- av2
                                                yield raid
                                                yield mission
                                            | None ->
                                                ()
                                        yield! work plane planesPerMission starts
                            }
                        for planesPerMission in [8; 4] do
                            for plane in fightersOf friendly do
                                yield! work plane (float32 planesPerMission) readyAirfields
                    }
                raidsAndCovers
    