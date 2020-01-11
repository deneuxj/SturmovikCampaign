// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2019 Johann Deneux <johann.deneux@gmail.com>
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

namespace Campaign.WarStateUpdate

open System.Numerics
open VectorExtension

open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.BasicTypes
open Campaign.WorldDescription
open Campaign.PlaneModel

// We use explicit command and result types for all changes to the state of the war.
// This makes it easier to present relevant data to players and help them understand what effect their actions have.

/// Commands to change a WarState
type Commands =
    // Damage a part of a building or a bridge
    | DamageBuildingPart of Instance: BuildingInstanceId * Part: int * Damage: float32
    // Repair a part of a building or a bridge
    | RepairBuildingPart of Instance: BuildingInstanceId * Part: int * Healing: float32
    // Remove a plane from an airfield
    | RemovePlane of AirfieldId * PlaneModelId * Health: float32
    // Add a plane to an airfield
    | AddPlane of AirfieldId * PlaneModelId * Health: float32
    // Add ground forces to a region
    | AddGroundForces of RegionId * CoalitionId * Amount: float32<MGF>
    // Destroy ground forces in a region
    | DestroyGroundForces of RegionId * CoalitionId * Amount: float32<MGF>
    // Move ground forces from a region to another
    | MoveGroundForces of RegionId * RegionId * CoalitionId * Amount: float32<MGF>

/// Interesting data to report from execution of commands
type Results =
    | UpdatedStorageValue of Instance: BuildingInstanceId * Amount: float32<M^3>
    | UpdatedPlanesAtAirfield of AirfieldId * Map<PlaneModelId, float32>
    | UpdatedGroundForces of RegionId * CoalitionId * float32<MGF>

module DamageExtension =
    type WarState with
        /// Apply damage to or repair a part of a building. Return new storage volume of the whole building.
        member this.ChangeHealth(bid, part, delta) =
            let health = this.GetBuildingPartHealthLevel(bid, part)
            let health = health + delta |> max 0.0f |> min 1.0f
            let isBridge = this.World.Bridges.ContainsKey(bid)
            this.SetBuildingPartHealthLevel(bid, part, health)
            let store =
                if isBridge then
                    0.0f<M^3>
                else
                    this.GetBuildingCapacity(bid)
            store

        /// Add or remove planes from an airfield, return all the planes at that airfield after the change
        member this.ChangePlanes(afid, plane, delta) =
            let qty = this.GetNumPlanes(afid, plane) + delta
            this.SetNumPlanes(afid, plane, qty)
            this.GetNumPlanes(afid)

        /// Add or remove forces of a coalition in a region, return the new amount of forces
        member this.ChangeGroundForces(rid, coalition, delta) =
            let forces =
                this.GetGroundForces(coalition, rid) + delta
                |> max 0.0f<MGF>
            this.SetGroundForces(coalition, rid, forces)
            forces

    type Commands with
        /// Execute commands on a WarState. Return the result of the command.
        member this.Execute(state : WarState) =
            match this with
            | DamageBuildingPart(bid, part, dmg) ->
                let storage = state.ChangeHealth(bid, part, -dmg)
                [ UpdatedStorageValue(bid, storage) ]
            | RepairBuildingPart(bid, part, heal) ->
                let storage = state.ChangeHealth(bid, part, heal)
                [ UpdatedStorageValue(bid, storage) ]
            | AddPlane(afid, plane, health) ->
                let newStatus = state.ChangePlanes(afid, plane, health)
                [ UpdatedPlanesAtAirfield(afid, newStatus) ]
            | RemovePlane(afid, plane, health) ->
                let newStatus = state.ChangePlanes(afid, plane, -health)
                [ UpdatedPlanesAtAirfield(afid, newStatus) ]
            | AddGroundForces(rid, coalition, amount) ->
                let newForces = state.ChangeGroundForces(rid, coalition, amount)
                [ UpdatedGroundForces(rid, coalition, newForces) ]
            | DestroyGroundForces(rid, coalition, destroyed) ->
                let newForces = state.ChangeGroundForces(rid, coalition, -destroyed)
                [ UpdatedGroundForces(rid, coalition, newForces) ]
            | MoveGroundForces(start, destination, coalition, forces) ->
                let startForces = state.ChangeGroundForces(start, coalition, forces)
                let destinationForces = state.ChangeGroundForces(destination, coalition, -forces)
                [ UpdatedGroundForces(start, coalition, startForces)
                  UpdatedGroundForces(destination, coalition, destinationForces) ]
