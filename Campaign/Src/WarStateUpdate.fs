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
// This makes it easier to present relevant data to players and help them understand what affect their actions have.

/// Commands to change a WarState
type Commands =
    // Damage a part of a building or a bridge
    | DamageBuilding of Instance: BuildingInstanceId * Part: int * Damage: float32
    // Repair a building or a bridge
    | RepairBuilding of Instance: BuildingInstanceId * Resources: float32<E>
    // Take out resources from a region
    | TakeOutResources of RegionId * float32<E>
    // Assign resources to a region
    | MoveResourcesIn of RegionId * float32<E>
    // Take out resources from an airfield
    | TakeOutSupplies of AirfieldId * float32<E>
    // Assign supplies to an airfield
    | MoveSuppliesIn of AirfieldId * float32<E>
    // Remove a plane from an airfield
    | RemovePlane of AirfieldId * PlaneModelId * Health: float32
    // Add a plane to an airfield
    | AddPlane of AirfieldId * PlaneModelId * Health: float32

/// Interesting data to report from execution of commands
type Results =
    | StorageChange of Instance: BuildingInstanceId * Amount: float32<E>

module DamageExtension =
    type WarState with
        /// Apply damage specified in a DamageBuilding command. Return the change in storage in the affected building.
        member this.ApplyDamage(bid, part, dmg) =
            let dmg = dmg |> max 0.0f |> min 1.0f
            let health = this.GetBuildingPartHealthLevel(bid, part)
            let health = health - dmg |> max 0.0f
            let isBridge = this.World.Bridges.ContainsKey(bid)
            let store =
                if isBridge then
                    0.0f<E>
                else
                    this.GetBuildingStorage(bid)
            let fill2 =
                let fill = this.GetBuildingPartFillLevel(bid, part)
                fill * (1.0f - dmg)
                |> min (this.GetBuildingPartFunctionalityLevel(bid, part))
            this.SetBuildingPartFillLevel(bid, part, fill2)
            this.SetBuildingPartHealthLevel(bid, part, health)
            let store2 =
                if isBridge then
                    0.0f<E>
                else
                    this.GetBuildingStorage(bid)
            store2 - store

    type Commands with
        /// Execute commands on a WarState. Return the result of the command.
        member this.Execute(state : WarState) =
            match this with
            | DamageBuilding(bid, part, dmg) ->
                let change = state.ApplyDamage(bid, part, dmg)
                StorageChange(bid, change)