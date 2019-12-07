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

type Commands =
    | DamageBuilding of Instance: BuildingInstance * Part: int * Damage: float32

module DamageExtension =
    type BuildingStatus with
        member this.ApplyDamage(part, damage) =
            let health =
                this.Health
                |> Map.tryFind part
                |> Option.defaultValue 1.0f
            let health = health - damage
            { this with
                Health = this.Health.Add(part, health)
            }

    type Commands with
        static member ApplyBuildingDamage(instance : BuildingInstance, part, damage, state : WarState) =
            let applyDamageToBuildings buildings =
                buildings
                |> List.map (fun building ->
                    if obj.ReferenceEquals(building.Instance, instance) then
                        building.ApplyDamage(part, damage)
                    else
                        building)
            let regions =
                state.Regions
                |> Map.map (fun _ region ->
                    if instance.Pos.Pos.IsInConvexPolygon region.Properties.Boundary then
                        let buildings = applyDamageToBuildings region.Buildings
                        { region with Buildings = buildings }
                    else
                        region)
            let airfields =
                state.Airfields
                |> Map.map (fun _ airfield ->
                    if instance.Pos.Pos.IsInConvexPolygon airfield.Properties.Boundary then
                        let buildings = applyDamageToBuildings airfield.Buildings
                        { airfield with Buildings = buildings }
                    else
                        airfield
                )
            { state with
                Regions = regions
                Airfields = airfields }


