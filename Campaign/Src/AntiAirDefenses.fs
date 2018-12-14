// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
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

module Campaign.AntiAirDefenses

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.StaticDefenses.Factory
open Util
open Campaign.WorldDescription
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.WorldState
open System.Numerics
open VectorExtension
open Campaign.StaticDefenseOptimization
open SturmovikMission.Blocks.StaticDefenses.Types


let mkAADefenses (includeSearchLights, world : World, state : WorldState) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let regionFillLevel =
        state.GetAmmoCostPerRegion(world)
        |> Map.map (fun region cost ->
            if cost > 0.0f<E> then
                sg.GetRegion(region).Supplies / cost
            else
                1.0f
            |> max 0.0f
            |> min 1.0f)
    let nests =
        [
            for area in world.AntiAirDefenses do
                let numUnits =
                    regionFillLevel
                    |> Map.tryFind area.Home
                    |> Option.defaultVal 0.0f
                    |> ((*) (float32 area.MaxNumGuns))
                    |> ceil
                    |> int
                if numUnits > 0 then
                    let owner = sg.GetRegion(area.Home).Owner
                    let country, coalition =
                        match owner with
                        | None -> failwithf "No owner found for group of anti-air defenses '%A'" area.DefenseAreaId
                        | Some Axis -> Mcu.CountryValue.Germany, Mcu.CoalitionValue.Axis
                        | Some Allies -> Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies
                    // Defense areas close to airfield spawns are made stronger
                    let isHighPrio =
                        state.RearAirfield(owner.Value)
                        |> wg.GetAirfield
                        |> fun af ->
                            af.Spawn
                            |> Seq.exists (fun spawn -> (Vector2.FromPos(spawn) - area.Position.Pos).Length() < 2500.0f)
                    let nest =
                        { Priority = if isHighPrio then 10.0f else 1.0f
                          Number = numUnits
                          Boundary = area.Boundary
                          Rotation = area.Position.Rotation
                          Settings = if isHighPrio then CanonGenerationSettings.Strong else CanonGenerationSettings.Default
                          Specialty = area.Role
                          IncludeSearchLights = includeSearchLights
                          Country = country
                        }
                    yield nest
                    // Add machine guns to protect from low-level attacks
                    match area.Role with
                    | AntiAirCanon ->
                        yield
                            { nest with
                                Number = (nest.Number / 4) |> max 1
                                IncludeSearchLights = false
                                Specialty = AntiAirMg
                            }
                    | _ -> ()
        ]
    nests
