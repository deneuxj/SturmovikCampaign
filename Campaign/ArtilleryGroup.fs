// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneu <johann.deneux@gmail.com>
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

module Campaign.ArtilleryGroup

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.StaticDefenses.Factory
open Util
open Campaign.WorldDescription
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.WorldState

type ArtilleryGroup = {
    All : Mcu.McuBase list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = []
        member x.SubGroups = []

    static member Create(random : System.Random, store, lcStore, includeSearchLights, missionBegin : Mcu.McuTrigger, world : World, state : WorldState, attackingColumns : ColumnMovement list) =
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
        let all =
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
                        let group = StaticDefenseGroup.Create(area.Role, includeSearchLights, random, store, lcStore, area.Boundary, area.Position.Rotation, numUnits, country, coalition)
                        let links = group.CreateLinks()
                        let mcus = McuUtil.deepContentOf group
                        links.Apply(mcus)
                        Mcu.addTargetLink missionBegin group.Api.Start.Index
                        yield! mcus
            ]
        { All = all
        }
