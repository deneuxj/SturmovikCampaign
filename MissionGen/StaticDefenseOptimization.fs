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

module Campaign.MissionGen.StaticDefenseOptimization

open System.Numerics
open SturmovikMission.Blocks.StaticDefenses.Types
open SturmovikMission.Blocks.StaticDefenses.Factory
open SturmovikMission.DataProvider
open Campaign.Common.BasicTypes

open Util

type Nest = {
    Priority : System.IComparable
    Number : int
    Boundary : Vector2 list
    Rotation : float32
    Settings : CanonGenerationSettings
    Specialty : DefenseSpecialty
    IncludeSearchLights : bool
    IncludeFlak : bool
    Country : Mcu.CountryValue
    Coalition : CoalitionId
    Group : System.IComparable
    Cost : float32
}
with
    member this.Generate(store, lcStore, random) =
        let group = StaticDefenseGroup.Create(this.Settings, this.Specialty, this.IncludeFlak, this.IncludeSearchLights, random, store, this.Boundary, this.Rotation, this.Number, this.Country, this.Coalition.ToCoalition)
        group

/// Select nests by priority so that the total number of cannons and guns does not exceed the given limit
let select random (limit : int, groupBudget : Map<System.IComparable * CoalitionId, float32>) nests =
    let sorted =
        nests
        |> List.sortByDescending (fun nest -> nest.Priority, nest.Number)
    // Get the most important nests, up to half the limit
    let untouched, excess =
        [
            let limit = limit / 2
            let mutable acc = 0
            let mutable budget = groupBudget
            for nest in sorted do
                acc <- acc + nest.Number
                let available = budget.TryFind(nest.Group, nest.Coalition) |> Option.defaultValue 0.0f
                let remaining = available - nest.Cost
                if remaining >= 0.0f then
                    budget <- budget.Add((nest.Group, nest.Coalition), remaining)
                yield acc <= limit && remaining >= 0.0f, nest
        ]
        |> List.partition fst
    // Fill the rest with randomly selected nests
    let randomized =
        excess
        |> Array.ofList
        |> Array.shuffle (random)
        |> List.ofArray
    [
        let mutable acc = 0
        let mutable budget = groupBudget
        for _, nest in untouched do
            acc <- acc + nest.Number
            let available = budget.TryFind(nest.Group, nest.Coalition) |> Option.defaultValue 0.0f
            let remaining = available - nest.Cost
            budget <- budget.Add((nest.Group, nest.Coalition), remaining)
            yield nest
        for _, nest in randomized do
            acc <- acc + nest.Number
            let available = budget.TryFind(nest.Group, nest.Coalition) |> Option.defaultValue 0.0f
            let remaining = available - nest.Cost
            if remaining >= 0.0f then
                budget <- budget.Add((nest.Group, nest.Coalition), remaining)
            if acc <= limit && remaining >= 0.0f then
                yield nest
    ]

let instantiateAll store lcStore random (nests : Nest list) =
    let groups =
        nests
        |> List.map (fun nest -> nest.Generate(store, lcStore, random))
    for group in groups do
        let links = group.CreateLinks()
        links.Apply(McuUtil.deepContentOf group)
    groups