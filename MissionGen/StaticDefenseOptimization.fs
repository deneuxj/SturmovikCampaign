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
open Util

type Nest = {
    Priority : float32
    Number : int
    Boundary : Vector2 list
    Rotation : float32
    Settings : CanonGenerationSettings
    Specialty : DefenseSpecialty
    IncludeSearchLights : bool
    IncludeFlak : bool
    Country : Mcu.CountryValue
}
with
    member this.Generate(store, lcStore, random) =
        let coalition = McuUtil.coalitionOf this.Country
        let group = StaticDefenseGroup.Create(this.Settings, this.Specialty, this.IncludeFlak, this.IncludeSearchLights, random, store, lcStore, this.Boundary, this.Rotation, this.Number, this.Country, coalition)
        group

/// Select nests by priority so that the total number of cannons and guns does not exceed the given limit
let select random (limit : int) nests =
    let sorted =
        nests
        |> List.sortByDescending (fun nest -> nest.Priority, nest.Number)
    let untouched, excess =
        [
            let limit = limit / 2
            let mutable acc = 0
            for nest in sorted do
                acc <- acc + nest.Number
                yield acc <= limit, nest
        ]
        |> List.partition fst
    let randomized =
        excess
        |> Array.ofList
        |> Array.shuffle (random)
        |> List.ofArray
    [
        let mutable acc = 0
        for _, nest in untouched do
            acc <- acc + nest.Number
            yield nest
        for _, nest in randomized do
            acc <- acc + nest.Number
            if acc <= limit then
                yield nest
    ]

let instantiateAll store lcStore random (missionBegin : Mcu.McuTrigger) (nests : Nest list) =
    let groups =
        nests
        |> List.map (fun nest -> nest.Generate(store, lcStore, random))
    for group in groups do
        let links = group.CreateLinks()
        links.Apply(McuUtil.deepContentOf group)
        Mcu.addTargetLink missionBegin group.Api.Start.Index
    groups