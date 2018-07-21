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

module Campaign.StaticBlocks

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData.CommonMethods
open SturmovikMission.Blocks.BlocksMissionData
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.BasicTypes


let inline createBlocksGen mkDamaged (random : System.Random) (store : NumericalIdentifiers.IdStore) (world : World) (state : WorldState) (inAttackArea : Vector2 -> bool) (blocks : ^T list) =
    let tryGetRegionAt(v : Vector2) =
        world.Regions
        |> List.tryFind (fun region ->
            v.IsInConvexPolygon(region.Boundary)
        )
    let getState =
        let m =
            state.Regions
            |> Seq.map (fun region -> region.RegionId, region)
            |> dict
        fun x -> m.[x]
    let getHealth (region : Region) (regionState : RegionState) (v : Vector2) =
        let afStorageWithHealth =
            List.zip world.Airfields state.Airfields
            |> List.filter (fun (af, _) -> af.Region = region.RegionId)
            |> List.collect (fun (af, afState) -> List.zip af.Storage afState.StorageHealth)
        let dist, health =
            try
                Seq.zip (region.Storage @ region.Production) (regionState.StorageHealth @ regionState.ProductionHealth)
                |> Seq.append afStorageWithHealth
                |> Seq.map (fun (block, health) -> (block.Pos.Pos - v).LengthSquared(), health)
                |> Seq.minBy fst
            with
            | _ -> System.Single.PositiveInfinity, [||]
        if dist < 1.0f then
            Some health
        else
            None
    let playArea =
        world.Regions
        |> List.map (fun region -> region.Boundary)
        |> List.concat
        |> convexHull
    [
        for block in blocks do
            let v = Vector2.FromPos(block)
            let subst = Mcu.substId <| store.GetIdMapper()
            let model : string = valueOf(getModel block)
            match tryGetRegionAt v with
            | None when v.IsInConvexPolygon playArea ->
                // Include all objects in the convex hull of all regions
                // This fixes a bug where bridges located in the space between two neighbouring regions were culled.
                let mcu =
                    createMcu block
                    :?> Mcu.HasEntity
                mcu.LinkTrId <- 0
                mcu.Name <- "GapBlock"
                subst mcu
                yield mcu :> Mcu.McuBase
            | None ->
                ()
            | Some region ->
                let state = getState region.RegionId
                let health = getHealth region state v
                match health with
                | None ->
                    // No strategic value, health is not tracked, show without damage
                    let mcu =
                        createMcu block
                        :?> Mcu.HasEntity
                    subst mcu
                    mcu.LinkTrId <- 0
                    mcu.Name <- sprintf "NoStrategicBlock-%d" mcu.Index
                    yield upcast mcu
                | Some healths ->
                    // Has health and strategic value, show damage if any
                    let building = StaticGroup.FromBlock block
                    let damagedBlock =
                        block
                        |> setDamaged (
                            mkDamaged (
                                let subBlocks = building.SubBlocks(world.SubBlockSpecs)
                                Array.zip subBlocks healths
                                |> Seq.map (fun (idx, h) -> (idx, T.Float (float h)))
                                |> Map.ofSeq))
                        |> setDurability (StaticGroup.FromBlock(block).Durability(world.SubBlockSpecs) |> T.Integer)
                        |> setIndex (T.Integer 1)
                        |> setLinkTrId (T.Integer 0) // No entity
                        |> createMcu
                        :?> Mcu.HasEntity
                    match state.Owner with
                    | Some Allies ->
                        damagedBlock.Country <- Some Mcu.CountryValue.Russia
                    | Some Axis ->
                        damagedBlock.Country <- Some Mcu.CountryValue.Germany
                    | _ ->
                        ()
                    subst damagedBlock
                    // Give an entity if located in an area attacked by AIs, so that AIs will target the block.
                    if inAttackArea (Vector2.FromMcu damagedBlock.Pos) then
                        damagedBlock.Name <- "TargetedStrategicBlock"
                        let subst2 = Mcu.substId <| store.GetIdMapper()
                        let entity = newEntity 1
                        McuUtil.vecCopy damagedBlock.Pos entity.Pos
                        McuUtil.vecCopy damagedBlock.Ori entity.Ori
                        subst2 entity
                        Mcu.connectEntity damagedBlock entity
                        yield upcast entity
                    else
                        damagedBlock.Name <- "StrategicBlock"
                    // Result
                    yield upcast damagedBlock
    ]

let createBlocks random store world state inAttackArea (blocks : T.Block list) = createBlocksGen T.Block.Damaged random store world state inAttackArea blocks

let createBridges random store world state inAttackArea (blocks : T.Bridge list) = createBlocksGen T.Bridge.Damaged random store world state inAttackArea blocks

let createGrounds (store : NumericalIdentifiers.IdStore) (blocks : T.Ground list) =
    [
        for block in blocks do
            let subst = Mcu.substId <| store.GetIdMapper()
            let mcu =
                createMcu block
            subst mcu
            yield mcu
    ]
