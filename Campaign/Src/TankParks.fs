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

module Campaign.TankParks

open System.Numerics
open VectorExtension

open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks
open Util
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Campaign.BasicTypes

let createParkedTanks store (maxTanksInParks : int) (world : World) (state : WorldState) inAttackArea (orders : OrderPackage) (coalition : CoalitionId) =
    let country = coalition.ToCountry |> int
    [
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && not(List.isEmpty region.TankHiding) && not regState.HasInvaders && not regState.NumExposedVehicles.IsEmpty then
                let subtracted =
                    orders.Columns
                    |> List.filter (fun order -> order.Start = region.RegionId)
                    |> List.map (fun order -> order.Composition)
                    |> Array.concat
                    |> compactSeq
                let parked =
                    subMaps regState.NumExposedVehicles subtracted
                    |> expandMap
                    |> Array.shuffle (System.Random())
                    |> Array.truncate maxTanksInParks
                let netPositions =
                    region.TankHiding
                    |> Seq.choose (fun block ->
                        match block.PlaneParkingPositions with
                        | Some x -> Some(block.Pos, x)
                        | None -> None)
                let parkingPositions =
                    seq {
                        for blockPos, relPosGroups in netPositions do
                            for absPos, _ in relPosGroups.Positions do
                                let dv = Vector2(absPos.X, absPos.Y) - relPosGroups.RefPos
                                yield blockPos.Pos + dv, blockPos.Rotation + absPos.Z
                    }
                if parked.Length > 0 then

                    for block in region.TankHiding do
                        let block = newBlockMcu store country block.Model block.Script 1000
                        yield Choice1Of2 block

                    for vehicle, (pos, rot) in Seq.zip parked parkingPositions do
                        let pos = Vector2(pos.X, pos.Y)
                        let model =
                            if world.IsWWI then
                                match coalition with
                                | Axis -> Vehicles.vehicles.StaticGermanTruck
                                | Allies -> Vehicles.vehicles.StaticRussianTruck
                            else
                                match vehicle, coalition with
                                | HeavyTank, Axis -> Vehicles.vehicles.GermanStaticHeavyTank
                                | MediumTank, Axis -> Vehicles.vehicles.GermanStaticMediumTank
                                | LightArmor, Axis -> Vehicles.vehicles.GermanStaticLightArmor
                                | HeavyTank, Allies -> Vehicles.vehicles.RussianStaticHeavyTank
                                | MediumTank, Allies -> Vehicles.vehicles.RussianStaticMediumTank
                                | LightArmor, Allies -> Vehicles.vehicles.RussianStaticLightArmor
                        let mcus =
                            let durability =
                                if world.IsWWI then
                                    LightArmor.Durability
                                else
                                    vehicle.Durability
                            if inAttackArea pos then
                                let block, entity = newBlockWithEntityMcu store country model.Model model.Script durability
                                [ block; upcast entity ]
                            else
                                [ newBlockMcu store country model.Model model.Script durability ]
                        for mcu in mcus do
                            pos.AssignTo mcu.Pos
                            mcu.Ori.Y <- float rot
                            yield Choice1Of2 mcu
    ]
