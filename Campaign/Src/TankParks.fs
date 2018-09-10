﻿// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
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
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks
open SturmovikMission.Blocks.Vehicles

open VectorExtension

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Util
open Campaign.ParkingArea
open Campaign.BasicTypes
open SturmovikMission.Blocks.StaticDefenses.Factory
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider
open SturmovikMission.Blocks.StaticDefenses.Types

let createParkedTanks store lcStore (world : World) (state : WorldState) inAttackArea withSearchLights (missionBegin : Mcu.McuTrigger) (orders : OrderPackage) (coalition : CoalitionId) =
    let netsModel, netRelPositions =
        let nets = Vehicles.vehicles.Nets
        let nets =
            { Script = nets.Script
              Model = nets.Model
              Pos = { Pos = Vector2.Zero; Altitude = 0.0f; Rotation = 0.0f }
            }
        let positions = nets.PlaneParkingPositions
        match positions with
        | Some positions ->
            let rels =
                let center = Vector3(positions.RefPos.X, positions.RefPos.Y, 0.0f)
                positions.Positions
                |> Seq.map fst
                |> Seq.map (fun pos -> pos - center)
                |> Array.ofSeq
            nets, rels
        | None ->
            failwith "Could not find Nets in static objects"
    let netFlatPos =
        netRelPositions
        |> Array.map (fun v3 -> Vector2(v3.X, v3.Y))
    let country = coalition.ToCountry |> int
    [
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && not(List.isEmpty region.Parking) && not regState.HasInvaders && not regState.NumExposedVehicles.IsEmpty then
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
                let netPositions = computeRandomParkingPositions netFlatPos region.Parking parked.Length
                let parkingPositions =
                    seq {
                        for center in netPositions do
                            for dv in netRelPositions do
                                yield Vector3(center.X, center.Y, 0.0f) + dv
                    }
                if parked.Length > 0 then
                    // Add machine guns among tanks for anti-air defense
                    let aaDefenses =
                        let boundary = region.Parking
                        StaticDefenseGroup.Create(CanonGenerationSettings.Default, StaticDefenses.Types.AntiAirMg, withSearchLights, System.Random(), store, lcStore, boundary, 0.0f, 8, coalition.ToCountry, coalition.ToCoalition)
                    let links = aaDefenses.CreateLinks()
                    links.Apply(McuUtil.deepContentOf(aaDefenses))
                    Mcu.addTargetLink missionBegin aaDefenses.Api.Start.Index
                    yield! McuUtil.deepContentOf(aaDefenses)

                    for pos in netPositions do
                        let block = newBlockMcu store country netsModel.Model netsModel.Script 1000
                        pos.AssignTo block.Pos
                        yield block

                    for vehicle, pos in Seq.zip parked parkingPositions do
                        let rot = pos.Z
                        let pos = Vector2(pos.X, pos.Y)
                        let model =
                            match vehicle, coalition with
                            | HeavyTank, Axis -> Vehicles.vehicles.GermanStaticHeavyTank
                            | MediumTank, Axis -> Vehicles.vehicles.GermanStaticMediumTank
                            | LightArmor, Axis -> Vehicles.vehicles.GermanStaticLightArmor
                            | HeavyTank, Allies -> Vehicles.vehicles.RussianStaticHeavyTank
                            | MediumTank, Allies -> Vehicles.vehicles.RussianStaticMediumTank
                            | LightArmor, Allies -> Vehicles.vehicles.RussianStaticLightArmor
                        let mcus =
                            if inAttackArea pos then
                                let block, entity = newBlockWithEntityMcu store country model.Model model.Script vehicle.Durability
                                [ block; upcast entity ]
                            else
                                [ newBlockMcu store country model.Model model.Script vehicle.Durability ]
                        for mcu in mcus do
                            pos.AssignTo mcu.Pos
                            mcu.Ori.Y <- float rot
                        yield! mcus
    ]