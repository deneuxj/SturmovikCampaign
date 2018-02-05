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

let createParkedTanks store (world : World) (state : WorldState) inAttackArea (orders : OrderPackage) (coalition : CoalitionId) =
    let netsModel, netRefPos, netRelPositions =
        let nets = Vehicles.vehicles.Nets
        let nets =
            { Script = nets.Script
              Model = nets.Model
              Pos = { Pos = Vector2.Zero; Altitude = 0.0f; Rotation = 0.0f }
            }
        let positions = nets.PlaneParkingPositions
        match positions with
        | Some positions ->
            nets, positions.RefPos, positions.Positions |> Seq.map fst |> Array.ofSeq
        | None ->
            failwith "Could not find Nets in static objects"
    let random = System.Random()
    let country = coalition.ToCountry |> int
    [
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && not(List.isEmpty region.Parking) && not regState.HasInvaders then
                let subtracted =
                    orders.Columns
                    |> List.filter (fun order -> order.Start = region.RegionId)
                    |> List.map (fun order -> order.Composition)
                    |> Array.concat
                    |> compactSeq
                let parked =
                    subMaps regState.NumVehicles subtracted
                    |> expandMap
                    |> Array.shuffle (System.Random())
                let parkingPositions = computeRandomParkingPositions region.Parking parked.Length
                if parked.Length > 0 then
                    for vehicle, pos in Seq.zip parked parkingPositions do
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
                        let netGroup, tankRot =
                            let block = newBlockMcu store country netsModel.Model netsModel.Script 1000
                            let idx = random.Next(0, netRelPositions.Length)
                            let dv = netRelPositions.[idx]
                            let rot = dv.Z
                            let dv = Vector2(dv.X, dv.Y) - netRefPos
                            let blockPos = pos - dv
                            blockPos.AssignTo block.Pos
                            block, rot
                        for mcu in mcus do
                            pos.AssignTo mcu.Pos
                            mcu.Ori.Y <- float tankRot
                        yield! mcus
                        yield netGroup
    ]
