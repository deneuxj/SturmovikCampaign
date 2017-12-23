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
                let parkingPositions = computeParkingPositions region.Parking parked.Length
                if parked.Length > 0 then
                    let mutable x0 = System.Single.PositiveInfinity
                    let mutable x1 = System.Single.NegativeInfinity
                    let mutable z0 = System.Single.PositiveInfinity
                    let mutable z1 = System.Single.NegativeInfinity
                    for vehicle, pos in Seq.zip parked parkingPositions do
                        x0 <- min x0 pos.X
                        x1 <- max x1 pos.X
                        z0 <- min z0 pos.Y
                        z1 <- max z1 pos.Y
                        let model =
                            match vehicle, coalition with
                            | HeavyTank, Axis -> Vehicles.vehicles.GermanStaticHeavyTank
                            | MediumTank, Axis -> Vehicles.vehicles.GermanStaticMediumTank
                            | LightArmor, Axis -> Vehicles.vehicles.GermanStaticLightArmor
                            | HeavyTank, Allies -> Vehicles.vehicles.RussianStaticHeavyTank
                            | MediumTank, Allies -> Vehicles.vehicles.RussianStaticMediumTank
                            | LightArmor, Allies -> Vehicles.vehicles.RussianStaticLightArmor
                        let position =
                            newBlockMcu store country Vehicles.vehicles.TankPosition.Model Vehicles.vehicles.TankPosition.Script 3000
                        let mcus =
                            if inAttackArea pos then
                                let block, entity = newBlockWithEntityMcu store country model.Model model.Script vehicle.Durability
                                [ block; upcast entity ]
                            else
                                [ newBlockMcu store country model.Model model.Script vehicle.Durability ]
                        let mcus = position :: mcus
                        for mcu in mcus do
                            pos.AssignTo mcu.Pos
                        yield! mcus
                    let x0 = x0 - maxParkingSpacing
                    let x1 = x1 + maxParkingSpacing
                    let z0 = z0 - maxParkingSpacing
                    let z1 = z1 + maxParkingSpacing
                    // fuel storage north and south of the group
                    let pos = Vector2(x1, 0.5f * (z0 + z1))
                    let block = newBlockMcu store country Vehicles.vehicles.Fuel.Model Vehicles.vehicles.Fuel.Script 1000
                    pos.AssignTo block.Pos
                    yield block
                    let pos = Vector2(x0, 0.5f * (z0 + z1))
                    let block = newBlockMcu store country Vehicles.vehicles.Fuel.Model Vehicles.vehicles.Fuel.Script 1000
                    pos.AssignTo block.Pos
                    yield block
                    // towers at the four corners
                    for x in [ x0; x1 ] do
                        for z in [ z0; z1 ] do
                            let pos = Vector2(x, z)
                            let block = newBlockMcu store country Vehicles.vehicles.Tower.Model Vehicles.vehicles.Tower.Script 2000
                            pos.AssignTo block.Pos
                            yield block
    ]
