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

/// Logic and entities of a battlefield.
module Campaign.Battlefield

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.Battlefield
open SturmovikMission.Blocks.Vehicles
open BasicTypes
open WorldDescription
open WorldState
open System.Numerics
open VectorExtension
open Util
open Orders
open SturmovikMission.Blocks.IconDisplay

type private AreaLocation =
    | DefenseBack
    | DefenseMiddle
    | AttackBack
    | AttackMiddle

type Battlefield =
    { Supporters : RespawningCanon list // Artillery behind the attacking front, fires at defenses
      Attackers : RespawningTank list // Moving tanks
      Defenders : RespawningCanon list // Static tanks, defensive artillery and anti-tank canons
      PlayerSpawns : PlayerTankSpawn list
      Icons : IconDisplay list
      All : McuUtil.IMcuGroup
    }
with
    static member Create(random : System.Random, store, lcStore, center : Vector2, yori : float32, boundary : Vector2 list, defendingCoalition : CoalitionId, numCanons : int, defenders : Map<GroundAttackVehicle, int>, attackers : GroundAttackVehicle[], region, numDefenders, numAttackers) =
        // Get a random position within the bounding rectangle of the boundary
        let getRandomPos(areaLocation) =
            let dir = Vector2.FromYOri(float yori)
            let back =
                boundary
                |> Seq.map (fun v -> Vector2.Dot(v - center, dir))
                |> Seq.min
            let front =
                boundary
                |> Seq.map (fun v -> Vector2.Dot(v - center, dir))
                |> Seq.max
            let side = Vector2.FromYOri (float yori + 90.0)
            let left =
                boundary
                |> Seq.map (fun v -> Vector2.Dot(v - center, side))
                |> Seq.min
            let right =
                boundary
                |> Seq.map (fun v -> Vector2.Dot(v - center, side))
                |> Seq.max
            let r0 =
                let r = random.NextDouble() |> float32
                match areaLocation with
                | DefenseBack -> 0.25f * r + 0.75f
                | AttackBack -> 0.25f * r
                | DefenseMiddle -> 0.25f * r + 0.5f
                | AttackMiddle -> 0.25f * r + 0.25f
            let r1 = random.NextDouble() |> float32
            let dx = (back * r0 + front * (1.0f - r0)) * dir
            let dz = (left * r1 + right * (1.0f - r1)) * side
            center + dx + dz
        // Get a random position within the boundary
        let getRandomPos(areaLocation) =
            Seq.initInfinite (fun _ -> getRandomPos areaLocation)
            |> Seq.find (fun v -> v.IsInConvexPolygon(boundary))
        // Player spawns
        let players =
            [
                let defendersExist =
                    defenders
                    |> Map.toSeq
                    |> Seq.exists (fun (_, qty) -> qty > 0)
                if defendersExist && attackers.Length > 0 then
                    let numDefendingHeavy =
                        defenders
                        |> Map.tryFind HeavyTank
                        |> Option.defaultVal 1
                    yield PlayerTankSpawn.Ceate(store, getRandomPos(DefenseBack), yori, defendingCoalition.ToCountry, numDefendingHeavy)
                    let numAttackingHeavy =
                        attackers
                        |> Seq.filter (function HeavyTank -> true | _ -> false)
                        |> Seq.length
                        |> max 1
                    let mirrored =
                        if yori < 180.0f then
                            yori + 180.0f
                        else
                            yori - 180.0f
                    yield PlayerTankSpawn.Ceate(store, getRandomPos(AttackBack), mirrored, defendingCoalition.Other.ToCountry, numAttackingHeavy)
            ]
        // Build an attacking tank
        let buildTank name (model : VehicleTypeData) =
            let tank = RespawningTank.Create(store, getRandomPos(AttackMiddle), getRandomPos(DefenseBack), defendingCoalition.Other.ToCountry)
            tank.Tank.Name <- sprintf "B-%s-A-%s" region name
            model.AssignTo(tank.Tank)
            tank |> Choice1Of2
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCanon(model : VehicleTypeData, wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(AttackBack), getRandomPos(DefenseBack), defendingCoalition.Other.ToCountry)
            wallModel.AssignTo(arty.Wall)
            model.AssignTo(arty.Canon)
            arty |> Choice2Of2
        // Instantiate attacker blocks
        let attackers, support =
            attackers
            |> Seq.map (fun vehicle ->
                match defendingCoalition.Other, vehicle with
                | Allies, HeavyTank -> vehicles.RussianHeavyTank |> buildTank vehicle.Description
                | Allies, MediumTank -> vehicles.RussianMediumTank |> buildTank vehicle.Description
                | Allies, LightArmor -> (vehicles.RussianRocketArtillery, vehicles.TankPosition) |> buildCanon
                | Axis, HeavyTank -> vehicles.GermanHeavyTank |> buildTank vehicle.Description
                | Axis, MediumTank -> vehicles.GermanMediumTank |> buildTank vehicle.Description
                | Axis, LightArmor -> (vehicles.GermanRocketArtillery, vehicles.TankPosition) |> buildCanon
            )
            |> List.ofSeq
            |> List.partition (function Choice1Of2 _ -> true | _ -> false)
        let attackers = attackers |> List.map (function Choice1Of2 x -> x | _ -> failwith "Not a Choice1Of2")
        let support = support |> List.map (function Choice2Of2 x -> x | _ -> failwith "Not a Choice2Of2")
        // Instantiate defender blocks
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCanon (location, model : VehicleTypeData, name , wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(location), getRandomPos(AttackBack), defendingCoalition.ToCountry)
            match name with
            | Some name -> arty.Canon.Name <- sprintf "B-%s-D-%s" region name
            | None -> ()
            wallModel.AssignTo(arty.Wall)
            model.AssignTo(arty.Canon)
            arty
        let defenders =
            defenders
            |> expandMap
            |> Seq.map (fun vehicle ->
                match defendingCoalition, vehicle with
                | Allies, HeavyTank -> vehicles.RussianHeavyTank
                | Allies, MediumTank -> vehicles.RussianMediumTank
                | Allies, LightArmor -> vehicles.RussianRocketArtillery
                | Axis, HeavyTank -> vehicles.GermanHeavyTank
                | Axis, MediumTank -> vehicles.GermanMediumTank
                | Axis, LightArmor -> vehicles.GermanRocketArtillery
                |> fun x -> buildCanon(DefenseBack, x, Some vehicle.Description, vehicles.TankPosition)
            )
            |> List.ofSeq
        let canons =
            List.init numCanons (fun _ ->
                match defendingCoalition with
                | Axis -> vehicles.GermanAntiTankCanon
                | Allies -> vehicles.RussianAntiTankCanon
                |> fun x -> buildCanon(DefenseMiddle, x, None, vehicles.AntiTankPosition)
            )
        // Icons
        let title =
            sprintf "Battle %s (%d:%d)" region numAttackers numDefenders
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, center, title, defendingCoalition.ToCoalition, Mcu.IconIdValue.CoverArmorColumn)
        // Result
        { Supporters = support
          Attackers = attackers
          Defenders = defenders @ canons
          Icons = [ icon1; icon2 ]
          PlayerSpawns = players
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = []
                  member x.LcStrings = []
                  member x.SubGroups = [
                    for s in support do
                        yield s.All
                    for a in attackers do
                        yield a.All
                    for d in defenders @ canons do
                        yield d.All
                    yield icon1.All
                    yield icon2.All
                    for p in players do
                        yield p.All
                  ]
            }
        }

    /// All the Start MCU triggers
    member this.Starts =
        [
            for a in this.Attackers do
                yield a.Start
            for s in this.Supporters do
                yield s.Start
            for d in this.Defenders do
                yield d.Start
            for i in this.Icons do
                yield upcast i.Show
        ]

/// Identify regions with invaders, pick a battle area
let identifyBattleAreas (world : World) (state : WorldState) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    seq {
        for region, regState in List.zip world.Regions state.Regions do
            match regState.Owner with
            | Some defending when regState.HasInvaders ->
                let attacker =
                    try
                        region.Neighbours
                        |> Seq.map sg.GetRegion
                        |> Seq.filter (fun ngh -> ngh.Owner = Some defending.Other)
                        |> Seq.maxBy (fun ngh -> ngh.StorageCapacity(wg.GetRegion(ngh.RegionId), world.SubBlockSpecs))
                        |> fun ngh -> ngh.RegionId
                        |> Some
                    with
                    | _ -> None
                let bf = world.GetBattlefield(attacker, regState.RegionId)
                yield bf.DefenseAreaId, defending
            | _ ->
                ()
    }

/// Generate battlefields from invasions in column movement orders.
let generateBattlefields maxVehicles killRatio random store lcStore (world : World) (state : WorldState) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    [
        for bf, defending in identifyBattleAreas world state do
            let bf = wg.GetAntiTankDefenses(bf)
            let regState = sg.GetRegion(bf.Home)
            let numGuns =
                state.GetAmmoFillLevel(world, bf) * (float32 bf.MaxNumGuns)
                |> ceil
                |> int
            let defendingVehicles =
                regState.NumVehicles
                |> expandMap
            let numDefending = defendingVehicles.Length
            let defendingVehicles =
                defendingVehicles
                |> Array.shuffle random
                |> Array.truncate maxVehicles
                |> compactSeq
            let attackingVehicles =
                regState.NumInvadingVehicles
                |> expandMap
            let numAttacking =  attackingVehicles.Length
            let attackingVehicles =
                attackingVehicles
                |> Array.shuffle random
                |> Array.truncate maxVehicles
            yield Battlefield.Create(random, store, lcStore, bf.Position.Pos, bf.Position.Rotation, bf.Boundary, defending, numGuns, defendingVehicles, attackingVehicles, string bf.Home, numDefending * killRatio, numAttacking * killRatio)
    ]