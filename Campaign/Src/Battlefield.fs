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
open SturmovikMission.Blocks.StaticDefenses
open SturmovikMission.Blocks.MapGraphics

type private AreaLocation =
    | DefenseBack
    | DefenseMiddle
    | AttackBack
    | AttackMiddle

type WWIData =
    { NumDefendingHowitzers : int
      Defenders : int
      NumAttackingHowitzers : int
      Attackers : int
    }

type WWIIData =
    { NumCannons : int
      Defenders : Map<GroundAttackVehicle, int>
      Attackers : GroundAttackVehicle[]
      IncludePlayerSpawns : bool
    }

type UnitData =
    | WWI of WWIData
    | WWII of WWIIData

type Battlefield =
    { Supporters : RespawningCanon list // Artillery behind the attacking front, fires at defenses
      Attackers : RespawningTank list // Moving tanks
      Defenders : RespawningCanon list // Static tanks, defensive artillery and anti-tank canons
      PlayerSpawns : PlayerTankSpawn list
      Icons : BattleIcons list
      All : McuUtil.IMcuGroup
    }
with
    static member Create(random : System.Random, store, lcStore, center : Vector2, yori : float32, boundary : Vector2 list, defendingCountry : CountryId, attackingCountry : CountryId, unitData : UnitData, region, numDefenders, numAttackers) =
        let defendingCoalition = defendingCountry.Coalition
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
                | DefenseBack -> 0.25f * r
                | AttackBack -> 1.0f - 0.25f * r
                | DefenseMiddle -> 0.25f * r + 0.25f
                | AttackMiddle -> 0.75f - 0.25f * r
            let r1 = random.NextDouble() |> float32
            let dx = (back * (1.0f - r0) + front * r0) * dir
            let dz = (left * (1.0f - r1) + right * r1) * side
            center + dx + dz
        // Get a random position within the boundary
        let getRandomPos(areaLocation) =
            Seq.initInfinite (fun _ -> getRandomPos areaLocation)
            |> Seq.find (fun v -> v.IsInConvexPolygon(boundary))
        // Player spawns
        let players =
            match unitData with
            | WWII { Defenders = defenders; Attackers = attackers; IncludePlayerSpawns = true } ->
                [
                    let defendersExist =
                        defenders
                        |> Map.toSeq
                        |> Seq.exists (fun (_, qty) -> qty > 0)
                    if defendersExist && attackers.Length > 0 then
                        let numDefendingHeavy =
                            defenders
                            |> Map.tryFind HeavyTank
                            |> Option.defaultValue 1
                        yield PlayerTankSpawn.Ceate(store, getRandomPos(DefenseBack), yori, defendingCountry.ToMcuValue, numDefendingHeavy)
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
                        yield PlayerTankSpawn.Ceate(store, getRandomPos(AttackBack), mirrored, attackingCountry.ToMcuValue, numAttackingHeavy)
                ]
            | _ ->
                []
        // Build an attacking tank
        let buildTank name (model : VehicleTypeData) =
            let tank = RespawningTank.Create(store, getRandomPos(AttackMiddle), getRandomPos(DefenseBack), attackingCountry.ToMcuValue)
            tank.Tank.Name <- sprintf "B-%s-A-%s" region name
            model.AssignTo(tank.Tank)
            tank |> Choice1Of2
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCanon name (model : VehicleTypeData, wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(AttackBack), getRandomPos(DefenseBack), attackingCountry.ToMcuValue)
            arty.Canon.Name <- sprintf "B-%s-A-%s" region name
            wallModel.AssignTo(arty.Wall)
            model.AssignTo(arty.Canon)
            arty |> Choice2Of2
        // Instantiate attacker blocks
        let attackers, support =
            match unitData with
            | WWII { Attackers = attackers } ->
                attackers
                |> Seq.map (fun vehicle ->
                    match defendingCoalition.Other, vehicle with
                    | Allies, HeavyTank -> vehicles.RussianHeavyTank |> buildTank vehicle.Description
                    | Allies, MediumTank -> vehicles.RussianMediumTank |> buildTank vehicle.Description
                    | Allies, LightArmor -> (vehicles.RussianRocketArtillery, vehicles.TankPosition) |> buildCanon vehicle.Description
                    | Axis, HeavyTank -> vehicles.GermanHeavyTank |> buildTank vehicle.Description
                    | Axis, MediumTank -> vehicles.GermanMediumTank |> buildTank vehicle.Description
                    | Axis, LightArmor -> (vehicles.GermanRocketArtillery, vehicles.TankPosition) |> buildCanon vehicle.Description
                )
                |> List.ofSeq
                |> List.partition (function Choice1Of2 _ -> true | _ -> false)
            | WWI { Attackers = attackers; NumAttackingHowitzers = howitzers } ->
                [],
                List.init attackers (fun _ ->
                    match defendingCoalition.Other with
                    | Axis -> (vehicles.GermanMachineGun, vehicles.MachineGunPosition) |> buildCanon LightArmor.Description
                    | Allies -> (vehicles.RussianMachineGun, vehicles.MachineGunPosition) |> buildCanon LightArmor.Description
                ) @
                List.init howitzers (fun _ ->
                    match defendingCoalition.Other with
                    | Axis -> (vehicles.GermanArtillery, vehicles.ArtilleryPosition) |> buildCanon LightArmor.Description
                    | Allies -> (vehicles.RussianArtillery, vehicles.MachineGunPosition) |> buildCanon LightArmor.Description)
        let attackers = attackers |> List.map (function Choice1Of2 x -> x | _ -> failwith "Not a Choice1Of2")
        let support = support |> List.map (function Choice2Of2 x -> x | _ -> failwith "Not a Choice2Of2")
        // Instantiate defender blocks
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCanon (location, model : VehicleTypeData, name , wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(location), getRandomPos(AttackBack), defendingCountry.ToMcuValue)
            match name with
            | Some name -> arty.Canon.Name <- sprintf "B-%s-D-%s" region name
            | None -> ()
            wallModel.AssignTo(arty.Wall)
            model.AssignTo(arty.Canon)
            arty
        let defenders =
            match unitData with
            | WWII { Defenders = defenders } ->
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
                    |> fun x -> buildCanon(DefenseBack, x, Some vehicle.Description, vehicles.TankPosition))
                |> List.ofSeq
            | WWI { Defenders = defenders } ->
                List.init defenders (fun _ ->
                    match defendingCoalition with
                    | Axis -> vehicles.GermanMachineGun
                    | Allies -> vehicles.RussianMachineGun
                    |> fun x -> buildCanon(DefenseBack, x, Some LightArmor.Description, vehicles.ArtilleryPosition))
        let canons =
            match unitData with
            | WWII { NumCannons = numCannons } ->
                List.init numCannons (fun _ ->
                    match defendingCoalition with
                    | Axis -> vehicles.GermanAntiTankCanon
                    | Allies -> vehicles.RussianAntiTankCanon
                    |> fun x -> buildCanon(DefenseMiddle, x, None, vehicles.AntiTankPosition))
            | WWI { NumDefendingHowitzers = howitzers } ->
                List.init howitzers (fun _ ->
                    match defendingCoalition with
                    | Axis -> vehicles.GermanArtillery
                    | Allies -> vehicles.RussianArtillery
                    |> fun x -> buildCanon(DefenseMiddle, x, None, vehicles.ArtilleryPosition))

        // Icons
        let icon1 = BattleIcons.Create(store, lcStore, center, yori, numAttackers, numDefenders, Defenders defendingCoalition.ToCoalition)
        let icon2 = BattleIcons.Create(store, lcStore, center, yori, numAttackers, numDefenders, Attackers defendingCoalition.Other.ToCoalition)
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
let generateBattlefields missionLength enablePlayerTanks maxVehicles maxAtGuns killRatio random store lcStore (world : World) (state : WorldState) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let ammoFill = state.GetAmmoFillLevelPerRegion(world, missionLength)
    [
        for bf, defending in identifyBattleAreas world state do
            let defendingCountry = world.CountryOfCoalition defending
            let attackingCountry = world.CountryOfCoalition defending.Other
            let bf = wg.GetAntiTankDefenses(bf)
            let regState = sg.GetRegion(bf.Home)
            let fill = ammoFill.TryFind(bf.Home) |> Option.defaultValue 0.0f |> max 0.0f |> min 1.0f
            let numGuns =
                fill * (float32 bf.MaxNumGuns)
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
            let unitData =
                if world.IsWWI then
                    WWI {
                        NumDefendingHowitzers = numGuns
                        Defenders = numDefending
                        NumAttackingHowitzers =
                            attackingVehicles
                            |> Seq.sumBy (function LightArmor -> 1 | _ -> 0)
                        Attackers =
                            attackingVehicles
                            |> Array.sumBy (function LightArmor -> 0 | _ -> 1)
                    }
                else
                    WWII {
                        NumCannons = numGuns |> min maxAtGuns
                        Defenders = defendingVehicles
                        Attackers = attackingVehicles
                        IncludePlayerSpawns = enablePlayerTanks
                    }
            yield Battlefield.Create(random, store, lcStore, bf.Position.Pos, bf.Position.Rotation, bf.Boundary, defendingCountry, attackingCountry, unitData, string bf.Home, numDefending * killRatio, numAttacking * killRatio)
    ]

type ArtilleryBattlefield =
    { SideA : RespawningCanon list
      SideB : RespawningCanon list
      Icons : IconDisplay list
      All : McuUtil.IMcuGroup
    }
with
    static member Create(random : System.Random, numPieces : int, store, lcStore, wg : WorldFastAccess, area : ArtilleryField, countryA : CountryId, countryB : CountryId) =
        // Get a random position within the bounding rectangle of artillery field
        let getRandomPos(isSideA : bool) =
            let dir = Vector2.FromYOri(float area.Position.Rotation)
            let back =
                area.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - area.Position.Pos, dir))
                |> Seq.min
            let front =
                area.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - area.Position.Pos, dir))
                |> Seq.max
            let side = Vector2.FromYOri (float area.Position.Rotation + 90.0)
            let left =
                area.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - area.Position.Pos, side))
                |> Seq.min
            let right =
                area.Boundary
                |> Seq.map (fun v -> Vector2.Dot(v - area.Position.Pos, side))
                |> Seq.max
            let r0 =
                let r = random.NextDouble() |> float32
                if isSideA then
                    0.1f * r
                else
                    1.0f - 0.1f * r
            let r1 = random.NextDouble() |> float32
            let dx = (back * (1.0f - r0) + front * r0) * dir
            let dz = (left * (1.0f - r1) + right * r1) * side
            area.Position.Pos + dx + dz
        // Get a random position within the boundary and the region
        let getRandomPos(isSideA) =
            Seq.initInfinite (fun _ -> getRandomPos isSideA)
            |> Seq.find (fun v ->
                v.IsInConvexPolygon(area.Boundary)
                &&
                if isSideA then
                    v.IsInConvexPolygon(wg.GetRegion(area.RegionA).Boundary)
                else
                    v.IsInConvexPolygon(wg.GetRegion(area.RegionB).Boundary))
        // Build a respawning artillery
        let buildArtillery(isSideA : bool, model : VehicleTypeData, wallModel : VehicleTypeData) =
            let country =
                if isSideA then
                    countryA
                else
                    countryB
            let arty = RespawningCanon.Create(store, getRandomPos(isSideA), getRandomPos(not isSideA), country.ToMcuValue)
            arty.Canon.Name <- Types.CannonObjectName
            wallModel.AssignTo(arty.Wall)
            model.AssignTo(arty.Canon)
            arty
        let modelA, modelB =
            match countryA.Coalition with
            | Axis -> vehicles.GermanArtillery, vehicles.RussianArtillery
            | Allies -> vehicles.RussianArtillery, vehicles.GermanArtillery
        let cannonsA =
            List.init numPieces (fun _ -> buildArtillery(true, modelA, vehicles.ArtilleryPosition))
        let cannonsB =
            List.init numPieces (fun _ -> buildArtillery(false, modelA, vehicles.ArtilleryPosition))
        let iconsA1, iconsA2 = IconDisplay.CreatePair(store, lcStore, area.SideAPos, "", countryA.Coalition.ToCoalition, Mcu.IconIdValue.CoverArtilleryPosition)
        let iconsB1, iconsB2 = IconDisplay.CreatePair(store, lcStore, area.SideBPos, "", countryB.Coalition.ToCoalition, Mcu.IconIdValue.CoverArtilleryPosition)
        let allIcons = [ iconsA1; iconsA2; iconsB1; iconsB2 ]
        // Result
        { SideA = cannonsA
          SideB = cannonsB
          Icons = allIcons
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = []
                  member x.LcStrings = []
                  member x.SubGroups = [
                    for c in cannonsA @ cannonsB do
                        yield c.All
                    for icon in allIcons do
                        yield icon.All
                  ]
            }
        }

    /// All the Start MCU triggers
    member this.Starts =
        [
            for a in this.SideA @ this.SideB do
                yield a.Start
            for icon in this.Icons do
                yield icon.Show :> Mcu.McuTrigger
        ]

/// Identify artillery fields between enemy regions without ongoing battles.
/// Return the field and coalition of side A.
let identifyArtilleryFields (world : World) (state : WorldState) =
    let sg = state.FastAccess
    world.ArtilleryFields
    |> List.choose(fun area ->
        let regionA = sg.GetRegion(area.RegionA)
        let regionB = sg.GetRegion(area.RegionB)
        if regionA.HasInvaders || regionB.HasInvaders then
            None
        else
            match regionA.Owner, regionB.Owner with
            | Some coalitionA, Some coalitionB when coalitionA <> coalitionB ->
                Some (area, coalitionA)
            | _ ->
                None)

/// Identify suitable artillery fields, and pick up to maxNum at random, then generate the battles.
let generateArtilleryFields random numPieces maxNum store lcStore world state =
    identifyArtilleryFields world state
    |> List.toArray
    |> Array.shuffle random
    |> Array.truncate maxNum
    |> Array.map (fun (area, sideA) ->
        let countryA = world.CountryOfCoalition sideA
        let countryB = world.CountryOfCoalition sideA.Other
        ArtilleryBattlefield.Create(random, numPieces, store, lcStore, world.FastAccess, area, countryA, countryB))