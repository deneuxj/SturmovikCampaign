﻿/// Logic and entities of a battlefield.
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
      Icons : IconDisplay list
      All : McuUtil.IMcuGroup
    }
with
    static member Create(random : System.Random, store, lcStore, center : Vector2, yori : float32, boundary : Vector2 list, defendingCoalition : CoalitionId, numCanons : int, defenders : Map<GroundAttackVehicle, int>, attackers : GroundAttackVehicle[], namePrefix) =
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
        // Build an attacking tank
        let buildTank name (model : VehicleTypeData) =
            let tank = RespawningTank.Create(store, getRandomPos(AttackMiddle), getRandomPos(DefenseBack))
            tank.Tank.Name <- namePrefix + "A-" + name
            tank.Tank.Model <- model.Model
            tank.Tank.Script <- model.Script
            tank.Tank.Country <- defendingCoalition.Other.ToCountry
            tank |> Choice1Of2
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCanon(model : VehicleTypeData, wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(AttackBack), getRandomPos(DefenseBack))
            arty.Wall.Model <- wallModel.Model
            arty.Wall.Script <- wallModel.Script
            arty.Canon.Model <- model.Model
            arty.Canon.Script <- model.Script
            arty.Canon.Country <- defendingCoalition.Other.ToCountry
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
            let arty = RespawningCanon.Create(store, getRandomPos(location), getRandomPos(AttackBack))
            match name with
            | Some name -> arty.Canon.Name <- namePrefix + "D-" + name
            | None -> ()
            arty.Wall.Model <- wallModel.Model
            arty.Wall.Script <- wallModel.Script
            arty.Canon.Model <- model.Model
            arty.Canon.Script <- model.Script
            arty.Canon.Country <- defendingCoalition.ToCountry
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
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, center, "Ground battle", defendingCoalition.ToCoalition, Mcu.IconIdValue.CoverArmorColumn)
        // Result
        { Supporters = support
          Attackers = attackers
          Defenders = defenders @ canons
          Icons = [ icon1; icon2 ]
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

/// Generate battlefields from invasions in column movement orders.
let generateBattlefields (maxVehicles) random store lcStore (world : World) (state : WorldState) (orders : ColumnMovement list) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    [
        for order in orders do
            match sg.GetRegion(order.Start), sg.GetRegion(order.Destination) with
            | { Owner = Some attacking } as regStart, ({ Owner = Some defending } as region) when attacking <> defending ->
                let bf = world.GetBattlefield(order.Start, order.Destination)
                let numGuns =
                    state.GetAmmoFillLevel(world, region.RegionId, regStart.RegionId) * (float32 bf.MaxNumGuns)
                    |> ceil
                    |> int
                let namePrefix = sprintf "B-%s-" (order.OrderId.AsString())
                let defendingVehicles =
                    region.NumVehicles
                    |> expandMap
                    |> Array.shuffle random
                    |> Array.truncate maxVehicles
                    |> compactSeq
                let attackingVehicles =
                    order.Composition
                    |> Array.shuffle random
                    |> Array.truncate maxVehicles
                yield Battlefield.Create(random, store, lcStore, bf.Position.Pos, bf.Position.Rotation, bf.Boundary, defending, numGuns, defendingVehicles, attackingVehicles, namePrefix)
            | _ ->
                ()
    ]