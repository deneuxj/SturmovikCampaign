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

type private AreaLocation =
    | Back
    | Defense
    | Support
    | Offense

type Battlefield =
    { Supporters : RespawningCanon list // Artillery behind the attacking front, fires at defenses
      Attackers : RespawningTank list // Moving tanks
      Defenders : RespawningCanon list // Static tanks, defensive artillery and anti-tank canons
      All : McuUtil.IMcuGroup
    }
with
    static member Create(random : System.Random, store, center : Vector2, yori : float32, boundary : Vector2 list, defendingCoalition : CoalitionId, numCanons : int, defenders : Map<GroundAttackVehicle, int>, attackers : GroundAttackVehicle[]) =
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
                | Back -> 0.25f * r + 0.75f
                | Support -> 0.25f * r
                | Defense -> 0.25f * r + 0.5f
                | Offense -> 0.25f * r + 0.25f
            let r1 = random.NextDouble() |> float32
            let dx = (back * r0 + front * (1.0f - r0)) * dir
            let dz = (left * r1 + right * (1.0f - r1)) * side
            center + dx + dz
        // Get a random position within the boundary
        let getRandomPos(areaLocation) =
            Seq.initInfinite (fun _ -> getRandomPos areaLocation)
            |> Seq.find (fun v -> v.IsInConvexPolygon(boundary))
        // Build an attacking tank
        let buildTank(model : VehicleTypeData) =
            let tank = RespawningTank.Create(store, getRandomPos(Offense), getRandomPos(Back))
            tank.Tank.Model <- model.Model
            tank.Tank.Script <- model.Script
            tank.Tank.Country <- defendingCoalition.Other.ToCountry
            tank |> Choice1Of2
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCanon(model : VehicleTypeData, wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(Support), getRandomPos(Back))
            arty.Canon.Model <- model.Model
            arty.Canon.Script <- model.Script
            arty.Canon.Country <- defendingCoalition.Other.ToCountry
            arty |> Choice2Of2
        // Instantiate attacker blocks
        let attackers, support =
            attackers
            |> Seq.map (fun vehicle ->
                match defendingCoalition.Other, vehicle with
                | Allies, HeavyTank -> vehicles.RussianHeavyTank |> buildTank
                | Allies, MediumTank -> vehicles.RussianMediumTank |> buildTank
                | Allies, LightArmor -> (vehicles.RussianRocketArtillery, vehicles.AntiTankPosition) |> buildCanon
                | Axis, HeavyTank -> vehicles.GermanHeavyTank |> buildTank
                | Axis, MediumTank -> vehicles.GermanMediumTank |> buildTank
                | Axis, LightArmor -> (vehicles.GermanRocketArtillery, vehicles.AntiTankPosition) |> buildCanon
            )
            |> List.ofSeq
            |> List.partition (function Choice1Of2 _ -> true | _ -> false)
        let attackers = attackers |> List.map (function Choice1Of2 x -> x | _ -> failwith "Not a Choice1Of2")
        let support = support |> List.map (function Choice2Of2 x -> x | _ -> failwith "Not a Choice2Of2")
        // Instantiate defender blocks
        // Build a supporting object (dug-in tank or rocket artillery)
        let buildCanon(location, model : VehicleTypeData, wallModel : VehicleTypeData) =
            let arty = RespawningCanon.Create(store, getRandomPos(location), getRandomPos(Back))
            arty.Canon.Model <- model.Model
            arty.Canon.Script <- model.Script
            arty.Canon.Country <- defendingCoalition.ToCountry
            arty
        let defenders =
            defenders
            |> expandMap
            |> Seq.map (fun vehicle ->
                match defendingCoalition.Other, vehicle with
                | Allies, HeavyTank -> vehicles.RussianHeavyTank
                | Allies, MediumTank -> vehicles.RussianMediumTank
                | Allies, LightArmor -> vehicles.RussianRocketArtillery
                | Axis, HeavyTank -> vehicles.GermanHeavyTank
                | Axis, MediumTank -> vehicles.GermanMediumTank
                | Axis, LightArmor -> vehicles.GermanRocketArtillery
                |> fun x -> buildCanon(Back, x, vehicles.AntiTankPosition)
            )
            |> List.ofSeq
        let canons =
            List.init numCanons (fun _ ->
                match defendingCoalition with
                | Axis -> vehicles.GermanAntiTankCanon
                | Allies -> vehicles.RussianAntiTankCanon
                |> fun x -> buildCanon(Defense, x, vehicles.AntiTankPosition)
            )
        // Result
        { Supporters = support
          Attackers = attackers
          Defenders = defenders @ canons
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

/// Generate battlefields from invasions in column movement orders.
let generateBattlefields random store (world : World) (state : WorldState) (orders : ColumnMovement list) =
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
                yield Battlefield.Create(random, store, bf.Position.Pos, bf.Position.Rotation, bf.Boundary, defending, numGuns, region.NumVehicles, order.Composition)
            | _ ->
                ()
    ]