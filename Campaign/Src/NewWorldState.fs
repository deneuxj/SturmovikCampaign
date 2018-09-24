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

/// Given a an old world state and mission result data, produce an updated world state
module Campaign.NewWorldState

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction
open Util
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.PlaneModel
open System.Numerics
open VectorExtension
open SturmovikMission.Blocks.BlocksMissionData
open FSharp.Control
open Campaign.AutoOrder

let private logger = NLog.LogManager.GetCurrentClassLogger()

/// Add production units according to production priorities
let applyProduction (dt : float32<H>) (world : World) (coalition : CoalitionId) (priorities : ProductionPriorities) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let vehiclePrio, energyPrio =
        let total =
            priorities.PrioritySupplies + priorities.PriorityVehicle
        if total <= 0.0f<E> then
            0.0f, 0.0f
        else
            priorities.PriorityVehicle / total, priorities.PriorityVehicle / total
    let regions =
        [
            for region, regState in Seq.zip world.Regions state.Regions do
                if regState.Owner = Some coalition && not(List.isEmpty region.Production) then
                    let vehiclePrio, energyprio =
                        // Avoid accumulating supplies. Use on tanks instead.
                        if regState.Products.Supplies >= 50.0f<E> then
                            1.0f, 0.0f
                        else
                            vehiclePrio, energyPrio
                    let energy = dt * regState.ProductionCapacity(region, world.SubBlockSpecs, world.ProductionFactor)
                    let supplies = regState.Products.Supplies + energyPrio * energy
                    let vehicles =
                        let oldValue =
                            Map.tryFind priorities.Vehicle regState.Products.Vehicles
                            |> Option.defaultVal 0.0f<E>
                        let newValue = oldValue + vehiclePrio * energy
                        Map.add priorities.Vehicle newValue regState.Products.Vehicles
                    let assignment = { regState.Products with Supplies = supplies; Vehicles = vehicles }
                    yield { regState with Products = assignment }
                else
                    yield regState
        ]
    // Add the most needed kind of plane to the rear airfield
    let airfields =
        [
            let random = System.Random()
            let rear = world.RearAirfields.TryFind(coalition)
            for af, afs in List.zip world.Airfields state.Airfields do
                if Some af.AirfieldId = rear then
                    let planeType = AutoOrder.pickPlaneToProduce coalition world state
                    // Pick the plane model of that type that has smallest number at the rear airfield
                    let candidates = world.PlaneSet.AllPlanesOfType(planeType, coalition) |> Array.shuffle random
                    let plane =
                        try
                            candidates
                            |> Array.map (fun plane -> plane, afs.NumPlanes.TryFind plane |> Option.defaultValue 0.0f)
                            |> Array.minBy snd
                            |> fst
                            |> Some
                        with
                        | _ -> None
                    // Weird case: No plane could be picked. Can only happen with broken planesets that lack models of a certain type
                    let plane =
                        match plane with
                        | Some _ -> plane
                        | None ->
                            try
                                world.PlaneSet.AllModels
                                |> Seq.minBy (fun _ -> random.NextDouble())
                                |> Some
                            with
                            | _ -> None
                    match plane with
                    | Some model ->
                        let fallOff =
                            2.0 - (state.Date - world.StartDate).TotalDays / 7.0
                            |> max 0.0
                            |> min 1.0
                            |> float32
                        let produced = fallOff * world.PlaneProduction * dt / model.Cost
                        let oldValue = afs.NumPlanes |> Map.tryFind model |> Option.defaultVal 0.0f
                        let newValue = oldValue + produced
                        yield { afs with
                                    NumPlanes = Map.add model newValue afs.NumPlanes
                        }
                    | None ->
                        // Another weird case: No plane at all in the given plane set and coalition. Don't produce any plane.
                        yield afs
                else
                    // Not the rear airfield. Don't produce any plane.
                    yield afs
        ]
    { state with Regions = regions; Airfields = airfields }

/// A region received supplies from production.
type Resupplied = {
    Region : RegionId
    Energy : float32<E>
}

/// Statistics about vehicles produced in a round.
type NewVehiclesSummary = {
    Planes : Map<PlaneModel, int>
    Tanks : Map<GroundAttackVehicle, int>
}
with
    static member Empty = { Planes = Map.empty; Tanks = Map.empty }

    member this.AddPlanes(planes) =
        { this with Planes = Util.addList this.Planes planes }

    member this.AddTanks(tanks) =
        { this with Tanks = Util.addList this.Tanks tanks }

/// Create new plane and vehicle instances if enough energy has been accumulated and convert supplies to local Resupplied events.
let convertProduction (world : World) (state : WorldState) =
    // Capacity and supply levels or regions including airfields
    let capacities = computeActualStorageCapacity world state
    let supplies = computeStorage world state
    let afStates =
        state.Airfields
        |> Seq.map (fun af -> af.AirfieldId, af)
        |> Map.ofSeq
        |> ref
    let regions =
        state.Regions
        |> Seq.map (fun region -> region.RegionId, region)
        |> Map.ofSeq
        |> ref
    let resupplied = ref []
    let newAxisVehicles = ref NewVehiclesSummary.Empty
    let newAlliesVehicles = ref NewVehiclesSummary.Empty
    for region, regState in Seq.zip world.Regions state.Regions do
        // Freshly produced planes
        let af =
            world.Airfields
            |> List.tryFind (fun af -> af.Region = region.RegionId)
        let newPlanes, remainingPlanes =
            match af with
            | Some af ->
                regState.Products.Planes
                |> Map.toSeq
                |> Seq.fold (fun (newPlanes, remainingPlanes) (plane, energy) ->
                    assert(energy >= 0.0f<E>)
                    let numNewPlanes = floor(energy / plane.Cost)
                    let energyLeft = energy - numNewPlanes * plane.Cost
                    (plane, int numNewPlanes) :: newPlanes, Map.add plane energyLeft remainingPlanes
                ) ([], Map.empty)
            | None ->
                [], regState.Products.Planes
        let remainingPlanes =
            // Retain only the new production choices. 0.1f instead of 0.0f to avoid float precision loss issues
            Map.filter (fun _ energy -> energy > 0.1f<E>) remainingPlanes

        // Supplies -> storage
        let supplySpace = Map.tryFind region.RegionId capacities |> Option.defaultValue 0.0f<E>
        let supplyLevel = Map.tryFind region.RegionId supplies |> Option.defaultValue 0.0f<E>
        let supplySpaceAvailable = max 0.0f<E> (supplySpace - supplyLevel)
        let transferedEnergy = min supplySpaceAvailable regState.Products.Supplies
        resupplied := { Region = region.RegionId; Energy = transferedEnergy } :: !resupplied

        // Use left-over supplies for light tanks
        let suppliesLeft = regState.Products.Supplies - transferedEnergy
        let vehicleProducts = regState.Products.Vehicles
        let lightEnergy = vehicleProducts.TryFind(LightArmor) |> Option.defaultValue 0.0f<E>
        let vehicleProducts = vehicleProducts.Add(LightArmor, lightEnergy + suppliesLeft)

        // Freshly produced vehicles
        let newVehicles, remainingVehicles =
            vehicleProducts
            |> Map.toSeq
            |> Seq.fold (fun (newVehicles, remainingVehicles) (vehicle, energy) ->
                assert(energy >= 0.0f<E>)
                let numNewVehicles = floor(energy / vehicle.Cost)
                let energyLeft = energy - numNewVehicles * vehicle.Cost
                (vehicle, int numNewVehicles) :: newVehicles, Map.add vehicle energyLeft remainingVehicles
            ) ([], Map.empty)

        // Record plane and tank production
        match regState.Owner with
        | Some Axis ->
            newAxisVehicles := newAxisVehicles.Value.AddPlanes(newPlanes).AddTanks(newVehicles)
        | Some Allies ->
            newAlliesVehicles := newAlliesVehicles.Value.AddPlanes(newPlanes).AddTanks(newVehicles)
        | None ->
            ()
        // Remove produced things from ongoing production
        let assignment = { regState.Products with Planes = remainingPlanes; Vehicles = remainingVehicles; Supplies = 0.0f<E> }
        // Add produced vehicles to region
        let numVehicles =
            newVehicles
            |> List.fold (fun numVehicles (vehicle, qty) ->
                let newNum =
                    match Map.tryFind vehicle numVehicles with
                    | Some x -> x + qty
                    | None -> qty
                Map.add vehicle newNum numVehicles
            ) regState.NumVehicles
        regions := Map.add regState.RegionId { regState with Products = assignment; NumVehicles = numVehicles } !regions
        // Add produced planes to airfield, if there is one.
        match af with
        | Some af ->
            let afState = afStates.Value.[af.AirfieldId]
            let numPlanes =
                newPlanes
                |> List.fold (fun numPlanes (plane, qty) ->
                    let newNum =
                        match Map.tryFind plane numPlanes with
                        | Some x -> x + float32 qty
                        | None -> float32 qty
                    Map.add plane newNum numPlanes
                ) afState.NumPlanes
            let afState = { afState with NumPlanes = numPlanes }
            afStates := Map.add af.AirfieldId afState !afStates
        | None -> ()
    // Result
    { state with
        Airfields = state.Airfields |> List.map (fun af -> afStates.Value.[af.AirfieldId])
        Regions = state.Regions |> List.map (fun region -> regions.Value.[region.RegionId]) },
    (resupplied.Value,
     newAxisVehicles.Value,
     newAlliesVehicles.Value)

/// Consume supplies to heal buildings in a group
/// healths : Array of healths for the buildings in the group
/// cost : Cost of repairing the entire group
/// available : Supplies available for repairs
/// healLimit : Maximum amount of supplies allowed to be used for healing
let healBuildings(healths : float32[], cost : float32<E>, available : float32<E>, healLimit : float32<E>) =
    let costPerBuilding = lazy (cost / float32 healths.Length)
    let reversedHealths, available, healLimit =
        healths
        |> Array.fold (fun (healths, available, healLimit) h ->
            let spend =
                ((1.0f - h) * costPerBuilding.Value)
                |> min available
                |> min healLimit
            let repaired = spend / costPerBuilding.Value
            (h + repaired) :: healths, available - spend, healLimit - spend) ([], available, healLimit)
    let healths =
        reversedHealths
        |> List.rev
        |> Array.ofList
    healths, available

/// Consume supplies to heal buildings
/// groupHealths : List of health levels for each group
/// groupDescriptions : List of building group descriptions, must match healths
/// energy : input energy
let computeHealing(subBlocksSpecs, groupHealths : float32[] list, groupDescriptions, energy, healLimit) =
    let energyPerGroup =
        groupDescriptions
        |> List.map (fun (x : StaticGroup) -> x.RepairCost(subBlocksSpecs))
    let healths, energy =
        List.zip groupHealths energyPerGroup
        |> List.fold (fun (groups, available) (healths, healthCost : float32<E>) ->
            let healths, available = healBuildings(healths, healthCost, available, healLimit)
            healths :: groups, available
        ) ([], energy)
    assert(energy >= 0.0f<E>)
    let healths = List.rev healths
    healths, energy

/// Compute how many supplies were removed from each region's stores
let computeShipped (orders : ResupplyOrder list) (shipped : SuppliesShipped list) =
    let orders =
        orders
        |> List.map (fun order -> order.OrderId, order)
        |> Map.ofList
    let shipped =
        let data =
            shipped
            |> Seq.groupBy (fun sup -> orders.[sup.OrderId].Convoy.Start)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> orders.[sup.OrderId].Convoy.TransportedSupplies))
        Map.ofSeq data
    shipped

/// Compute how many supplies can be potentially added to each region's stores
let computeDelivered (orders : ResupplyOrder list) (shipped : SuppliesShipped list) (blocked : VehiclesBlocked list) (damages : Damage list) =
    let blocked =
        blocked
        |> Seq.map (fun bl -> bl.OrderId) // No need to include the rank offset, convoys are never split.
        |> Set.ofSeq
    let damages =
        let data =
            damages
            |> Seq.groupBy (fun dmg -> dmg.Object)
            |> Seq.map (fun (victim, damages) -> victim, damages |> Seq.map (fun dmg -> dmg.Data))
        Map.ofSeq data
    let orders =
        orders
        |> List.map (fun order -> order.OrderId, order)
        |> Map.ofList
    let arrived =
        let data =
            shipped
            |> Seq.filter (fun sup -> blocked.Contains(sup.OrderId) |> not)
            |> Seq.groupBy (fun sup -> orders.[sup.OrderId].Convoy.Destination)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> orders.[sup.OrderId].Convoy.TransportedSupplies))
        Map.ofSeq data
    let arrived =
        // Supplies that could not reach their destination because of a destroyed bridge are returned to the starting region
        let returned =
            shipped
            |> Seq.filter (fun sup -> blocked.Contains(sup.OrderId))
            |> Seq.groupBy (fun sup -> orders.[sup.OrderId].Convoy.Start)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> orders.[sup.OrderId].Convoy.TransportedSupplies))
        returned
        |> Seq.fold (fun arrived (reg, supplies) ->
            let x = Map.tryFind reg arrived |> Option.defaultValue 0.0f<E>
            Map.add reg (x + supplies) arrived) arrived
    let arrived =
        damages
        |> Map.fold (fun arrived victim damages ->
            let damages = damages |> Seq.sumBy (fun damage -> damage.Amount)
            match victim with
            | DamagedObject.Convoy vehicle ->
                match orders.TryFind vehicle.OrderId with
                | Some order ->
                    let oldValue =
                        Map.tryFind order.Convoy.Destination arrived |> Option.defaultVal 0.0f<E>
                    let toBeRemoved =
                        match order.Means with
                        | ByRail -> order.Convoy.TransportedSupplies * (min damages 1.0f)
                        | ByRoad ->
                            if vehicle.Rank > 0 then
                                ResupplyOrder.TruckCapacity
                            else
                                // Lead car, does not transport anything
                                0.0f<E>
                        | ByRiverShip
                        | BySeaShip ->
                            if vehicle.Rank >= 0 && vehicle.Rank < 2 && damages >= 1.0f then
                                ResupplyOrder.ShipCapacity
                            else
                                0.0f<E>
                        | ByAir _ -> order.Convoy.TransportedSupplies
                    let newValue = oldValue - toBeRemoved |> max 0.0f<E>
                    Map.add order.Convoy.Destination newValue arrived
                | None ->
                    arrived
            | _ -> arrived
        ) arrived
    arrived

/// Compute how many supplies each region produced
let computeSuppliesProduced (newSupplies : Resupplied list) =
    let newSupplies =
        newSupplies
        |> List.map (fun sup -> sup.Region, sup.Energy)
        |> Map.ofSeq
    newSupplies

/// Compute supplies delivered by air cargo in each region
let computeCargoSupplies (wg : WorldFastAccess) (landed : Landed list) =
    landed
    |> List.map (fun landed -> wg.GetAirfield(landed.Airfield).Region, landed.Cargo)
    |> List.groupBy fst
    |> List.map (fun (af, xs) -> af, xs |> List.sumBy snd)
    |> Map.ofList

/// Remove supplies consummed by guns defending the region
let applyConsumption (dt : float32<H>) (world : World) (state : WorldState) =
    let sg = state.FastAccess

    let frontRegions =
        computeFrontLine false world state.Regions
        |> Seq.collect (fun (regA, regB) -> [regA; regB])
        |> Set.ofSeq

    let distanceToFront =
        computeDistance false (fun world -> world.Roads @ world.Rails) (fun region -> sg.GetRegion(region).Owner) (frontRegions.Contains) world

    let needs =
        computeFullDefenseNeeds world
        |> Map.ofList

    let operationCosts = state.GetOperatingCostPerRegion world

    let regions =
        [
            for region, regState in List.zip world.Regions state.Regions do
                let forcesK = regState.Supplies / needs.[region.RegionId] |> min 1.0f
                let fullCost = operationCosts.[region.RegionId]
                let distanceFactor =
                    match distanceToFront.TryFind region.RegionId with
                    | None -> 0.0f
                    | Some 0 -> 1.0f
                    | Some 1 -> 0.5f
                    | Some _ -> 0.0f
                let cost = fullCost * distanceFactor * forcesK
                let supplies = regState.Supplies - dt * cost |> max 0.0f<E>
                yield { regState with Supplies = supplies }
        ]

    { state with Regions = regions }

/// Remove shipped supplies from region of origin, apply damages due to attacks
let applyDamages (world : World) (state : WorldState) (shipped : SuppliesShipped list) (damages : Damage list) (orders : ResupplyOrder list) =
    let wg = WorldFastAccess.Create world

    let shipped = computeShipped orders shipped

    let damages =
        let data =
            damages
            |> Seq.groupBy (fun dmg -> dmg.Object)
            |> Seq.map (fun (victim, damages) -> victim, damages |> Seq.map (fun dmg -> dmg.Data))
        Map.ofSeq data

    let applyDamage health =
        Option.map (fun damages ->
            damages
            |> Seq.sumBy (fun data -> data.Amount)
            |> (-) health
            |> max 0.0f)
        >> Option.defaultVal health

    let regionsAfterShipping =
        [
            for regState in state.Regions do
                let newStored =
                    match Map.tryFind regState.RegionId shipped with
                    | Some sent ->
                        regState.Supplies - sent
                    | None ->
                        regState.Supplies
                    |> max 0.0f<E>
                yield { regState with Supplies = newStored }
        ]
    let airfieldsAfterDamages =
        [
            for afState in state.Airfields do
                let af = wg.GetAirfield(afState.AirfieldId)
                let planes =
                    afState.NumPlanes
                    |> Map.map (fun plane qty ->
                        match Map.tryFind (ParkedPlane(afState.AirfieldId, plane)) damages with
                        | Some damages ->
                            let totalDamages =
                                damages
                                |> Seq.sumBy (fun data -> data.Amount)
                            qty - totalDamages
                        | None ->
                            qty)
                let capacityBeforeDamages = afState.StorageCapacity(af, world.SubBlockSpecs)
                let storeHealth =
                    let subBlocksOf = wg.GetAirfieldStorageSubBlocks af.AirfieldId
                    afState.StorageHealth
                    |> List.mapi (fun group health ->
                        let buildings = subBlocksOf group
                        // For each building in the group, apply all damage that affects it
                        buildings
                        |> Array.map2 (fun h idx ->
                            Map.tryFind (Airfield(afState.AirfieldId, group, idx)) damages
                            |> applyDamage h) health)
                let afState = { afState with
                                    NumPlanes = planes
                                    StorageHealth = storeHealth }
                let capacityAfterDamages = afState.StorageCapacity(af, world.SubBlockSpecs)
                let factor =
                    if capacityAfterDamages = 0.0f<E> then
                        0.0f
                    elif capacityBeforeDamages = 0.0f<E> then
                        0.0f
                    else
                        capacityAfterDamages / capacityBeforeDamages
                let afState = { afState with Supplies = if afState.Supplies > 0.0f<E> then factor * afState.Supplies else afState.Supplies }
                yield afState
        ]
    let regionsAfterDamages =
        [
            for regState in regionsAfterShipping do
                let region = wg.GetRegion regState.RegionId
                let prodHealth =
                    let subBlocksOf = wg.GetRegionProductionSubBlocks region.RegionId
                    regState.ProductionHealth
                    |> List.mapi (fun group health ->
                        let buildings = subBlocksOf group
                        buildings
                        |> Array.map2 (fun h idx ->
                            Map.tryFind (Production(region.RegionId, group, idx)) damages
                            |> applyDamage h) health)
                let capacityBeforeDamages = regState.StorageCapacity(region, world.SubBlockSpecs)
                let storeHealth =
                    let subBlocksOf = wg.GetRegionStorageSubBlocks region.RegionId
                    regState.StorageHealth
                    |> List.mapi (fun group health ->
                        let buildings = subBlocksOf group
                        buildings
                        |> Array.map2 (fun h idx ->
                            Map.tryFind (Storage(region.RegionId, group, idx)) damages
                            |> applyDamage h) health)
                let regState = { regState with ProductionHealth = prodHealth; StorageHealth = storeHealth }
                let capacityAfterDamages = regState.StorageCapacity(region, world.SubBlockSpecs)
                let factor =
                    if capacityAfterDamages = 0.0f<E> then
                        0.0f
                    elif capacityBeforeDamages = 0.0f<E> then
                        0.0f
                    else
                        capacityAfterDamages / capacityBeforeDamages
                let damageToVehicle vehicleType =
                    damages
                    |> Map.tryFind (Vehicle(region.RegionId, vehicleType))
                    |> Option.map (Seq.sumBy (fun data -> data.Amount))
                    |> Option.defaultVal 0.0f
                    |> (round >> int)
                let subtract =
                    GroundAttackVehicle.AllVehicles
                    |> List.map (fun x -> x, damageToVehicle x)
                    |> Map.ofList
                let numVehicles =
                    subMaps regState.NumVehicles subtract
                    |> Map.map (fun _ v -> max v 0)
                let regState = { regState with Supplies = factor * regState.Supplies; NumVehicles = numVehicles }
                yield regState
        ]
    let regionsAfterDamagesToDefenses =
        [
            for region, regState in List.zip world.Regions regionsAfterDamages do
                let damagesToDefenses =
                    Seq.append world.AntiAirDefenses world.AntiTankDefenses
                    |> Seq.filter (fun area -> area.Home = region.RegionId)
                    |> Seq.sumBy (fun area ->
                        let damagedCannons =
                            Map.tryFind (Cannon(area.DefenseAreaId)) damages
                            |> Option.defaultVal Seq.empty
                            |> Seq.sumBy (fun data -> data.Amount)
                        let damagedLightMachineGuns =
                            Map.tryFind (LightMachineGun(area.DefenseAreaId)) damages
                            |> Option.defaultVal Seq.empty
                            |> Seq.sumBy (fun data -> data.Amount)
                        let damagedHeavyMachineGuns =
                            Map.tryFind (HeavyMachineGun(area.DefenseAreaId)) damages
                            |> Option.defaultVal Seq.empty
                            |> Seq.sumBy (fun data -> data.Amount)
                        damagedCannons * cannonCost + damagedLightMachineGuns * lightMachineGunCost + damagedHeavyMachineGuns * heavyMachineGunCost)
                yield
                    if damagesToDefenses > 0.0f<E> then
                        { regState with Supplies = max 0.0f<E> (regState.Supplies - damagesToDefenses) }
                    else
                        regState
        ]
    { state with
        Regions = regionsAfterDamagesToDefenses
        Airfields = airfieldsAfterDamages }

/// Transfer supplies and apply repairs
let applyResupplies (dt : float32<H>) (world : World) (state : WorldState) newSupplies =
    let wg = WorldFastAccess.Create world
    let regionNeeds = state.GetAmmoCostPerRegion world
    let healLimit = world.RepairSpeed * dt

    // Subtract an amount from energy reserve first, and then from funds for repairs first
    let subtract amount (energy, forRepairs) =
        let fromEnergy = min energy amount
        (energy - fromEnergy, forRepairs - (amount - fromEnergy))

    // Repair and resupply regions
    let regionsAfterSupplies =
        [
            for regState in state.Regions do
                let region = wg.GetRegion regState.RegionId
                let energy =
                    Map.tryFind region.RegionId newSupplies
                    |> Option.defaultValue 0.0f<E>
                let forRepairs = min energy (healLimit * 5.0f)
                let energy = energy - forRepairs
                // Highest prio: repair production
                let prodHealth, forRepairs =
                    computeHealing(
                        world.SubBlockSpecs,
                        regState.ProductionHealth,
                        region.Production,
                        forRepairs,
                        healLimit)
                // Second prio: fill up region supplies
                let storeCapacity = regState.StorageCapacity(region, world.SubBlockSpecs)
                // Consume energy to fill up supplies up to what's needed
                let needs =
                    regionNeeds.TryFind regState.RegionId
                    |> Option.defaultVal 0.0f<E>
                    |> max 0.0f<E>
                let fillTarget = min needs storeCapacity
                let toSupplies =
                    fillTarget - regState.Supplies
                    |> min (energy + forRepairs)
                    |> max 0.0f<E>
                let supplies = regState.Supplies + toSupplies
                let energy, forRepairs = subtract toSupplies (energy, forRepairs)
                // Last: repair storage
                let storeHealth, forRepairs =
                    computeHealing(
                        world.SubBlockSpecs,
                        regState.StorageHealth,
                        region.Storage,
                        forRepairs,
                        healLimit)
                yield
                    { regState with ProductionHealth = prodHealth; StorageHealth = storeHealth; Supplies = supplies },
                    (energy, forRepairs)
        ]
    let newSupplies =
        regionsAfterSupplies
        |> List.map (fun (regState, energy) -> regState.RegionId, energy)
        |> Map.ofList
    let regionsAfterSupplies =
        regionsAfterSupplies
        |> List.map fst
    // Distribute supplies between regions and airfields
    let airfieldsDistribution, regSupplies =
        let x, regSupplies =
            state.Airfields
            |> List.fold (fun (airfields, regionEnergies) afState ->
                let af = wg.GetAirfield(afState.AirfieldId)
                // take from what's left of incoming supplies
                let energy, forRepairs =
                    Map.tryFind af.Region regionEnergies
                    |> Option.defaultValue (0.0f<E>, 0.0f<E>)
                // repair airfield storage
                let storeHealth, forRepairs =
                    computeHealing(world.SubBlockSpecs, afState.StorageHealth, af.Storage, forRepairs, healLimit)
                let afState = { afState with StorageHealth = storeHealth }
                // fill storage
                let bombNeeds =
                    afState.BombNeeds * bombCost
                    |> max (1000.0f<K> * bombCost)
                let fillTarget = min bombNeeds (afState.StorageCapacity(af, world.SubBlockSpecs))
                let toAfSupplies =
                    fillTarget - afState.Supplies
                    |> max 0.0f<E>
                    |> min (energy + forRepairs)
                let energy, forRepairs = subtract toAfSupplies (energy, forRepairs)
                // what goes over the bomb storage fill target goes back to the region
                let backToRegion =
                    afState.Supplies + toAfSupplies - fillTarget
                    |> max 0.0f<E>
                // result
                let afState = { afState with Supplies = afState.Supplies + toAfSupplies - backToRegion }
                let energy = energy + backToRegion
                afState :: airfields, Map.add af.Region (energy, forRepairs) regionEnergies
            ) ([], newSupplies)
        List.rev x, regSupplies
    let regionsAfterDistribution =
        [
            for regState, region in List.zip regionsAfterSupplies world.Regions do
                // Energy back from airfields; excess is returned
                let backFromAf, forRepairs =
                    regSupplies.TryFind regState.RegionId
                    |> Option.defaultValue (0.0f<E>, 0.0f<E>)
                let newEnergy =
                    backFromAf + regState.Supplies + forRepairs
                    |> min (regState.StorageCapacity(region, world.SubBlockSpecs))
                let transferred = newEnergy - regState.Supplies
                yield
                    { regState with Supplies = newEnergy },
                    newEnergy - transferred
        ]
    let regSupplies =
        regionsAfterDistribution
        |> List.map (fun (region, energy) -> region.RegionId, energy)
        |> Map.ofList
    { state with
        Regions = regionsAfterDistribution |> List.map fst
        Airfields = airfieldsDistribution },
    regSupplies

/// Distribute cargo to airfields
let distributeCargo (world : World) (state : WorldState) (cargo : Map<RegionId, float32<E>>) =
    // Transfer according to needs, limited by capacity
    let afs, cargo =
        List.zip world.Airfields state.Airfields
        |> List.fold(fun (afStates, cargo) (af, afState) ->
            let needs = afState.BombNeeds * bombCost
            let capacity = afState.StorageCapacity(af, world.SubBlockSpecs)
            let available =
                Map.tryFind af.Region cargo
                |> Option.defaultValue 0.0f<E>
            let transferred =
                (min needs capacity) - afState.Supplies
                |> min available
                |> max 0.0f<E>
            { afState with Supplies = afState.Supplies + transferred } :: afStates, Map.add af.Region (available - transferred) cargo
        ) ([], cargo)
    // Transfer anything that's left according to capacity
    let afs, _ =
        List.zip world.Airfields (List.rev afs)
        |> List.fold(fun (afStates, cargo) (af, afState) ->
            let capacity = afState.StorageCapacity(af, world.SubBlockSpecs)
            let available =
                Map.tryFind af.Region cargo
                |> Option.defaultValue 0.0f<E>
            let transferred =
                capacity - afState.Supplies
                |> min available
                |> max 0.0f<E>
            { afState with Supplies = afState.Supplies + transferred } :: afStates, Map.add af.Region (available - transferred) cargo
        ) ([], cargo)
    { state with
        Airfields = List.rev afs }

/// Captured planes are converted to resources
let convertCapturedPlanes (wg : WorldFastAccess) (state : WorldState) =
    let sg = state.FastAccess
    let mutable regions = Map.empty
    let airfields =
        [
            for afs in state.Airfields do
                let af = wg.GetAirfield(afs.AirfieldId)
                match sg.GetRegion(af.Region).Owner with
                | Some coalition ->
                    let captured =
                        afs.NumPlanes
                        |> Map.toSeq
                        |> Seq.sumBy (fun (model, qty) ->
                            if model.Coalition <> coalition then
                                model.Cost * qty
                            else
                                0.0f<E>)
                    let numPlanes =
                        afs.NumPlanes
                        |> Map.filter (fun model _ -> model.Coalition = coalition)
                    let x =
                        Map.tryFind af.Region regions
                        |> Option.defaultValue 0.0f<E>
                    regions <- Map.add af.Region (x + captured) regions
                    yield { afs with NumPlanes = numPlanes }
                | None ->
                    yield afs
        ]
    airfields, regions

/// Apply damages to airfields and regions, then repair and resupply according to resupplies, local production and cargo deliveries
let applyDamagesAndResupplies (mustConvertCapturedPlanes : bool) (dt : float32<H>) (world : World) (state : WorldState) (shipped : SuppliesShipped list) (blocked : VehiclesBlocked list) (damages : Damage list) (orders : ResupplyOrder list) (newSupplies : Resupplied list) (cargo : Landed list) =
    let wg = world.FastAccess
    // Damages
    let state = applyDamages world state shipped damages orders
    // Resupplies
    let arrived = computeDelivered orders shipped blocked damages
    let state, leftOver = applyResupplies dt world state arrived
    let newSupplies = computeSuppliesProduced newSupplies |> Map.sumUnion leftOver
    // New production
    let state, leftOver = applyResupplies dt world state newSupplies
    // Captured planes
    let state, leftOver =
        if mustConvertCapturedPlanes then
            let afs, captured = convertCapturedPlanes wg state
            let state = { state with Airfields = afs }
            let energy = Map.sumUnion leftOver captured
            let state, leftOver = applyResupplies dt world state energy
            state, leftOver
        else
            state, leftOver
    // New attempt with the left-overs
    let state, waste = applyResupplies dt world state leftOver
    // Cargo deliveries
    let newCargo =
        computeCargoSupplies wg cargo
        |> Map.map (fun _ qty -> bombCost * qty * (1.0f - world.CargoReservedForBombs))
    let state, cargoLeft = applyResupplies dt world state newCargo
    // Bombs
    let bombCargo =
        computeCargoSupplies wg cargo
        |> Map.map (fun r qty ->
            let supplies = qty * bombCost
            let suppliesLeft =
                Map.tryFind r cargoLeft
                |> Option.defaultValue 0.0f<E>
            suppliesLeft + supplies * world.CargoReservedForBombs)
    let state = distributeCargo world state bombCargo
    state

/// Update airfield planes according to departures and arrivals
let applyPlaneTransfers (state : WorldState) (takeOffs : TookOff list) (landings : Landed list) =
    let airfields =
        state.Airfields
        |> Seq.map (fun af -> af.AirfieldId, af)
        |> Map.ofSeq
    let airfieldsAfterTakeOffs =
        takeOffs
        |> List.fold (fun airfields takeOff ->
            let af = Map.find takeOff.Airfield airfields
            let oldPlaneValue =
                Map.tryFind takeOff.Plane af.NumPlanes
                |> fun x -> defaultArg x 0.0f
            let newPlaneValue =
                oldPlaneValue - 1.0f |> max 0.0f
            let newPlanes =
                Map.add takeOff.Plane newPlaneValue af.NumPlanes
            Map.add takeOff.Airfield { af with NumPlanes = newPlanes; Supplies = af.Supplies - takeOff.Cargo * bombCost - takeOff.WeaponCost } airfields
        ) airfields
    let airfieldsAfterLandings =
        landings
        |> filterNoTakeOff takeOffs
        |> List.fold (fun airfields landing ->
            let af = Map.find landing.Airfield airfields
            let oldPlaneValue =
                Map.tryFind landing.Plane af.NumPlanes
                |> fun x -> defaultArg x 0.0f
                |> max 0.0f
            let newPlanes =
                Map.add landing.Plane (oldPlaneValue + landing.Health) af.NumPlanes
            Map.add landing.Airfield { af with NumPlanes = newPlanes } airfields
        ) airfieldsAfterTakeOffs
    let airfields =
        state.Airfields
        |> List.map (fun af -> airfieldsAfterLandings.[af.AirfieldId])
    { state with Airfields = airfields }

/// Similar to applyPlaneTransfers, but for AI ferry flights
let applyPlaneFerries (state : WorldState) ferryEvents =
    let airfields =
        state.Airfields
        |> Seq.map (fun af -> af.AirfieldId, af)
        |> Map.ofSeq
    // Deduce plane from start airfield if landed or killed.
    // That way incomplete flights to not lead to a plane loss.
    let airfieldsAfterTakeOffs =
        ferryEvents
        |> List.fold (fun airfields planeFerryEvent ->
            match planeFerryEvent with
            | PlaneFerryLanded order
            | PlaneFerryKilled order ->
                let af = Map.find order.Start airfields
                let oldPlaneValue =
                    Map.tryFind order.Plane af.NumPlanes
                    |> fun x -> defaultArg x 0.0f
                let newPlaneValue =
                    oldPlaneValue - 1.0f |> max 0.0f
                let newPlanes =
                    Map.add order.Plane newPlaneValue af.NumPlanes
                Map.add order.Start { af with NumPlanes = newPlanes } airfields
            | _ ->
                airfields
        ) airfields
    // Credit destination airfield after landing.
    let airfieldsAfterLandings =
        ferryEvents
        |> List.fold (fun airfields planeFerryEvent ->
            match planeFerryEvent with
            | PlaneFerryLanded order ->
                let af = Map.find order.Destination airfields
                let oldPlaneValue =
                    Map.tryFind order.Plane af.NumPlanes
                    |> fun x -> defaultArg x 0.0f
                let newPlaneValue =
                    oldPlaneValue + 1.0f |> max 0.0f
                let newPlanes =
                    Map.add order.Plane newPlaneValue af.NumPlanes
                Map.add order.Destination { af with NumPlanes = newPlanes } airfields
            | _ ->
                airfields
        ) airfieldsAfterTakeOffs
    let airfields =
        state.Airfields
        |> List.map (fun af -> airfieldsAfterLandings.[af.AirfieldId])
    { state with Airfields = airfields }

/// Remove vehicles killed during a battle preamble from a vehicle count map.
let decimateColumn (config : Configuration.Configuration) random (numVehicles : Map<GroundAttackVehicle, int>) (killed : BattleParticipantKilled list) =
    // Make it so that every concrete vehicle kill accounts for a fraction of a kill when applied.
    // This makes it harder for players to reach the kill limit.
    // It's a bit of a hack, but the way we do that is to multiply the number of vehicles before processing, then divide them
    let numVehicles =
        numVehicles
        |> Map.map (fun k num -> num * config.BattleKillRatio)
    // Remove no more than 50% of the original total value
    let totalValue =
        numVehicles
        |> Map.fold (fun value vehicle num ->
            value + (float32 num) * vehicle.Cost
        ) 0.0f<E>
    // First remove the exact vehicles, if they can be found,
    // keeping track of the value of vehicles killed that could not be removed.
    let rec damageExact numVehicles unmatchedAiDamage unmatchedPlayerDamage aiDamageSoFar playerDamageSoFar (killed : BattleParticipantKilled list) =
        match killed with
        | [] -> numVehicles, unmatchedAiDamage + unmatchedPlayerDamage, aiDamageSoFar + playerDamageSoFar
        | veh :: rest ->
            if veh.KilledByPlayer.IsNone then
                if unmatchedAiDamage + aiDamageSoFar >= config.MaxBattleKillsRatioByAI * totalValue then
                    // Damage by AI reached the limit, skip
                    damageExact numVehicles unmatchedAiDamage unmatchedPlayerDamage aiDamageSoFar playerDamageSoFar rest
                else
                    match numVehicles |> Map.tryFind veh.Vehicle with
                    | Some n when n > 0 ->
                        let numVehicles = Map.add veh.Vehicle (n - 1) numVehicles
                        damageExact numVehicles unmatchedAiDamage unmatchedPlayerDamage (aiDamageSoFar + veh.Vehicle.Cost) playerDamageSoFar rest
                    | _ ->
                        damageExact numVehicles (unmatchedAiDamage + veh.Vehicle.Cost) unmatchedPlayerDamage aiDamageSoFar playerDamageSoFar rest
            else
                if unmatchedPlayerDamage + playerDamageSoFar >= config.MaxBattleKillsRatioByPlayers * totalValue then
                    // Damage by players reached the limit, skip
                    damageExact numVehicles unmatchedAiDamage unmatchedPlayerDamage aiDamageSoFar playerDamageSoFar rest
                else
                    match numVehicles |> Map.tryFind veh.Vehicle with
                    | Some n when n > 0 ->
                        let numVehicles = Map.add veh.Vehicle (n - 1) numVehicles
                        damageExact numVehicles unmatchedAiDamage unmatchedPlayerDamage aiDamageSoFar (playerDamageSoFar + veh.Vehicle.Cost) rest
                    | _ ->
                        damageExact numVehicles unmatchedAiDamage (unmatchedPlayerDamage + veh.Vehicle.Cost) aiDamageSoFar playerDamageSoFar rest

    // Remove other vehicles up to the value computed above
    let rec damageOther damageLeft (vehicles : GroundAttackVehicle list) =
        if damageLeft <= 0.0f<E> then
            vehicles
        else
            match vehicles with
            | [] -> []
            | veh :: rest ->
                damageOther (damageLeft - veh.Cost) rest
    // Result
    let numVehicles, unmatchedDamage, doneDamage = damageExact numVehicles 0.0f<E> 0.0f<E> 0.0f<E> 0.0f<E> killed
    let numVehicles =
        damageOther unmatchedDamage (numVehicles |> expandMap |> Array.shuffle random |> List.ofArray)
        |> compactSeq
        |> Map.map (fun k num -> num / config.BattleKillRatio)
    let doneDamage = doneDamage + unmatchedDamage
    doneDamage / totalValue, numVehicles

/// Simulate a battle between two enemy columns that arrived at the same region
/// This is also used for column movements, which are seen as degenerate battles with an empty attacker side.
type BattleParticipants = {
    Defenders : Map<GroundAttackVehicle, int>
    Attackers : Map<GroundAttackVehicle, int>
    DefenderCoalition : CoalitionId
    AttackerBonus : float32
    DefenderBonus : float32
    Region : RegionId
}
with
    member this.Decimate(config, random : System.Random, preambleKills : BattleParticipantKilled list) =
        let defendersDecimation =
            preambleKills
            |> List.filter (fun killed -> killed.BattleId = this.Region && killed.Coalition = this.DefenderCoalition)
        let attackersDecimation =
            preambleKills
            |> List.filter (fun killed -> killed.BattleId = this.Region && killed.Coalition = this.DefenderCoalition.Other)
        let damageToDefenders, defenders = decimateColumn config random this.Defenders defendersDecimation
        let damageToAttackers, attackers = decimateColumn config random this.Attackers attackersDecimation
        { this with
            Defenders = defenders
            Attackers = attackers
        }, damageToDefenders, damageToAttackers

    /// Return the winning side, number of remaining vehicles of each type, collateral damages to storage.
    member this.RunBattle(random : System.Random) =
        match this.Defenders, this.Attackers with
        | _, numAttackers when numAttackers.IsEmpty ->
            this, 0.0f
        | numDefenders, _ when numDefenders.IsEmpty ->
            this, 0.0f
        | _ ->
            // Lists of vehicle type and health. Health is simply the cost of the vehicle.
            let attackers =
                this.Attackers
                |> Util.expandMap
                |> Array.map (fun vehicle -> vehicle, this.AttackerBonus * float32 vehicle.Cost)
            let defenders =
                this.Defenders
                |> Util.expandMap
                |> Array.map (fun vehicle -> vehicle, this.DefenderBonus * float32 vehicle.Cost)
            // Each attacker select a target randomly, then all targets get health removed according to the number of units that aim at them.
            let fire attackers defenders =
                let numDefenders = Array.length defenders
                let targets =
                    attackers
                    |> Array.map (fun _ -> random.Next(0, numDefenders))
                    |> Util.compactSeq
                defenders
                |> Seq.mapi (fun i (v, health) ->
                    match Map.tryFind i targets with
                    | Some n -> (v, health - float32 n)
                    | None -> (v, health)
                )
                |> Seq.filter (fun (_, health) -> health > 0.0f)
                |> Array.ofSeq
            // Have attackers and defenders fire at eachother simultaneously, and keep doing so until one side is defeated.
            let rec round attackers defenders =
                let defenders2 = fire attackers defenders
                let attackers2 = fire defenders attackers
                match defenders2, attackers2 with
                | _, [||] -> (this.DefenderCoalition, defenders2)
                | [||], _ -> (this.DefenderCoalition.Other, attackers2)
                | _, _ -> round attackers2 defenders2
            let victor, survivors = round attackers defenders
            let victors, defeated =
                if victor = this.DefenderCoalition then
                    this.Defenders, this.Attackers
                else
                    this.Attackers, this.Defenders
            // Amount of damages to storage is same as total health losses on both sides
            let damages =
                (defeated
                 |> Map.toSeq
                 |> Seq.sumBy (fun (vehicle, qty) -> float32 qty * float32 vehicle.Cost)) +
                (victors
                 |> Map.toSeq
                 |> Seq.sumBy (fun (vehicle, qty) -> float32 qty * float32 vehicle.Cost)) -
                (survivors
                 |> Array.sumBy (fun (vehicle, health) -> health))
            let remainingVictors =
                survivors
                |> Seq.groupBy fst
                |> Seq.map (fun (vehicle, healths) -> vehicle, healths |> Seq.sumBy snd)
                |> Map.ofSeq
                |> Map.map (fun vehicle healthTotal -> (healthTotal / float32 vehicle.Cost) |> floor |> int)
            let participants =
                if victor = this.DefenderCoalition then
                    { this with Defenders = remainingVictors; Attackers = Map.empty }
                else
                    { this with Attackers = remainingVictors; Defenders = Map.empty }
            participants, damages

    member this.Victors =
        if Map.isSumNull this.Attackers then
            Some this.DefenderCoalition
        elif Map.isSumNull this.Defenders then
            Some this.DefenderCoalition.Other
        else
            None

    static member ComputeLosses(before : BattleParticipants, after : BattleParticipants) =
        assert(before.DefenderCoalition = after.DefenderCoalition)
        let diffAttackers = Util.subMaps before.Attackers after.Attackers
        let diffDefenders = Util.subMaps before.Defenders after.Defenders
        [(before.DefenderCoalition, diffDefenders); (before.DefenderCoalition.Other, diffAttackers)]
        |> Map.ofSeq

/// Active pattern to classify the status/result of a battle
let (|BattleEnded|BattleContinues|NoBattle|) (x : BattleParticipants) =
    match x.Victors with
    | None ->
        if Map.isSumNull x.Defenders then
            NoBattle
        else
            BattleContinues
    | Some victors when victors = x.DefenderCoalition ->
        BattleEnded(victors, x.Defenders)
    | Some victors ->
        BattleEnded(victors, x.Attackers)

/// Build a battle for each region with invaders
let buildBattles (state : WorldState) (paras : ParaDropResult list) =
    let sg = WorldStateFastAccess.Create state
    [
        for regState in state.Regions do
            match regState.Owner with
            | Some defendingSide when regState.HasInvaders ->
                let region = regState.RegionId
                let defenders, attackers =
                    regState.NumVehicles, regState.NumInvadingVehicles
                // Bonus from paratrooper drops
                let computeParaBonus coalition =
                    paras
                    |> List.filter (fun drop -> drop.Coalition = coalition && drop.BattleId = region)
                    |> List.sumBy (fun drop ->
                        match drop.Precision with
                        | Precise -> 0.01f // 1% force multiplier for precise drop (per soldier)
                        | Wide -> 0.005f)   // 0.5% force multiplier for wide drop
                    |> min 0.5f // Up to 50% multiplier overall
                yield
                    { Defenders = defenders
                      Attackers = attackers
                      DefenderCoalition = defendingSide
                      AttackerBonus = 1.0f + computeParaBonus defendingSide.Other
                      DefenderBonus = 1.0f + computeParaBonus defendingSide
                      Region = region
                    }
            | _ ->
                ()
    ]

/// Compute the tanks that have reached their destination (i.e. departed and were not damaged)
let computeCompletedColumnMovements (movements : ColumnMovement list) (departures : ColumnLeft list) (blocked : VehiclesBlocked list) (damaged : Damage list) =
    let blocked =
        blocked
        |> Seq.map (fun bl -> bl.OrderId, bl.RankOffset)
        |> Set.ofSeq
    let damageMap =
        damaged
        |> Seq.choose (fun damage ->
            match damage.Object with
            | Column vehicle ->
                Some(vehicle, damage.Data.Amount)
            | _ ->
                None)
        |> Seq.groupBy fst
        |> Seq.map (fun(veh, damages) -> veh, damages |> Seq.sumBy snd)
        |> Map.ofSeq
    let departed =
        departures
        |> Seq.groupBy(fun d -> d.OrderId)
        |> dict
    // Return updated movements with damaged vehicles removed
    [
        for move in movements do
            match departed.TryGetValue(move.OrderId) with
            | true, groups ->
                let comp =
                    [|
                        for group in groups do
                            let sendBack = blocked.Contains((move.OrderId, Some group.RankOffset))
                            for i, tank in group.Vehicles |> Seq.indexed do
                                let rank, damageThreshold =
                                    match move.TransportType with
                                    | ColByRoad -> group.RankOffset + i, 0.25f
                                    | ColByTrain -> 0, 1.0f
                                    | ColByRiverShip
                                    | ColBySeaShip ->
                                        let numShips = move.NumShips
                                        if numShips > 0 then
                                            i % numShips, 1.0f
                                        else
                                            0, 1.0f
                                let vehicle = { OrderId = move.OrderId; Rank = rank }
                                match Map.tryFind vehicle damageMap with
                                | Some amount when amount >= damageThreshold -> ()
                                | _ -> yield sendBack, tank
                    |]
                let sentBack, arrived = comp |> Array.partition fst
                if not(Array.isEmpty sentBack) then
                    // Return survivors to start region
                    yield { move with Composition = sentBack |> Array.map snd; Destination = move.Start }
                if not(Array.isEmpty arrived) then
                    yield { move with Composition = arrived |> Array.map snd }
            | false, _ ->
                // If it did not depart, then it could not arrive.
                ()
    ]


/// Used for after-action reports
type BattleSummary = {
    Region : RegionId
    Participants : BattleParticipants
    Victors : CoalitionId
    Survivors : Map<GroundAttackVehicle, int>
    Losses : Map<CoalitionId, Map<GroundAttackVehicle, int>>
    DamageToDefendersFromAir : float32
    DamageToAttackersFromAir : float32
    CollateralDamage : float32
}

/// Run each battle, update vehicles in targetted regions and flip them if attackers are victorious
let applyConquests config (world : World) (state : WorldState) (battles : BattleParticipants list) (preambleKills : BattleParticipantKilled list) =
    let random = System.Random()
    let sg = state.FastAccess
    let battleResults =
        battles
        |> List.map (fun battle ->
            let battle2, damageToDefenders, damageToAttackers = battle.Decimate(config, random, preambleKills)
            let afterBattle, collateral = battle2.RunBattle(random)
            let losses = BattleParticipants.ComputeLosses(battle, afterBattle)
            battle2.Region, (afterBattle, collateral, losses, damageToDefenders, damageToAttackers))
        |> Map.ofList
    let battleReports =
        battles
        |> List.filter (fun battle -> not battle.Attackers.IsEmpty)
        |> List.choose (fun battle ->
            match battleResults.TryFind battle.Region with
            | Some (BattleEnded(victors, survivors), damage, losses, damageToDefenders, damageToAttackers) ->
                Some {
                    Region = battle.Region
                    Participants = battle
                    Victors = victors
                    Survivors = survivors
                    Losses = losses
                    CollateralDamage = damage
                    DamageToDefendersFromAir = damageToDefenders
                    DamageToAttackersFromAir = damageToAttackers
                }
            | Some _
            | None ->
                None)
    let regions =
        [
            for region in state.Regions do
                match Map.tryFind region.RegionId battleResults with
                | Some(BattleEnded(newOwner, survivors), damages, _, _, _) ->
                    yield { region with Owner = Some newOwner; NumVehicles = survivors; NumExposedVehicles = survivors;  NumInvadingVehicles = Map.empty; Supplies = max 0.0f<E> (region.Supplies - 1.0f<E> * damages) }
                | Some _
                | None ->
                    yield region
        ]
    let airfields =
        [
            for af, afState in List.zip world.Airfields state.Airfields do
                match Map.tryFind af.Region battleResults with
                | Some(BattleEnded(newOwner, survivors), damages, _, _, _) ->
                    let factor =
                        match sg.GetRegion(af.Region).Owner with
                        | Some owner when owner <> newOwner -> 1.5f<E> // Previous owners sabotaged some of their planes.
                        | Some _ -> 1.0f<E> // Previous owners defended their planes, but some got damaged in the battle.
                        | None -> 0.0f<E> // No battle took place, region was neutral.
                    yield afState.ApplyDamage (factor * damages)
                | Some _
                | None ->
                    yield afState
        ]
    { state with Regions = regions; Airfields = airfields }, battleReports

/// Subtract vehicles that have departed from regions of departure. Must be called before buildBattles.
let applyVehicleDepartures (state : WorldState) (movements : ColumnMovement list) (departures : ColumnLeft list) =
    let movements =
        movements
        |> Seq.map (fun movement -> movement.OrderId, movement)
        |> Map.ofSeq
    // Registered departures from safe movements
    let departed =
        departures
        |> List.choose (fun departure -> Map.tryFind departure.OrderId movements |> Option.map (fun movement -> movement, departure))
        |> List.fold (fun regionMap (movement, departure) ->
            let numVehicles : Map<GroundAttackVehicle, int> =
                Map.tryFind movement.Start regionMap
                |> Option.defaultVal Map.empty
            let numVehicles = Util.addMaps numVehicles (Util.compactSeq departure.Vehicles)
            Map.add movement.Start numVehicles regionMap
        ) Map.empty
    // Remove departed vehicles from regions
    let regions =
        state.Regions
        |> List.map (fun region ->
            match Map.tryFind region.RegionId departed with
            | None -> region
            | Some removed ->
                let numVehicles =
                    removed
                    |> Map.fold (fun numVehicles vehicle removed ->
                        let newQty =
                            match Map.tryFind vehicle numVehicles with
                            | Some n -> n - removed
                            | None -> 0
                            |> max 0
                        Map.add vehicle newQty numVehicles
                    ) region.NumVehicles
                { region with NumVehicles = numVehicles }
        )
    { state with Regions = regions }

/// Reset the exposed vehicles in each region.
let resetExposedVehicles (state : WorldState) =
    { state with
        Regions = state.Regions |> List.map (fun regState -> { regState with NumExposedVehicles = Map.empty })
    }

/// Add vehicles that have arrived in a region to that region. Must be called after applyConquests.
let applyVehicleArrivals (state : WorldState) (movements : ColumnMovement list) =
    let arrived =
        movements
        |> Seq.groupBy (fun move -> move.Destination)
        |> Map.ofSeq
    let state = resetExposedVehicles state
    let regions =
        state.Regions
        |> List.map (fun region ->
            match Map.tryFind region.RegionId arrived with
            | None -> region
            | Some columns ->
                columns
                |> Seq.fold (fun region column ->
                    let comp =
                        column.Composition
                        |> Util.compactSeq
                    match region.Owner with
                    | None ->
                        { region with
                            Owner = Some column.OrderId.Coalition
                            NumVehicles = comp
                            NumExposedVehicles = comp
                        }
                    | Some coalition when column.OrderId.Coalition = coalition ->
                        { region with
                            NumVehicles = Util.addMaps region.NumVehicles comp
                            NumExposedVehicles = Util.addMaps region.NumExposedVehicles comp
                        }
                    | Some coalition ->
                        { region with
                            NumInvadingVehicles = Util.addMaps region.NumInvadingVehicles comp
                        }
                ) region
        )
    { state with Regions = regions }

let updateRunways (world : World) (state : WorldState) (windDirection : float32) =
    let afStates =
        [
            for af, afState in List.zip world.Airfields state.Airfields do
                yield afState.SetRunway(windDirection, af.Spawn)
        ]
    { state with Airfields = afStates
    }

type WorkingDay =
    { MorningStart : int
      EveningStop : int }

let shortWinterWorkingDay = { MorningStart = 9; EveningStop = 17 }
let longWinterWorkingDay = { MorningStart = 6; EveningStop = 17 }
let shortSummerWorkingDay = { MorningStart = 8; EveningStop = 18 }
let longSummerWorkingDay = { MorningStart = 6; EveningStop = 18 }

let nextDate (longDay : bool) (dt : float32<H>) (date : System.DateTime) =
    let workingDay =
        if date.Month >= 4 && date.Month <= 10 then
            if longDay then
                longSummerWorkingDay
            else
                shortSummerWorkingDay
        else
            if longDay then
                longWinterWorkingDay
            else
                shortWinterWorkingDay
    let h = floor(float32 dt)
    let mins = 60.0f * ((float32 dt) - h)
    let newDate =
        let x = date + System.TimeSpan(int h, int mins, 0)
        if x.Hour >= workingDay.EveningStop then
            let x2 = x.AddDays(1.0)
            System.DateTime(x2.Year, x2.Month, x2.Day, workingDay.MorningStart, 0, 0)
        else
            x
    newDate

type MissionResults = {
    Entries : string list
    Shipments : SuppliesShipped list
    StaticDamages : Damage list
    VehicleDamages : Damage list
    TakeOffs : TookOff list
    Landings : Landed list
    ColumnDepartures : ColumnLeft list
    ParaDrops : ParaDropResult list
    FerryPlanes : Choice<PlaneFerryOrder, PlaneFerryOrder, PlaneFerryOrder> list
    BattleKills : BattleParticipantKilled list
    Blocked : VehiclesBlocked list
}
with
    member this.Damages =
        this.StaticDamages @ this.VehicleDamages
        |> Damage.GroupByObject
        |> List.ofSeq

let newState (config : Configuration.Configuration) (world : World) (state : WorldState) (axisOrders : OrderPackage) (alliesOrders : OrderPackage) (results : MissionResults) windOri =
    let dt = 1.0f<H> * float32 config.MissionLength / 60.0f
    let mustConvertCapturedPlanes = config.MaxCapturedPlanes = 0
    let columnOrders = axisOrders.Columns @ alliesOrders.Columns
    let state2 =
        state
        |> applyProduction dt world Axis axisOrders.Production
        |> applyProduction dt world Allies alliesOrders.Production
    let state3, ((newSupplies, newAxisVehicles, newAlliesVehicles) as newlyProduced) = convertProduction world state2
    let state3b = applyConsumption dt world state3
    let state4 = applyDamagesAndResupplies mustConvertCapturedPlanes dt world state3b results.Shipments results.Blocked results.Damages (axisOrders.Resupply @ alliesOrders.Resupply) newSupplies results.Landings
    let state5 = applyPlaneTransfers state4 results.TakeOffs results.Landings
    let state5b = applyPlaneFerries state5 results.FerryPlanes
    let state6 = applyVehicleDepartures state5b columnOrders results.ColumnDepartures
    let battles = buildBattles state6 results.ParaDrops
    let state7, battleReports = applyConquests config world state6 battles results.BattleKills
    let state7b =
        computeCompletedColumnMovements columnOrders results.ColumnDepartures results.Blocked results.Damages
        |> applyVehicleArrivals state7
    let state8 = updateRunways world state7b windOri
    { state8 with Date = nextDate config.LongWorkDay dt state8.Date; AttackingSide = state8.AttackingSide.Other }, newlyProduced, battleReports