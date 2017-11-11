﻿/// Given a an old world state and mission result data, produce an updated world state
module Campaign.NewWorldState

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction
open Campaign.Util
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.PlaneModel
open System.Numerics
open VectorExtension
open SturmovikMission.Blocks.BlocksMissionData

/// Try to find the airfield that is furthest away from any enemy region.
let tryFindRearAirfield (world : World) (coalition : CoalitionId) (state : WorldState) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let furthest =
        try
            List.zip world.Airfields state.Airfields
            |> Seq.filter (fun (af, afs) -> sg.GetRegion(af.Region).Owner = Some coalition)
            |> Seq.maxBy(fun (af, afs) ->
                let distance =
                    try
                        List.zip world.Regions state.Regions
                        |> Seq.filter(fun (region, regs) -> regs.Owner = Some (coalition.Other))
                        |> Seq.map (fun (region, _) -> (region.Position - af.Pos).LengthSquared())
                        |> Seq.min
                    with
                    | _ -> 0.0f
                distance)
            |> fst
            |> fun af -> af.AirfieldId
            |> Some
        with
        | _ -> None
    furthest

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
                    let energy = dt * regState.ProductionCapacity(region, productionFactor world)
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
            let rear = tryFindRearAirfield world coalition state
            for af, afs in List.zip world.Airfields state.Airfields do
                if Some af.AirfieldId = rear then
                    let plane = AutoOrder.pickPlaneToProduce coalition world state
                    let plane = PlaneModel.RandomPlaneOfType(world.PlaneSet, plane, coalition)
                    match plane with
                    | Some model ->
                        let produced = world.PlaneProduction * dt / model.Cost
                        let oldValue = afs.NumPlanes |> Map.tryFind model |> Option.defaultVal 0.0f
                        let newValue = oldValue + produced
                        yield { afs with
                                    NumPlanes = Map.add model newValue afs.NumPlanes
                        }
                    | None ->
                        yield afs
                else
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
        // Freshly produced vehicles
        let newVehicles, remainingVehicles =
            regState.Products.Vehicles
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
        // Supplies -> storage
        let supplySpaceAvailable = max 0.0f<E> (regState.StorageCapacity(region) - regState.Supplies)
        let transferedEnergy = min supplySpaceAvailable regState.Products.Supplies
        resupplied := { Region = region.RegionId; Energy = transferedEnergy } :: !resupplied
        // Remove produced things from ongoing production
        let suppliesLeft = regState.Products.Supplies - transferedEnergy
        let assignment = { regState.Products with Planes = remainingPlanes; Vehicles = remainingVehicles; Supplies = suppliesLeft }
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

/// Consume supplies to heal buildings
/// healths : List of health levels
/// buildings : List of building descriptions, must match healths
/// energy : input energy
/// healLimit : max amount of input energy that can be used for healing
let computeHealing(healths, buildings, energy, healLimit) =
    let energyPerBuilding =
        buildings
        |> List.map (fun (x : StaticGroup) -> x.RepairCost)
    let prodHealing, energy, healLimit =
        List.zip healths energyPerBuilding
        |> List.fold (fun (healings, available, healLimit) (health, healthCost : float32<E>) ->
            let spend =
                ((1.0f - health) * healthCost)
                |> min available
                |> min healLimit
            (spend / healthCost) :: healings, available - spend, healLimit - spend
        ) ([], energy, healLimit)
    assert(healLimit >= 0.0f<E>)
    assert(energy >= 0.0f<E>)
    let prodHealing = List.rev prodHealing
    let prodHealth =
        List.zip healths prodHealing
        |> List.map (fun (health, healing) -> health + healing |> max 0.0f |> min 1.0f)
    prodHealth, energy

/// Apply damages due to attacks, use supplies to repair damages and replenish storage
let applyRepairsAndDamages (dt : float32<H>) (world : World) (state : WorldState) (shipped : SuppliesShipped list) (damages : Damage list) (orders : ResupplyOrder list) (newSupplies : Resupplied list) =
    let wg = WorldFastAccess.Create world
    let regionNeeds = AutoOrder.computeSupplyNeeds world state
    let healLimit = 200.0f<E/H> * dt
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
            |> Seq.groupBy (fun sup -> orders.[sup.OrderId].Convoy.Destination)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> orders.[sup.OrderId].Convoy.TransportedSupplies))
        Map.ofSeq data
    let shipped =
        let data =
            shipped
            |> Seq.groupBy (fun sup -> orders.[sup.OrderId].Convoy.Start)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> orders.[sup.OrderId].Convoy.TransportedSupplies))
        Map.ofSeq data
    let newSupplies =
        newSupplies
        |> List.map (fun sup -> sup.Region, sup.Energy)
        |> Map.ofSeq
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
                        | ByRoad -> ResupplyOrder.TruckCapacity
                        | ByAir _ -> 0.0f<E> // Air cargo not handled here. It's handled using the take-off/landing mechanism, same as for human players.
                    let newValue = oldValue - toBeRemoved |> max 0.0f<E>
                    Map.add order.Convoy.Destination newValue arrived
                | None ->
                    arrived
            | _ -> arrived
        ) arrived
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
    let applyDamage health =
        Option.map (fun damages ->
            damages
            |> Seq.sumBy (fun data -> data.Amount)
            |> (-) health
            |> max 0.0f)
        >> Option.defaultVal health
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
                let capacityBeforeDamages = afState.StorageCapacity(af)
                let storeHealth =
                    afState.StorageHealth
                    |> List.mapi (fun idx health ->
                        Map.tryFind (Airfield(afState.AirfieldId, idx)) damages
                        |> applyDamage health)
                let afState = { afState with
                                    NumPlanes = planes
                                    StorageHealth = storeHealth }
                let capacityAfterDamages = afState.StorageCapacity(af)
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
                    regState.ProductionHealth
                    |> List.mapi (fun idx health ->
                        Map.tryFind (Production(region.RegionId, idx)) damages
                        |> applyDamage health)
                let capacityBeforeDamages = regState.StorageCapacity(region)
                let storeHealth =
                    regState.StorageHealth
                    |> List.mapi (fun idx health ->
                        Map.tryFind (Storage(region.RegionId, idx)) damages
                        |> applyDamage health)
                let regState = { regState with ProductionHealth = prodHealth; StorageHealth = storeHealth }
                let capacityAfterDamages = regState.StorageCapacity(region)
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
    // Repair and resupply regions
    let regionsAfterSupplies =
        [
            for regState in regionsAfterDamagesToDefenses do
                let region = wg.GetRegion regState.RegionId
                let energy =
                    let fromShipments =
                        arrived.TryFind region.RegionId
                        |> Option.defaultVal 0.0f<E>
                    let fromProduction =
                        newSupplies.TryFind region.RegionId
                        |> Option.defaultVal 0.0f<E>
                    fromShipments + fromProduction
                // Highest prio: repair production
                let prodHealth, energy =
                    computeHealing(
                        regState.ProductionHealth,
                        region.Production,
                        energy,
                        healLimit)
                // Second prio: fill up region supplies
                let storeCapacity = regState.StorageCapacity(region)
                // Consume energy to fill up supplies up to what's needed
                let needs =
                    regionNeeds.TryFind regState.RegionId
                    |> Option.defaultVal 0.0f<E>
                    |> max 0.0f<E>
                let fillTarget = min needs storeCapacity
                let toSupplies =
                    fillTarget - regState.Supplies
                    |> min energy
                    |> max 0.0f<E>
                let supplies = regState.Supplies + toSupplies
                let energy = energy - toSupplies
                // Last: repair storage
                let storeHealth, energy =
                    computeHealing(
                        regState.StorageHealth,
                        region.Storage,
                        energy,
                        healLimit)
                yield { regState with ProductionHealth = prodHealth; StorageHealth = storeHealth; Supplies = supplies + energy }
        ]
    // Distribute supplies between regions and airfields
    let airfieldsDistribution, regSupplies =
        let x, regSupplies =
            airfieldsAfterDamages
            |> List.fold (fun (airfields, regionEnergies) afState ->
                let af = wg.GetAirfield(afState.AirfieldId)
                // take from region
                let energy =
                    Map.tryFind af.Region regionEnergies
                    |> fun x -> defaultArg x 0.0f<E>
                // repair airfield storage
                let storeHealth, energy =
                    computeHealing(afState.StorageHealth, af.Storage, energy, healLimit)
                let afState = { afState with StorageHealth = storeHealth }
                // fill storage
                let bombNeeds =
                    afState.BombNeeds * bombCost
                    |> max (1000.0f<K> * bombCost)
                let fillTarget = min bombNeeds (afState.StorageCapacity(af))
                let toAfSupplies =
                    fillTarget - afState.Supplies
                    |> max 0.0f<E>
                    |> min energy
                let energy = energy - toAfSupplies
                // what goes over the bomb storage fill target goes back to the region
                let backToRegion =
                    afState.Supplies + toAfSupplies - fillTarget
                    |> max 0.0f<E>
                // result
                let afState = { afState with Supplies = afState.Supplies + toAfSupplies - backToRegion }
                let energy = energy + backToRegion
                afState :: airfields, Map.add af.Region energy regionEnergies
            ) ([], regionsAfterSupplies |> List.map (fun regState -> regState.RegionId, regState.Supplies) |> Map.ofList)
        List.rev x, regSupplies
    let regionsAfterDistribution =
        [
            for regState, region in List.zip regionsAfterSupplies world.Regions do
                // Energy back from airfields; excess over capacity is lost
                let newEnergy =
                    regSupplies.TryFind regState.RegionId
                    |> Option.defaultVal 0.0f<E>
                    |> min (regState.StorageCapacity(region))
                yield { regState with Supplies = newEnergy }
        ]
    { state with
        Regions = regionsAfterDistribution
        Airfields = airfieldsDistribution }

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
            Map.add takeOff.Airfield { af with NumPlanes = newPlanes; Supplies = af.Supplies - takeOff.Cargo - takeOff.BombLoad * bombCost } airfields
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
            Map.add landing.Airfield { af with NumPlanes = newPlanes; Supplies = af.Supplies + landing.Cargo } airfields
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
let decimateColumn random (numVehicles : Map<GroundAttackVehicle, int>) (killed : BattleParticipantKilled list) =
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
            if not veh.KilledByPlayer then
                if unmatchedAiDamage + aiDamageSoFar >= 0.25f * totalValue then
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
                if unmatchedPlayerDamage + playerDamageSoFar >= 0.5f * totalValue then
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
    member this.Decimate(random : System.Random, preambleKills : BattleParticipantKilled list) =
        let defendersDecimation =
            preambleKills
            |> List.filter (fun killed -> killed.BattleId = this.Region && killed.Coalition = this.DefenderCoalition)
        let attackersDecimation =
            preambleKills
            |> List.filter (fun killed -> killed.BattleId = this.Region && killed.Coalition = this.DefenderCoalition.Other)
        let damageToDefenders, defenders = decimateColumn random this.Defenders defendersDecimation
        let damageToAttackers, attackers = decimateColumn random this.Attackers attackersDecimation
        { this with
            Defenders = defenders
            Attackers = attackers
        }, damageToDefenders, damageToAttackers

    /// Return the winning side, number of remaining vehicles of each type, collateral damages to storage.
    member this.RunBattle(random : System.Random) =
        match this.Defenders, this.Attackers with
        | _, numAttackers when numAttackers.IsEmpty ->
            this.DefenderCoalition, this.Defenders, 0.0f
        | numDefenders, _ when numDefenders.IsEmpty ->
            this.DefenderCoalition.Other, this.Attackers, 0.0f
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
            victor, remainingVictors, damages

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
let computeCompletedColumnMovements (movements : ColumnMovement list) (departures : ColumnLeft list) (damaged : Damage list) =
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
        |> Seq.map (fun d -> d.OrderId)
        |> Set.ofSeq
    // Return updated movements with damaged vehicles removed
    [
        for move in movements do
            if departed.Contains(move.OrderId) then
                let comp =
                    [|
                        for i, tank in move.Composition |> Seq.indexed do
                            let vehicle = { OrderId = move.OrderId; Rank = i }
                            match Map.tryFind vehicle damageMap with
                            | Some amount when amount > 0.25f -> ()
                            | _ -> yield tank
                    |]
                if not(Array.isEmpty comp) then
                    yield { move with Composition = comp }
    ]


/// Used for after-action reports
type BattleSummary = {
    Region : RegionId
    Participants : BattleParticipants
    Victors : CoalitionId
    Survivors : Map<GroundAttackVehicle, int>
    DamageToDefendersFromAir : float32
    DamageToAttackersFromAir : float32
    CollateralDamage : float32
}

/// Run each battle, update vehicles in targetted regions and flip them if attackers are victorious
let applyConquests (world : World) (state : WorldState) (battles : BattleParticipants list) (preambleKills : BattleParticipantKilled list) =
    let random = System.Random()
    let sg = state.FastAccess
    let battleResults =
        battles
        |> List.map (fun battle ->
            let battle, damageToDefenders, damageToAttackers = battle.Decimate(random, preambleKills)
            battle.Region, (battle.RunBattle(random), damageToDefenders, damageToAttackers))
        |> Map.ofList
    let battleReports =
        battles
        |> List.filter (fun battle -> not battle.Attackers.IsEmpty)
        |> List.choose (fun battle ->
            match battleResults.TryFind battle.Region with
            | Some ((victors, survivors, damage), damageToDefenders, damageToAttackers) ->
                Some {
                    Region = battle.Region
                    Participants = battle
                    Victors = victors
                    Survivors = survivors
                    CollateralDamage = damage
                    DamageToDefendersFromAir = damageToDefenders
                    DamageToAttackersFromAir = damageToAttackers
                }
            | None ->
                None)
    let regions =
        [
            for region in state.Regions do
                match Map.tryFind region.RegionId battleResults with
                | Some((newOwner, survivors, damages), _, _) ->
                    yield { region with Owner = Some newOwner; NumVehicles = survivors; Supplies = max 0.0f<E> (region.Supplies - 1.0f<E> * damages) }
                | None ->
                    yield region
        ]
    let airfields =
        [
            for af, afState in List.zip world.Airfields state.Airfields do
                match Map.tryFind af.Region battleResults with
                | Some((newOwner, survivors, damages), _, _) ->
                    let factor =
                        match sg.GetRegion(af.Region).Owner with
                        | Some owner when owner <> newOwner -> 1.5f<E> // Previous owners sabotaged some of their planes.
                        | Some _ -> 1.0f<E> // Previous owners defended their planes, but some got damaged in the battle.
                        | None -> 0.0f<E> // No battle took place, region was neutral.
                    yield afState.ApplyDamage (factor * damages)
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
            let numVehicles = Util.addMaps numVehicles departure.Vehicles
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

/// Add vehicles that have arrived in a region to that region. Must be called after applyConquests.
let applyVehicleArrivals (state : WorldState) (movements : ColumnMovement list) =
    let arrived =
        movements
        |> Seq.groupBy (fun move -> move.Destination)
        |> Map.ofSeq
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
                        }
                    | Some coalition when column.OrderId.Coalition = coalition ->
                        { region with
                            NumVehicles = Util.addMaps region.NumVehicles comp
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

let eveningStop = 20
let morningStart = 5

let nextDate (dt : float32<H>) (date : System.DateTime) =
    let h = floor(float32 dt)
    let mins = 60.0f * ((float32 dt) - h)
    let newDate =
        let x = date + System.TimeSpan(int h, int mins, 0)
        if x.Hour >= eveningStop then
            let x2 = x.AddDays(1.0)
            System.DateTime(x2.Year, x2.Month, x2.Day, morningStart, 0, 0)
        else
            x
    newDate

let newState (dt : float32<H>) (world : World) (state : WorldState) axisProduction alliesProduction (movements : ColumnMovement list) convoyDepartures supplies damages tookOff landed columnDepartures paradrops ferryPlanes battleKills windOri =
    let state2 =
        state
        |> applyProduction dt world Axis axisProduction
        |> applyProduction dt world Allies alliesProduction
    let state3, ((newSupplies, newAxisVehicles, newAlliesVehicles) as newlyProduced) = convertProduction world state2
    let state4 = applyRepairsAndDamages dt world state3 convoyDepartures damages supplies newSupplies
    let state5 = applyPlaneTransfers state4 tookOff landed
    let state5b = applyPlaneFerries state5 ferryPlanes
    let state6 = applyVehicleDepartures state5b movements columnDepartures
    let battles = buildBattles state6 paradrops
    let state7, battleReports = applyConquests world state6 battles battleKills
    let state7b =
        computeCompletedColumnMovements movements columnDepartures damages
        |> applyVehicleArrivals state7
    let state8 = updateRunways world state7b windOri
    { state8 with Date = nextDate dt state8.Date; AttackingSide = state8.AttackingSide.Other }, newlyProduced, battleReports