﻿/// Given a an old world state and mission result data, produce an updated world state
module Campaign.NewWorldState

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction
open Campaign.Util
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.PlaneModel

/// What to produce in each category of production, and how much does each category need
type ProductionPriorities = {
    Vehicle : GroundAttackVehicle
    PriorityVehicle : float32<E>
    Plane : PlaneType
    PriorityPlane : float32<E>
    PrioritySupplies : float32<E>
}

/// Decide what vehicles and planes to produce, and how important they are.
let computeProductionPriorities (coalition : CoalitionId) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state

    let supplyNeed =
        let allNeeds = AutoOrder.computeSupplyNeeds world state
        state.Regions
        |> Seq.sumBy (fun state ->
            if state.Owner = Some coalition then
                Map.tryFind state.RegionId allNeeds
                |> Option.defaultVal 0.0f<E>
            else
                0.0f<E>)
    assert(supplyNeed >= 0.0f<E>)

    let vehicleToProduce, vehicleNeed =
        // vehicle need is dictated by number of regions on the front line
        let numHeavy, numMedium, numLight, need =
            state.Regions
            |> Seq.filter (fun state -> state.Owner = Some coalition) // Regions we control
            |> Seq.filter (fun state -> // Regions at the front
                let region = wg.GetRegion state.RegionId
                region.Neighbours
                |> Seq.exists (fun ngh -> sg.GetRegion(ngh).Owner <> Some coalition))
            |> Seq.map (fun state ->
                let numHeavy = state.GetNumVehicles HeavyTank
                let numMedium = state.GetNumVehicles MediumTank
                let numLight = state.GetNumVehicles LightArmor
                let desiredValue = 3.0f * GroundAttackVehicle.HeavyTankCost + 9.0f * GroundAttackVehicle.MediumTankCost + 3.0f * GroundAttackVehicle.LightArmorCost
                let availableValue = float32 numHeavy * GroundAttackVehicle.HeavyTankCost + float32 numMedium * GroundAttackVehicle.MediumTankCost + float32 numLight * GroundAttackVehicle.LightArmorCost
                numHeavy, numMedium, numLight, max 0.0f<E> (desiredValue - availableValue))
            |> Seq.fold (fun (t1, t2, t3, t4) (n1, n2, n3, n4) -> (t1 + n1, t2 + n2, t3 + n3, t4 + n4)) (0, 0, 0, 0.0f<E>)
        let vehicle =
            if numMedium = 0 then
                MediumTank
            elif numHeavy < numLight && 3 * numHeavy < numMedium then
                HeavyTank
            elif numLight >= numHeavy && 3 * numLight < numMedium then
                LightArmor
            else
                MediumTank
        vehicle, need
    assert(vehicleNeed >= 0.0f<E>)

    let planeTypeToProduce, planeNeed =
        let planeTypeShares =
            match coalition with
            | Axis -> [ 0.4f; 0.3f; 0.2f; 0.1f ]
            | Allies -> [ 0.4f; 0.4f; 0.2f; 0.0f ]
            |> List.zip [ Fighter; Attacker; Bomber; Transport ]
            |> Map.ofList
        assert(planeTypeShares |> Seq.sumBy (fun kvp -> kvp.Value) = 1.0f)
        let numPlanesPerType =
            state.Airfields
            |> Seq.filter (fun afs -> sg.GetRegion(wg.GetAirfield(afs.AirfieldId).Region).Owner = Some coalition) // Only our coalition
            |> Seq.fold (fun perType afs ->
                afs.NumPlanes
                |> Map.toSeq
                |> Seq.map (fun (plane, qty) -> plane.PlaneType, qty) // Replace exact plane model by plane type
                |> Seq.groupBy fst
                |> Seq.map (fun (typ, xs) -> typ, xs |> Seq.sumBy snd) // Total number of planes per type at that airfield
                |> Seq.fold (fun perType (typ, qty) -> // Add to total number of planes per type for all regions
                    let newQty =
                        qty +
                        (Map.tryFind typ perType |> Option.defaultVal 0.0f)
                    Map.add typ newQty perType
                ) perType
            ) Map.empty
        let total =
            numPlanesPerType
            |> Seq.sumBy (fun kvp -> kvp.Value)
        let relNumPlanesPerType =
            numPlanesPerType
            |> Map.map (fun typ qty -> qty / total)
        let mostNeeded =
            planeTypeShares
            |> Map.filter (fun _ qty -> qty > 0.0f)
            |> Map.map (fun typ share ->
                let actual = Map.tryFind typ relNumPlanesPerType |> Option.defaultVal 0.0f
                actual / share)
            |> Map.toSeq
            |> Seq.minBy snd
            |> fst
        let valueTarget =
            0.8f * state.TotalPlaneValueOfCoalition(world, coalition.Other)
        // plane need is dictated by enemy plane forces. We don't want to fall too far behind.
        let need =
            valueTarget - state.TotalPlaneValueOfCoalition(world, coalition)
            |> max 200.0f<E>
        mostNeeded, need

    { Vehicle = vehicleToProduce
      PriorityVehicle = vehicleNeed
      Plane = planeTypeToProduce
      PriorityPlane = planeNeed
      PrioritySupplies = supplyNeed
    }


/// Add production units according to production priorities
let applyProduction (dt : float32<H>) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let work (state : WorldState) (coalition : CoalitionId) =
        let priorities = computeProductionPriorities coalition world state
        let vehiclePrio, planePrio, energyPrio =
            let total =
                priorities.PriorityPlane + priorities.PrioritySupplies + priorities.PriorityVehicle
            if total <= 0.0f<E> then
                0.0f, 0.0f, 0.0f
            else
                priorities.PriorityVehicle / total, priorities.PriorityPlane / total, priorities.PriorityVehicle / total
        let regions =
            [
                for region, regState in Seq.zip world.Regions state.Regions do
                    if regState.Owner = Some coalition && not(List.isEmpty region.Production) then
                        // Redistribute plane production resources if this region has no airfield to receive the newly produced planes.
                        let vehiclePrio, planePrio, energyPrio =
                            if world.Airfields |> List.exists (fun af -> af.Region = region.RegionId) then
                                vehiclePrio, planePrio, energyPrio
                            else
                                vehiclePrio + 0.5f * planePrio, 0.0f, energyPrio + 0.5f * planePrio
                        let energy = dt * regState.ProductionCapacity(region)
                        let supplies = regState.Products.Supplies + energyPrio * energy
                        let planes =
                            let planeModel =
                                regState.Products.Planes
                                |> Map.filter (fun plane _ -> plane.PlaneType = priorities.Plane)
                                |> Map.toSeq
                                |> Seq.sortBy snd
                                |> Seq.tryLast
                                |> Option.map fst
                                |> Option.defaultVal (priorities.Plane.Random(coalition) |> Option.get)
                            let oldValue =
                                Map.tryFind planeModel regState.Products.Planes
                                |> Option.defaultVal 0.0f<E>
                            assert(planePrio >= 0.0f)
                            assert(planePrio <= 1.0f)
                            let newValue = oldValue + planePrio * energy
                            Map.add planeModel newValue regState.Products.Planes
                        let vehicles =
                            let oldValue =
                                Map.tryFind priorities.Vehicle regState.Products.Vehicles
                                |> Option.defaultVal 0.0f<E>
                            let newValue = oldValue + vehiclePrio * energy
                            Map.add priorities.Vehicle newValue regState.Products.Vehicles
                        let assignment = { regState.Products with Supplies = supplies; Planes = planes; Vehicles = vehicles }
                        yield { regState with Products = assignment }
                    else
                        yield regState
            ]
        { state with Regions = regions }
    [ Axis; Allies ]
    |> List.fold work state

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
    resupplied.Value

/// Consume supplies to heal buildings
let computeHealing(healths, buildings, energy) =
    let energyPerBuilding =
        buildings
        |> List.map (fun (x : StaticGroup) -> x.RepairCost)
    let prodHealing, energy =
        List.zip healths energyPerBuilding
        |> List.fold (fun (healings, available) (health, healthCost : float32<E>) ->
            let spend =
                min ((1.0f - health) * healthCost) available
            (spend / healthCost) :: healings, available - spend
        ) ([], energy)
    let prodHealing = List.rev prodHealing
    let prodHealth =
        List.zip healths prodHealing
        |> List.map (fun (health, healing) -> health + healing |> max 0.0f |> min 1.0f)
    prodHealth, energy

/// Apply damages due to attacks, use supplies to repair damages and replenish storage
let applyRepairsAndDamages (dt : float32<H>) (world : World) (state : WorldState) (shipped : SuppliesShipped list) (supplies : Resupplied list) (damages : Damage list) =
    let wg = WorldFastAccess.Create world
    let damages =
        let data =
            damages
            |> Seq.groupBy (fun dmg -> dmg.Object)
            |> Seq.map (fun (victim, damages) -> victim, damages |> Seq.map (fun dmg -> dmg.Data))
        Map.ofSeq data
    let supplies =
        let data =
            supplies
            |> Seq.groupBy (fun sup -> sup.Region)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> sup.Energy))
        Map.ofSeq data
    let shipped =
        let data =
            shipped
            |> Seq.groupBy (fun sup -> sup.Sender)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> sup.Energy))
        Map.ofSeq data
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
                let afState = { afState with Supplies = factor * afState.Supplies }
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
                    |> Seq.filter (fun area -> area.Home.Home = region.RegionId)
                    |> Seq.sumBy (fun area ->
                        let damage =
                            Map.tryFind (Canon(area.DefenseAreaId)) damages
                            |> Option.defaultVal Seq.empty
                            |> Seq.sumBy (fun data -> data.Amount)
                        damage * canonCost)
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
                    match Map.tryFind region.RegionId supplies with
                    | Some e -> e
                    | None -> 0.0f<E>
                let prodHealth, energy =
                    computeHealing(
                        regState.ProductionHealth,
                        region.Production,
                        energy)
                let storeHealth, energy =
                    computeHealing(
                        regState.StorageHealth,
                        region.Storage,
                        energy)
                let storeCapacity = regState.StorageCapacity(region)
                // Consume energy to fill supplies
                // FIXME: we should only consume what's needed, i.e. no supplies for anti-tank defenses if there are no enemy regions nearby.
                let toSupplies =
                    storeCapacity - regState.Supplies
                    |> min energy
                    |> max 0.0f<E>
                let supplies = regState.Supplies + toSupplies
                // Part of supplies in storage that exceed the storage capacity is lost
                let waste = 0.2f * max 0.0f<E> (supplies - regState.StorageCapacity(region))
                let energy = energy - toSupplies
                yield { regState with ProductionHealth = prodHealth; StorageHealth = storeHealth; Supplies = supplies - waste }, energy
        ]
    let regionEnergies =
        regionsAfterSupplies
        |> Seq.map (fun (reg, energy) -> reg.RegionId, energy)
        |> Map.ofSeq
    // Use what's left for airfields
    let airfieldsAfterResupplies =
        let x, _ =
            airfieldsAfterDamages
            |> List.fold (fun (airfields, regionEnergies) afState ->
                let af = wg.GetAirfield(afState.AirfieldId)
                let energy =
                    Map.tryFind af.Region regionEnergies
                    |> fun x -> defaultArg x 0.0f<E>
                let storeHealth, energy =
                    computeHealing(afState.StorageHealth, af.Storage, energy)
                { afState with StorageHealth = storeHealth } :: airfields, Map.add af.Region energy regionEnergies
            ) ([], regionEnergies)
        List.rev x
    { state with
        Regions = regionsAfterSupplies |> List.map fst
        Airfields = airfieldsAfterResupplies }

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
            let newPlanes =
                Map.add takeOff.Plane (oldPlaneValue - 1.0f) af.NumPlanes
            Map.add takeOff.Airfield { af with NumPlanes = newPlanes } airfields
        ) airfields
    let airfieldsAfterLandings =
        landings
        |> List.fold (fun airfields landing ->
            let af = Map.find landing.Airfield airfields
            let oldPlaneValue =
                Map.tryFind landing.Plane af.NumPlanes
                |> fun x -> defaultArg x 0.0f
            let newPlanes =
                Map.add landing.Plane (oldPlaneValue + landing.Health) af.NumPlanes
            Map.add landing.Airfield { af with NumPlanes = newPlanes} airfields
        ) airfieldsAfterTakeOffs
    let airfields =
        state.Airfields
        |> List.map (fun af -> airfieldsAfterLandings.[af.AirfieldId])
    { state with Airfields = airfields }

/// Simulate a battle between two enemy columns that arrived at the same region
/// This is also used for column movements, which are seen as degenerate battles with an empty attacker side.
type BattleParticipants = {
    Defenders : ColumnArrived list
    Attackers : ColumnArrived list
}
with
    member this.RunBattle(random : System.Random) =
        match this.Defenders, this.Attackers with
        | [], _ -> this.Attackers, 0.0f
        | _, [] -> this.Defenders, 0.0f
        | _ ->
            // Lists of vehicle type and health. Health is simply the cost of the vehicle.
            let attackers =
                this.Attackers
                |> List.map (fun arrived -> arrived.Vehicle, float32 arrived.Vehicle.Cost)
            let defenders =
                this.Defenders
                |> List.map (fun arrived -> arrived.Vehicle, float32 arrived.Vehicle.Cost)
            // Each attacker select a target randomly, then all targets get health removed according to the number of units that aim at them.
            let fire attackers defenders =
                let numDefenders = List.length defenders
                let targets =
                    attackers
                    |> List.map (fun _ -> random.Next(0, numDefenders - 1))
                    |> Util.compactSeq
                defenders
                |> Seq.mapi (fun i (v, health) ->
                    match Map.tryFind i targets with
                    | Some n -> (v, health - float32 n)
                    | None -> (v, health)
                )
                |> Seq.filter (fun (_, health) -> health > 0.0f)
                |> List.ofSeq
            // Have attackers and defenders fire at eachother simultaneously, and keep doing so until one side is defeated.
            let rec round attackers defenders =
                let defenders2 = fire attackers defenders
                let attackers2 = fire defenders attackers
                match defenders2, attackers2 with
                | [], _ :: _ -> (this.Attackers, attackers2, this.Defenders)
                | _ :: _, [] -> (this.Defenders, defenders2, this.Attackers)
                | _, _ -> round attackers2 defenders2
            let victors, survivors, defeated = round attackers defenders
            let damages =
                (defeated
                 |> List.sumBy (fun vehicle -> float32 vehicle.Vehicle.Cost)) +
                (victors
                 |> List.sumBy (fun vehicle -> float32 vehicle.Vehicle.Cost)) -
                (survivors
                 |> List.sumBy (fun (vehicle, health) -> health))
            let counts =
                survivors
                |> Seq.groupBy fst
                |> Seq.map (fun (vehicle, healths) -> vehicle, healths |> Seq.sumBy snd)
                |> Map.ofSeq
            let remainingVictors =
                victors
                |> List.fold (fun (filtered, counts) arrived ->
                    match Map.tryFind arrived.Vehicle counts with
                    | None -> (filtered, counts)
                    | Some h when h < 1.0f -> (filtered, Map.remove arrived.Vehicle counts)
                    | Some h -> (arrived :: filtered, Map.add arrived.Vehicle (h - 1.0f) counts)
                ) ([], counts)
                |> fst
            remainingVictors, damages

/// Group arrivals by destination and build the battle participants.
/// Note: we generate battles without attackers when a column moves into an uncontested region, including a region already under the column owner's control.
let buildBattles (state : WorldState) (movements : ColumnMovement list) (arrivals : ColumnArrived list) =
    let sg = WorldStateFastAccess.Create state
    let movements =
        movements
        |> Seq.map (fun movement -> movement.OrderId, movement)
        |> Map.ofSeq
    let byDestination =
        arrivals
        |> Seq.groupBy (fun arrival -> movements.[arrival.OrderId].Destination)
    byDestination
    |> Seq.map (fun (region, columns) ->
        let regState = sg.GetRegion(region)
        let defenders, attackers =
            columns
            |> List.ofSeq
            |> List.partition (fun arrived -> regState.Owner = Some (movements.[arrived.OrderId].OrderId.Coalition))
        let homeDefense =
            match regState.Owner with
            | Some coalition ->
                regState.NumVehicles
                |> Util.expandMap
                |> Seq.map (fun vehicle -> { OrderId = { Index = -1; Coalition = coalition }; Vehicle = vehicle })
                |> List.ofSeq
            | None ->
                []
        region,
        { Defenders = defenders @ homeDefense
          Attackers = attackers
        }
    )
    |> List.ofSeq
    
/// Run each battle, update vehicles in targetted regions and flip them if attackers are victorious
let applyConquests (state : WorldState) (battles : (RegionId * BattleParticipants) list) =
    let battles = battles |> Map.ofList
    let random = System.Random()
    let regions =
        [
            for region in state.Regions do
                match Map.tryFind region.RegionId battles with
                | Some battle ->
                    let survivors, damages = battle.RunBattle(random)
                    match survivors with
                    | [] ->
                        yield { region with NumVehicles = Map.empty }
                    | survivor :: _ ->
                        let numVehicles =
                            survivors
                            |> Seq.map (fun arrived -> arrived.Vehicle)
                            |> Util.compactSeq
                        let newOwner =
                            survivor.OrderId.Coalition
                        yield { region with Owner = Some newOwner; NumVehicles = numVehicles; Supplies = max 0.0f<E> (region.Supplies - 1.0f<E> * damages) }
                | None ->
                    yield region
        ]
    { state with Regions = regions }

/// Subtract vehicles that have arrived or have been destroyed from regions of departure. Must be called before applyConquests.
// Note: An alternative would to subtract vehicles that have departed, but we want to let slow tanks that fail to reach their destination before the mission ends be returned to their starting region.
let applyVehicleDepartures (state : WorldState) (movements : ColumnMovement list) (arrivals : ColumnArrived list) (damagedTanks : VehicleInColumn list) =
    let movements =
        movements
        |> Seq.map (fun movement -> movement.OrderId, movement)
        |> Map.ofSeq
    let departed =
        arrivals // Every arrival is preceded by a departure
        |> List.choose (fun departure -> Map.tryFind departure.OrderId movements |> Option.map (fun movement -> movement, departure))
        |> List.fold (fun regionMap (movement, departure) ->
            let numVehicles : Map<GroundAttackVehicle, int> =
                Map.tryFind movement.Start regionMap
                |> Option.defaultVal Map.empty
            let qty =
                Map.tryFind departure.Vehicle numVehicles
                |> Option.defaultVal 0
            let numVehicles = Map.add departure.Vehicle (qty + 1) numVehicles
            Map.add movement.Start numVehicles regionMap
        ) Map.empty
    let departed =
        damagedTanks // Every tank damage was preceded by a departure
        |> List.choose (fun damaged -> Map.tryFind damaged.OrderId movements |> Option.map (fun movement -> movement, damaged))
        |> List.fold (fun regionMap (movement, damaged) ->
            let numVehicles : Map<GroundAttackVehicle, int> =
                Map.tryFind movement.Start regionMap
                |> Option.defaultVal Map.empty
            let vehicle =
                try
                    movement.Composition.[damaged.Rank]
                with
                | _ ->
                    printfn "Could not find damaged vehicle %d in departed column" damaged.Rank
                    GroundAttackVehicle.LightArmor
            let qty =
                Map.tryFind vehicle numVehicles
                |> Option.defaultVal 0
            let numVehicles = Map.add vehicle (qty + 1) numVehicles
            Map.add movement.Start numVehicles regionMap
        ) departed
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


let eveningStop = 20
let morningStart = 5

let newState (dt : float32<H>) (world : World) (state : WorldState) movements convoyDepartures supplies damages tookOff landed columnDepartures columnArrivals =
    let state2 = applyProduction dt world state
    let state3, extra = convertProduction world state2
    let state4 = applyRepairsAndDamages dt world state3 convoyDepartures (supplies @ extra) damages
    let state5 = applyPlaneTransfers state4 tookOff landed
    let battles = buildBattles state5 movements columnArrivals
    let damagedTanks =
        damages
        |> List.choose (fun damage ->
            if damage.Data.Amount >= 0.25f then
                match damage.Object with
                | Column(col) -> Some col
                | _ -> None
            else None)
    let state6 = applyVehicleDepartures state5 movements columnArrivals damagedTanks
    let state7 = applyConquests state6 battles
    let state8 = updateNumCanons world state7
    let h = floor(float32 dt)
    let mins = 60.0f * ((float32 dt) - h)
    let newDate =
        let x = state8.Date + System.TimeSpan(int h, int mins, 0)
        if x.Hour >= eveningStop then
            let x2 = x.AddDays(1.0)
            System.DateTime(x2.Year, x2.Month, x2.Day, morningStart, 0, 0)
        else
            x
    { state8 with Date = newDate }