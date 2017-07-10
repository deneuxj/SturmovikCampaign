/// Given a an old world state and mission result data, produce an updated world state
module Campaign.NewWorldState

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction
open Campaign.Util
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.PlaneModel
open System.Numerics
open Vector
open SturmovikMission.Blocks.BlocksMissionData

/// Add production units according to production priorities
let applyProduction (dt : float32<H>) (world : World) (coalition : CoalitionId) (priorities : ProductionPriorities) (state : WorldState) =
    let wg = WorldFastAccess.Create world
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
                    let energy = dt * regState.ProductionCapacity(region, productionFactor world)
                    let supplies = regState.Products.Supplies + energyPrio * energy
                    let planes =
                        let planeModel =
                            regState.Products.Planes
                            |> Map.filter (fun plane _ -> plane.PlaneType = priorities.Plane)
                            |> Map.toSeq
                            |> Seq.sortBy snd
                            |> Seq.tryLast
                            |> Option.map fst
                            |> Option.defaultVal (priorities.Plane.Random(world.PlaneSet, coalition) |> Option.get)
                        let oldValue =
                            Map.tryFind planeModel regState.Products.Planes
                            |> Option.defaultVal 0.0f<E>
                        assert(planePrio >= 0.0f)
                        assert(planePrio <= 1.0f)
                        let newValue = oldValue + planePrio * energy
                        let toBeProduced = floor(newValue / planeModel.Cost)
                        let excess = newValue - toBeProduced * planeModel.Cost
                        regState.Products.Planes
                        |> Map.add planeModel (toBeProduced * planeModel.Cost) // add round value of new planes of the model currently being built
                        |> if toBeProduced >= 1.0f then
                            // If enough energy was accumulated to produce planes, pick a new model of the same type for the next of production.
                            let newPlaneModel =
                                priorities.Plane.Random(world.PlaneSet, coalition)
                            match newPlaneModel with
                            | Some model ->
                                Map.add model excess
                            | None ->
                                // Should not happen, but if we failed to pick a new model of plane, we keep the old one and let it have the entire amount of energy
                                Map.add planeModel newValue
                            else
                                // No excess energy, keep the mapping as it was
                                id
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
                    |> Seq.filter (fun area -> area.Home.Home = region.RegionId)
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
            let newPlanes =
                Map.add takeOff.Plane (oldPlaneValue - 1.0f) af.NumPlanes
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
            let newPlanes =
                Map.add landing.Plane (oldPlaneValue + landing.Health) af.NumPlanes
            Map.add landing.Airfield { af with NumPlanes = newPlanes; Supplies = af.Supplies + landing.Cargo } airfields
        ) airfieldsAfterTakeOffs
    let airfields =
        state.Airfields
        |> List.map (fun af -> airfieldsAfterLandings.[af.AirfieldId])
    { state with Airfields = airfields }

/// Simulate a battle between two enemy columns that arrived at the same region
/// This is also used for column movements, which are seen as degenerate battles with an empty attacker side.
type BattleParticipants = {
    Defenders : Map<GroundAttackVehicle, int>
    Attackers : Map<GroundAttackVehicle, int>
    DefenderCoalition : CoalitionId
    AttackerBonus : float32
}
with
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
                |> Array.map (fun vehicle -> vehicle, float32 vehicle.Cost)
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

/// Group arrivals by destination and build the battle participants.
/// Note: we generate battles without attackers when a column moves towards an uncontested friendly region.
let buildBattles (state : WorldState) (movements : ColumnMovement list) (departures : ColumnLeft list) (damaged : Damage list) (paras : ParaDropResult list) =
    let sg = WorldStateFastAccess.Create state
    let movements =
        movements
        |> Seq.map (fun movement -> movement.OrderId, movement)
        |> Map.ofSeq
    let damagedOnWayTo =
        damaged
        // Retain vehicles in columns that have been disabled (more than 25% damage)
        |> Seq.choose (fun damage ->
            match damage.Object with
            | Column vehicle when damage.Data.Amount >= 0.25f ->
                // Consider vehicle as fully damaged
                Some (vehicle, { damage.Data with Amount = 1.0f })
            | _ -> None)
        |> Seq.groupBy (fun (column, damage) -> movements.[column.OrderId].Destination)
        |> Seq.map (fun (destination, columnsAndDamages) ->
            destination,
            columnsAndDamages
            |> Seq.groupBy (fun (column, _) -> movements.[column.OrderId].Composition.[column.Rank], column.OrderId.Coalition)
            |> Seq.map (fun (vehicle, data) -> vehicle, data |> Seq.map snd |> Seq.sumBy (fun data -> data.Amount)))
        |> dict
    let byDestination =
        departures
        |> Seq.groupBy (fun departure -> movements.[departure.OrderId].Destination)
    let byStart =
        departures
        |> Seq.groupBy (fun departure -> movements.[departure.OrderId].Start)
    byDestination
    |> Seq.map (fun (region, columns) ->
        let regState = sg.GetRegion(region)
        let (defenders, attackers), defendingSide =
            match regState.Owner with
            | None ->
                // Attacking a neutral territory: defenders are always allies, attackers are axis
                columns
                |> List.ofSeq
                |> List.partition (fun arrived -> arrived.OrderId.Coalition = Allies),
                Allies
            | Some owner ->
                // Attacking a non-neutral territory: defenders are the owners, attackers are the others
                columns
                |> List.ofSeq
                |> List.partition (fun arrived -> arrived.OrderId.Coalition = owner),
                owner
        // Remove damaged vehicles from attacker's columns
        let attackers =
            let damaged =
                match damagedOnWayTo.TryGetValue region with
                | false, _ -> Map.empty
                | true, damages ->
                    damages
                    |> Seq.choose (
                        function
                        | ((vehicle, coalition), damages) when Some coalition.Other = regState.Owner -> Some(vehicle, damages |> int)
                        | _ -> None)
                    |> Map.ofSeq
            let mapped =
                attackers
                |> Seq.map (fun column -> column.Vehicles)
                |> Seq.fold Util.addMaps Map.empty
            Util.subMaps mapped damaged
            |> Map.filter (fun _ qty -> qty > 0)
        // Remove damaged vehicles from defender's reinforcements
        let defenders =
            let damaged =
                match damagedOnWayTo.TryGetValue region with
                | false, _ -> Map.empty
                | true, damages ->
                    damages
                    |> Seq.choose (
                        function
                        | ((vehicle, coalition), damages) when Some coalition = regState.Owner -> Some(vehicle, damages |> int)
                        | _ -> None)
                    |> Map.ofSeq
            let mapped =
                defenders
                |> Seq.map (fun column -> column.Vehicles)
                |> Seq.fold Util.addMaps Map.empty
            Util.subMaps mapped damaged
            |> Map.filter (fun _ qty -> qty > 0)
        // Add defense from vehicles parked at the region
        let defenders =
            match regState.Owner with
            | Some coalition ->
                // Remove departed vehicles from the vehicles parked at the region
                let departed =
                    byStart
                    |> Seq.tryFind (fun (start, _) -> region = start)
                    |> Option.map (fun (_, departed) ->
                        departed
                        |> Seq.map (fun order -> movements.[order.OrderId])
                        |> Seq.fold (fun m order -> order.Composition |> Util.compactSeq |> Util.addMaps m) Map.empty)
                    |> Option.defaultVal Map.empty
                Util.subMaps regState.NumVehicles departed
                |> Util.addMaps defenders
                |> Map.filter (fun _ qty -> qty > 0)
            | None ->
                Map.empty
        // Bonus to attackers from paratrooper drops
        let bonus =
            paras
            |> List.filter (fun drop -> drop.Coalition = defendingSide.Other && drop.LandZone = region)
            |> List.sumBy (fun drop ->
                match drop.Precision with
                | Precise -> 0.01f // 1% force multiplier for precise drop (per soldier)
                | Wide -> 0.005f)   // 0.5% force multiplier for wide drop
            |> min 1.0f // Up to 100% multiplier overall
        region,
        { Defenders = defenders
          Attackers = attackers
          DefenderCoalition = defendingSide
          AttackerBonus = 1.0f + bonus
        }
    )
    |> List.ofSeq

/// Used for after-action reports
type BattleSummary = {
    Region : RegionId
    Participants : BattleParticipants
    Victors : CoalitionId
    Survivors : Map<GroundAttackVehicle, int>
    CollateralDamage : float32
}

/// Run each battle, update vehicles in targetted regions and flip them if attackers are victorious
let applyConquests (world : World) (state : WorldState) (battles : (RegionId * BattleParticipants) list) =
    let random = System.Random()
    let sg = WorldStateFastAccess.Create state
    let battleResults =
        battles
        |> List.map (fun (region, battle) -> region, battle.RunBattle(random))
        |> Map.ofList
    let battleReports =
        battles
        |> List.filter (fun (_, battle) -> not battle.Attackers.IsEmpty)
        |> List.choose (fun (region, battle) ->
            match battleResults.TryFind region with
            | Some (victors, survivors, damage) ->
                Some {
                    Region = region
                    Participants = battle
                    Victors = victors
                    Survivors = survivors
                    CollateralDamage = damage
                }
            | None ->
                None)
    let regions =
        [
            for region in state.Regions do
                match Map.tryFind region.RegionId battleResults with
                | Some(newOwner, survivors, damages) ->
                    yield { region with Owner = Some newOwner; NumVehicles = survivors; Supplies = max 0.0f<E> (region.Supplies - 1.0f<E> * damages) }
                | None ->
                    yield region
        ]
    let airfields =
        [
            for af, afState in List.zip world.Airfields state.Airfields do
                match Map.tryFind af.Region battleResults with
                | Some(newOwner, survivors, damages) ->
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

/// Subtract vehicles that have departed from regions of departure. Must be called before applyConquests.
let applyVehicleDepartures (state : WorldState) (movements : ColumnMovement list) (departures : ColumnLeft list) =
    let movements =
        movements
        |> Seq.map (fun movement -> movement.OrderId, movement)
        |> Map.ofSeq
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

let updateRunways (world : World) (state : WorldState) (windDirection : float32) =
    let afStates =
        [
            for af, afState in List.zip world.Airfields state.Airfields do
                yield afState.SetRunway(windDirection, af.Spawn |> List.choose runwayOfAirfieldSpawn)
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

let newState (dt : float32<H>) (world : World) (state : WorldState) axisProduction alliesProduction movements convoyDepartures supplies damages tookOff landed columnDepartures paradrops windOri =
    let state2 =
        state
        |> applyProduction dt world Axis axisProduction
        |> applyProduction dt world Allies alliesProduction
    let state3, ((newSupplies, newAxisVehicles, newAlliesVehicles) as newlyProduced) = convertProduction world state2
    let state4 = applyRepairsAndDamages dt world state3 convoyDepartures damages supplies newSupplies
    let state5 = applyPlaneTransfers state4 tookOff landed
    let battles = buildBattles state5 movements columnDepartures damages paradrops
    let state6 = applyVehicleDepartures state5 movements columnDepartures
    let state7, battleReports = applyConquests world state6 battles
    let state8 = updateNumCanons world state7
    let state9 = updateRunways world state8 windOri
    { state9 with Date = nextDate dt state9.Date }, newlyProduced, battleReports