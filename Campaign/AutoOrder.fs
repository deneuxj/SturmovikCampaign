module Campaign.AutoOrder

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Vector
open Util

/// Compute the full-health storage capacity of each region, including airfields'
let computeStorageCapacity (world : World) =
    let afCapacity =
        seq {
            for af in world.Airfields do
                let capacity =
                    af.Storage
                    |> Seq.sumBy (fun building -> getSupplyCapacityPerBuilding building.Model)
                yield af.Region, capacity
        }
    let regCapacity =
        seq {
            for reg in world.Regions do
                let capacity =
                    reg.Storage
                    |> Seq.sumBy (fun building -> getSupplyCapacityPerBuilding building.Model)
                yield reg.RegionId, capacity
        }
    Seq.concat [ afCapacity; regCapacity ]
    |> Seq.groupBy fst
    |> Seq.map (fun (region, caps) -> region, caps |> Seq.sumBy snd)
    |> Map.ofSeq

/// Compute current storage of each region, including airfields'
let computeStorage (world : World) (state : WorldState) =
    let afStorage =
        seq {
            for af, afState in Seq.zip world.Airfields state.Airfields do
                yield af.Region, afState.Supplies
        }
    let regStorage =
        seq {
            for regState in state.Regions do
                yield regState.RegionId, regState.Supplies
        }
    Seq.concat [ afStorage; regStorage ]
    |> Seq.groupBy fst
    |> Seq.map (fun (region, caps) -> region, caps |> Seq.sumBy snd)
    |> Map.ofSeq

let computeSupplyNeeds (world : World) (state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let wg = WorldFastAccess.Create world
    // Bombs and repairs at airfields
    let afNeeds =
        seq {
            for af, afs in Seq.zip world.Airfields state.Airfields do
                let bombNeed =
                    afs.NumPlanes
                    |> Map.toSeq
                    |> Seq.sumBy(fun (plane, qty) -> plane.BombCapacity * qty * bombCost)
                let capacity =
                    af.Storage
                    |> Seq.sumBy (fun building -> getSupplyCapacityPerBuilding building.Model)
                let supplyDiff = min bombNeed capacity
                let repairs =
                    Seq.zip af.Storage afs.StorageHealth
                    |> Seq.sumBy (fun (building, health) -> (1.0f - health) * getEnergyHealthPerBuilding building.Model)
                yield af.Region, supplyDiff + repairs
        }
    let frontLine = computeFrontLine world state.Regions
    // Amounts of anti-tank and anti-air canons needed to have the region fully defended
    let regionCanonNeeds =
        seq {
            for antiTank, antiTankState in Seq.zip world.AntiTankDefenses state.AntiTankDefenses do
                match antiTank.Home with
                | FrontLine(home, ngh) when frontLine.Contains(home, ngh)->
                    yield home, float32(max 0 (getAntiTankCanonsForArea antiTank - antiTankState.NumUnits)) * canonCost
                | _ ->
                    ()
            for antiAir, antiAirState in Seq.zip world.AntiAirDefenses state.AntiAirDefenses do
                match antiAir.Home with
                | Central(home) ->
                    yield home, float32(max 0 (getAntiAirCanonsForArea antiAir - antiAirState.NumUnits)) * canonCost
                | _ ->
                    ()
        }
        |> Seq.groupBy fst
        |> Seq.map (fun (region, costs) -> region, costs |> Seq.sumBy snd)
    // Costs for canons adjusted by storage capacity
    let regionSaturatedCanonNeeds =
        let capacities = computeStorageCapacity world
        seq {
            for region, costs in regionCanonNeeds do
                let capacity =
                    Map.tryFind region capacities
                    |> Option.defaultVal 0.0f<E>
                let regState = sg.GetRegion(region)
                let reg = wg.GetRegion(region)
                let cost =
                    min (capacity - regState.Supplies) costs
                let repairs =
                    List.zip reg.Production regState.ProductionHealth
                    |> List.sumBy (fun (building, health) -> (1.0f - health) *  getEnergyHealthPerBuilding building.Model)
                yield region, cost + repairs
        }
    Seq.concat [ afNeeds ; regionSaturatedCanonNeeds ]
    |> Seq.groupBy fst
    |> Seq.map (fun (region, costs) -> region, costs |> Seq.sumBy snd)
    |> Map.ofSeq

/// Create convoy orders from regions owned by a coalition to neighbour regions that are further away from factories.
let createConvoyOrders (maxConvoySize : int, vehicleCapacity : float32<E>) (getPaths : World -> Path list) (coalition : CoalitionId) (world : World, state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let getOwner = sg.GetRegion >> (fun x -> x.Owner)
    let distances = computeRegionDistances true getPaths getOwner (coalition, world)
    let areConnectedByRoad(start, destination) =
        getPaths world
        |> List.exists (fun path -> path.MatchesEndpoints(start, destination).IsSome)
    let capacities = computeStorageCapacity world
    let storages = computeStorage world state
    let needs = computeSupplyNeeds world state
    let tryFind x y = Map.tryFind x y |> Option.defaultVal 0.0f<E>
    [
        for region, regState in List.zip world.Regions state.Regions do
            match Map.tryFind region.RegionId distances with
            | Some level ->
                for ngh in region.Neighbours do
                    match Map.tryFind ngh distances with
                    | Some other when other > level && areConnectedByRoad(region.RegionId, ngh) ->
                        let transfer =
                            let availableToSend = regState.Supplies
                            let senderNeeds = tryFind region.RegionId needs
                            let selfishNess = 0.5f
                            let willingToSend =
                                availableToSend - selfishNess * senderNeeds
                                |> max 0.0f<E>
                            let alreadyAtReceiver = tryFind ngh storages
                            let availableToReceive = tryFind ngh capacities
                            let receiverNeeds = tryFind ngh needs
                            let requested =
                                (min receiverNeeds availableToReceive) - alreadyAtReceiver
                                |> max 0.0f<E>
                            min willingToSend requested
                        if transfer > 0.0f<E> then
                            let size =
                                ceil(transfer / vehicleCapacity)
                                |> int
                                |> min maxConvoySize
                            yield { Start = region.RegionId ; Destination = ngh ; Size = size }
                    | _ ->
                        ()
            | _ ->
                ()
    ]


let createRoadConvoyOrders coalition =
    createConvoyOrders (ColumnMovement.MaxColumnSize, ResupplyOrder.TruckCapacity) (fun world -> world.Roads) coalition
    >> List.mapi (fun i convoy -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByRoad; Convoy = convoy })


let createRailConvoyOrders coalition =
    createConvoyOrders (1, ResupplyOrder.TrainCapacity) (fun world -> world.Rails) coalition
    >> List.mapi (fun i convoy -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByRail; Convoy = convoy })


let createAllConvoyOrders coalition x =
    createRoadConvoyOrders coalition x @ createRailConvoyOrders coalition x


/// Prioritize convoys according to needs of destination
let prioritizeConvoys (world : World) (state : WorldState) (orders : ResupplyOrder list) =
    let needs =
        computeSupplyNeeds world state
    let sorted =
        orders
        // Sort by supply needs
        |> List.sortByDescending (fun order ->
            Map.tryFind order.Convoy.Destination needs
            |> fun x -> defaultArg x 0.0f<E>)
        // At most one convoy of each type from each region
        |> List.fold (fun (starts, ok) order ->
            let source = (order.Means, order.Convoy.Start)
            if Set.contains source starts then
                (starts, ok)
            else
                (Set.add source starts, order :: ok)
        ) (Set.empty, [])
        |> snd
        |> List.rev
    sorted


let createGroundInvasionOrders (coalition : CoalitionId, world : World, state : WorldState) =
    let random = System.Random()
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    seq {
        for region in world.Regions do
            let regState = sg.GetRegion region.RegionId
            if regState.Owner = Some coalition then
                let targets =
                    region.Neighbours
                    |> Seq.filter(fun ngh ->
                        let nghState = sg.GetRegion ngh
                        match nghState.Owner with
                        | None -> true
                        | Some other -> other <> coalition
                    )
                for target in targets do
                    let composition =
                        regState.NumVehicles
                    let composition =
                        expandMap composition
                        |> Array.shuffle random
                    yield {
                        OrderId = {
                                    Index = 0 // Set later to a unique value
                                    Coalition = coalition
                        }
                        Start = region.RegionId
                        Destination = target
                        Composition = composition
                    }
    }
    |> Seq.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = i + 1 } })
    |> List.ofSeq

let valueOfVehicles =
    Seq.sumBy (
        function
        | HeavyTank -> 3.0
        | MediumTank -> 2.0
        | LightArmor -> 1.0)

let valueOfVehicles2 =
    Map.map (fun k num ->
        (float num) *
        match k with
        | HeavyTank -> 3.0
        | MediumTank -> 2.0
        | LightArmor -> 1.0
    )
    >> Map.toSeq
    >> Seq.sumBy snd


let getInvasionSuccessProbablity(world : World, state : WorldState) (order : ColumnMovement) =
    let sg = WorldStateFastAccess.Create state
    let defenseArea =
        world.AntiTankDefenses
        |> List.tryFind (fun area -> area.Home = FrontLine(order.Destination, order.Start))
    match defenseArea with
    | Some defenseArea ->
        let defenses = sg.GetAntiTankDefenses(defenseArea.DefenseAreaId)
        let attackValue =
            order.Composition
            |> valueOfVehicles
        let staticDefenseValue =
            float defenses.NumUnits
        let mobileDefenseValue =
            sg.GetRegion(order.Destination).NumVehicles
            |> valueOfVehicles2
        let defenseValue = staticDefenseValue + mobileDefenseValue
        match attackValue with
        | 0.0 -> 0.0
        | attackValue ->
            let ratio = defenseValue / attackValue
            System.Math.Pow(2.0, -ratio)
    | None ->
        0.5


let getInvasionValue (world, state) =
    let sg = WorldStateFastAccess.Create state
    let getRegionDistances =
        let distanceToAxisFactories =
            computeRegionDistances false (fun world -> world.Roads) (fun region -> sg.GetRegion(region).Owner) (Axis, world)
        let distanceToAlliesFactories =
            computeRegionDistances false (fun world -> world.Roads) (fun region -> sg.GetRegion(region).Owner) (Allies, world)
        fun attacker ->
            match attacker with
            | Some Axis -> distanceToAlliesFactories
            | Some Allies -> distanceToAxisFactories
            | None -> failwith "Start of attacking column has no owner"
    fun order ->
        let distances = getRegionDistances (sg.GetRegion(order.Start).Owner)
        match Map.tryFind order.Destination distances with
        | Some dist -> 1.0 / float(1 + dist)
        | None -> 0.0


let prioritizeGroundInvasionOrders(world : World, state : WorldState) (orders : ColumnMovement list) =
    let targetValue = getInvasionValue(world, state)
    let successProbability = getInvasionSuccessProbablity(world, state)
    orders
    |> List.map (fun order -> order, successProbability order, targetValue order)
    |> List.sortByDescending (fun (order, probability, gain) -> probability * gain)
    |> List.distinctBy(fun (order, _, _) -> order.Start)


let prioritizedReinforcementOrders(world : World, state : WorldState) coalition invasions =
    let invasions =
        invasions
        |> Seq.groupBy (fun (invasion, _, _) -> invasion.Start)
        |> dict
    let reinforcementNeed region =
        // invasions starting in region
        match invasions.TryGetValue region with
        | true, invasions ->
            invasions
            |> Seq.map (fun (invasion, probability, value) -> (1.0 - probability) * valueOfVehicles invasion.Composition)
            |> Seq.max
        | false, _ ->
            0.0
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let rec propagate reinforcementValues working =
        match working with
        | [] -> reinforcementValues
        | (region, value) :: rest ->
            let addAndMoveOn() =
                let newWorking =
                    wg.GetRegion(region).Neighbours
                    |> List.filter(fun r -> sg.GetRegion(r).Owner = Some coalition)
                    |> List.map (fun r -> r, 0.5 * value)
                propagate (Map.add region value reinforcementValues) (rest @ newWorking)
            match Map.tryFind region reinforcementValues with
            | Some oldValue ->
                if oldValue < value then
                    addAndMoveOn()
                else
                    propagate reinforcementValues rest
            | None ->
                addAndMoveOn()
    let reinforcementValues =
        world.Regions
        |> List.filter (fun r -> sg.GetRegion(r.RegionId).Owner = Some coalition)
        |> List.map (fun r -> r.RegionId, reinforcementNeed r.RegionId)
        |> propagate Map.empty
    [
        for region in world.Regions do
            if sg.GetRegion(region.RegionId).Owner = Some coalition then
                for ngh in region.Neighbours do
                    if sg.GetRegion(ngh).Owner = Some coalition then
                        let excessSource =
                            valueOfVehicles2(sg.GetRegion(region.RegionId).NumVehicles)
                        let excessDestination =
                            valueOfVehicles2(sg.GetRegion(ngh).NumVehicles) - reinforcementValues.[ngh]
                        yield (region.RegionId, ngh, excessSource - excessDestination)
    ]
    |> List.sortByDescending (fun (src, dest, transit) -> transit)
    |> List.mapi (fun i (src, dest, transit) ->
        let vehicles = sg.GetRegion(src).NumVehicles
        let k =
            transit / valueOfVehicles2 vehicles
            |> min 1.0
        let composition =
            vehicles
            |> Map.map (fun veh num -> int(ceil(k * float num)))
            |> expandMap
            |> Array.shuffle (System.Random())
        { OrderId = {
                        Index = i + 1
                        Coalition = coalition
          }
          Start = src
          Destination = dest
          Composition = composition
        }
    )


let filterIncompatible(reinforcements, invasions) =
    let safeInvasions =
        invasions
        |> List.filter (fun (invasion, probability, value) -> probability >= 0.5)
    let safeInvasionStarts =
        safeInvasions
        |> Seq.map (fun (invasion, _, _) -> invasion.Start)
        |> Set.ofSeq
    let compatibleReinforcements =
        reinforcements
        |> List.filter (fun reinforcement -> not(safeInvasionStarts.Contains(reinforcement.Start)))
    compatibleReinforcements, safeInvasions
