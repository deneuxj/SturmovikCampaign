module Campaign.AutoOrder

open System.Threading

open VectorExtension

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Campaign.MinMax
open Campaign.Util
open Campaign.BasicTypes
open Campaign.PlaneModel
open AiPlanes

/// Compute the full-health storage capacity of each region, including airfields'
let computeStorageCapacity (world : World) =
    let afCapacity =
        seq {
            for af in world.Airfields do
                let capacity =
                    af.Storage
                    |> Seq.sumBy (fun building -> building.Storage)
                yield af.Region, capacity
        }
    let regCapacity =
        seq {
            for reg in world.Regions do
                let capacity =
                    reg.Storage
                    |> Seq.sumBy (fun building -> building.Storage)
                yield reg.RegionId, capacity
        }
    Seq.concat [ afCapacity; regCapacity ]
    |> Seq.groupBy fst
    |> Seq.map (fun (region, caps) -> region, caps |> Seq.sumBy snd)
    |> Map.ofSeq

/// Compute the actual storage capacity of each region, including airfields'
let computeActualStorageCapacity (world : World) (state : WorldState) =
    let afCapacity =
        seq {
            for af, afState in List.zip world.Airfields state.Airfields do
                let capacity =
                    List.zip af.Storage afState.StorageHealth
                    |> Seq.sumBy (fun (building, health) -> health * building.Storage)
                yield af.Region, capacity
        }
    let regCapacity =
        seq {
            for reg, regState in List.zip world.Regions state.Regions do
                let capacity =
                    List.zip reg.Storage regState.StorageHealth
                    |> Seq.sumBy (fun (building, health) -> health * building.Storage)
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

/// Compute the additional supply needs of each region. Can be negative if the region has more resources it needs for itself.
let computeSupplyNeeds (world : World) (state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let wg = WorldFastAccess.Create world
    // Bombs and repairs at airfields. Needs can be negative.
    let afNeeds =
        seq {
            for af, afs in Seq.zip world.Airfields state.Airfields do
                let bombNeed =
                    afs.BombNeeds * bombCost
                    |> min (afs.StorageCapacity(af))
                let repairs =
                    Seq.zip af.Storage afs.StorageHealth
                    |> Seq.sumBy (fun (building, health) -> (1.0f - health) * building.RepairCost)
                yield af.Region, bombNeed + repairs - afs.Supplies
        }
    // Ammo needs. Can be negative.
    let regionAmmoCost = state.GetAmmoCostPerRegion(world)
    let regionCanonNeeds =
        regionAmmoCost
        |> Map.map (fun region cost -> cost - sg.GetRegion(region).Supplies)
    // Costs for canons adjusted by storage capacity. Can be negative
    let regionSaturatedCanonNeeds =
        let capacities = computeStorageCapacity world
        seq {
            for region, costs in regionCanonNeeds |> Map.toSeq do
                let capacity =
                    Map.tryFind region capacities
                    |> Option.defaultVal 0.0f<E>
                let regState = sg.GetRegion(region)
                let reg = wg.GetRegion(region)
                let cost =
                    min (capacity - regState.Supplies) costs
                let repairs =
                    List.zip reg.Production regState.ProductionHealth
                    |> List.sumBy (fun (building, health) -> (1.0f - health) *  building.RepairCost)
                yield region, cost + repairs
        }
    Seq.concat [ afNeeds ; regionSaturatedCanonNeeds ]
    |> Seq.groupBy fst
    |> Seq.map (fun (region, costs) -> region, costs |> Seq.sumBy snd)
    |> Map.ofSeq

/// Forward the supply needs of regions at the frontline to regions at the back
let computeForwardedNeeds (world : World) (state : WorldState) (needs : Map<RegionId, float32<E>>) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let inFront =
        let frontLine = computeFrontLine true world state.Regions
        frontLine
        |> Seq.map fst
        |> Set.ofSeq
    let distances = computeDistance true (fun world -> world.Roads) (fun r -> sg.GetRegion(r).Owner) inFront.Contains world
    let getSuccessors (region : RegionId) =
        let region = wg.GetRegion(region)
        match Map.tryFind region.RegionId distances with
        | Some lvl ->
            region.Neighbours
            |> List.filter (fun ngh ->
                match Map.tryFind ngh distances with
                | Some lvl2 -> lvl2 > lvl
                | _ -> false)
        | None ->
            []
    let update _ (region : RegionId) oldValue newValue =
        match oldValue with
        | None -> Some newValue
        | Some oldValue ->
            if newValue > oldValue then
                Some newValue
            else
                None
    let roots =
        inFront
        |> Seq.map (fun region -> Map.tryFind region needs |> Option.defaultVal 0.0f<E> |> fun value -> region, value)
        |> List.ofSeq
    Algo.propagate getSuccessors update roots


/// Create convoy orders from regions owned by a coalition to neighbour regions that have bigger needs.
let createConvoyOrders (minTransfer : float32<E>) (maxTransfer : float32<E>) (getPaths : World -> (Path * 'D) list) (coalition : CoalitionId) (world : World, state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let getOwner = sg.GetRegion >> (fun x -> x.Owner)
    let distances = computeDistanceFromFactories (getPaths >> List.map fst) getOwner world coalition
    let areConnectedByRoad(start, destination) =
        getPaths world
        |> List.choose (fun (path, data) -> if path.MatchesEndpoints(start, destination).IsSome then Some data else None)
    let capacities = computeStorageCapacity world
    let storages = computeStorage world state
    let needs = computeSupplyNeeds world state
    let forwardedNeeds = computeForwardedNeeds world state needs
    let tryFind x y = Map.tryFind x y |> Option.defaultVal 0.0f<E>
    [
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && regState.Supplies > 0.0f<E> then
                let senderOwnNeeds = tryFind region.RegionId needs
                let senderForwardedNeeds = tryFind region.RegionId forwardedNeeds
                let senderDistance =
                    Map.tryFind region.RegionId distances
                    |> Option.defaultVal System.Int32.MaxValue
                for ngh in region.Neighbours do
                    if getOwner ngh = Some coalition then
                        for data in areConnectedByRoad(region.RegionId, ngh) do
                            let receiverDistance =
                                Map.tryFind ngh distances
                                |> Option.defaultVal System.Int32.MaxValue
                            let receiverOwnNeeds = tryFind ngh needs
                            let receiverForwardedNeeds = tryFind ngh forwardedNeeds
                            let senderNeeds, receiverNeeds =
                                if senderDistance < receiverDistance then
                                    senderOwnNeeds, receiverOwnNeeds + receiverForwardedNeeds
                                elif senderDistance > receiverDistance then
                                    senderOwnNeeds + senderForwardedNeeds, receiverOwnNeeds
                                else
                                    senderOwnNeeds, receiverOwnNeeds
                            if receiverNeeds > senderNeeds then
                                let transfer =
                                    let alreadyAtReceiver = tryFind ngh storages
                                    let availableToReceive = tryFind ngh capacities - alreadyAtReceiver
                                    let requested = min receiverNeeds availableToReceive
                                    let willingToSend = regState.Supplies - 0.75f * senderNeeds
                                    if willingToSend >= minTransfer then
                                        willingToSend
                                        |> min requested
                                        |> min maxTransfer
                                        |> max minTransfer
                                    else
                                        0.0f<E>
                                if transfer >= minTransfer then
                                    yield { Start = region.RegionId ; Destination = ngh ; TransportedSupplies = transfer }, data
    ]


let createRoadConvoyOrders coalition =
    createConvoyOrders (6.0f * ResupplyOrder.TruckCapacity) (float32 ColumnMovement.MaxColumnSize * ResupplyOrder.TruckCapacity) (fun world -> world.Roads |> List.map (fun x -> x, ())) coalition
    >> List.mapi (fun i (convoy, ()) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByRoad; Convoy = convoy })


let createRailConvoyOrders coalition =
    createConvoyOrders (0.0f<E>) (ResupplyOrder.TrainCapacity) (fun world -> world.Rails |> List.map (fun x -> x, ())) coalition
    >> List.mapi (fun i (convoy, ()) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByRail; Convoy = convoy })

let createShipConvoyOrders coalition =
    createConvoyOrders
        (0.0f<E>)
        (2.0f * ResupplyOrder.ShipCapacity)
        (fun world -> (world.SeaWays |> List.map (fun x -> x, BySeaShip)) @ (world.RiverWays |> List.map (fun x -> x, ByRiverShip)))
        coalition
    >> List.mapi (fun i (convoy, means) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = means; Convoy = convoy })

let createAirConvoyOrders coalition =
    let exactCapacity = (PlaneModel.Ju52.CargoCapacity * bombCost)
    createConvoyOrders
        exactCapacity
        exactCapacity
        (fun world ->
            [
                for af1 in world.Airfields do
                    for af2 in world.Airfields do
                        if af1 <> af2 && (af1.Pos - af2.Pos).Length() < 100000.0f then
                            yield { StartId = af1.Region; EndId = af2.Region; Locations = [] }, (af1.AirfieldId, af2.AirfieldId)
            ])
        coalition
    >> List.mapi (fun i (convoy, (af1, af2)) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByAir(af1, af2); Convoy = convoy })

let createAllConvoyOrders coalition x =
    createShipConvoyOrders coalition x @ createRoadConvoyOrders coalition x @ createRailConvoyOrders coalition x @ createAirConvoyOrders coalition x

/// Prioritize convoys according to needs of destination
let prioritizeConvoys (world : World) (state : WorldState) (orders : ResupplyOrder list) =
    let needs =
        computeSupplyNeeds world state
    let remaining =
        state.Regions
        |> List.map (fun regState -> regState.RegionId, regState.Supplies)
        |> Map.ofList
    let sorted =
        orders
        // Remove flights that start from airfields without transport planes
        |> List.filter (fun order ->
            match order.Means with
            | ByAir(af0, _) ->
                let afs =
                    state.Airfields
                    |> List.find (fun af -> af.AirfieldId = af0)
                afs.NumPlanes
                |> Map.exists (fun plane qty -> plane.Roles.Contains PlaneModel.CargoTransporter && qty >= 1.0f)
            | _ ->
                true)
        // Sort by supply needs
        |> List.sortByDescending (fun order ->
            Map.tryFind order.Convoy.Destination needs
            |> fun x -> defaultArg x 0.0f<E>)
        // Stop sending convoys if no more supplies are available at the source
        |> List.fold (fun (remaining, ok) order ->
            let source = (order.Means, order.Convoy.Start)
            let supplies = Map.tryFind order.Convoy.Start remaining |> Option.defaultVal 0.0f<E>
            let requested = order.Convoy.TransportedSupplies
            let transported = min supplies requested
            if transported > 0.0f<E> then
                let remaining = Map.add order.Convoy.Start (supplies - transported) remaining
                (remaining, { order with Convoy = { order.Convoy with TransportedSupplies = transported } } :: ok)
            else
                (remaining, ok)
            ) (remaining, [])
        |> snd
        |> List.rev
    // Remove duplicate / reversed trips
    let noRoundTrip =
        sorted
        |> List.fold (fun (endpoints, ok) order ->
            let orderEndPoints = order.Convoy.EndPoints
            if Set.contains orderEndPoints endpoints then
                (endpoints, ok)
            else
                (Set.add orderEndPoints endpoints, order :: ok)
        ) (Set.empty, [])
        |> snd
        |> List.rev
    noRoundTrip

/// Select vehicles among those available in a region
let selectVehicles (regState : RegionState) (force : float32<E>) =
    let content =
        regState.NumVehicles
        |> expandMap
        |> Array.shuffle (System.Random())
        |> Array.fold (fun (forceLeft, column) vehicle ->
            if forceLeft > 0.0f<E> then
                forceLeft - vehicle.Cost, vehicle :: column
            else
                forceLeft, column
        ) (force, [])
        |> snd
        |> Array.ofList
    content

/// Check if a set of paths contains a path between two regions
let hasPath (start, destination) (paths : Path list) =
    paths
    |> List.exists (fun path -> path.MatchesEndpoints(start, destination).IsSome)

/// Try to get a kind of path that connects two regions
/// Prioritize in that order: trains (if allowed), roads, sea ways
let tryGetPathKind allowTrains (world : World) (start, destination) =
    let hasPath = hasPath (start, destination)
    if allowTrains && hasPath world.Rails then
        Some ColByTrain
    elif hasPath world.Roads then
        Some ColByRoad
    elif hasPath world.SeaWays then
        Some ColBySeaShip
    elif hasPath world.RiverWays then
        Some ColByRiverShip
    else
        None

/// Build an order from a move computed by the minmax search.
let realizeMove (world : World) (state : WorldState) (move : Move) =
    let regStart = world.Regions.[move.Start].RegionId
    let regDest = world.Regions.[move.Destination].RegionId
    let regState = state.Regions.[move.Start]
    let owner = regState.Owner
    if owner.IsNone then
        failwith "Cannot start tank column from neutral zone"
    let content =
        selectVehicles regState move.Force
    match tryGetPathKind move.AllowTrains world (regStart, regDest) with
    | Some medium ->
        { OrderId = { Index = -1; Coalition = owner.Value }
          Start = regStart
          Destination = regDest
          Composition = content
          TransportType = medium
        }
    | None ->
        failwithf "Cannot realize move between %s and %s" (string regStart) (string regDest)

/// Run a minmax search for the best column moves for each coalition.
let decideColumnMovements (world : World) (state : WorldState) thinkTime =
    let board, neighboursOf = BoardState.Create(world, state)
    let minMax cancel n = minMax cancel n neighboursOf
    let rec timeBound cancel prev n board =
        //printfn "Max depth: %d" n
        let res = minMax cancel n board
        if cancel.IsCancellationRequested || n >= 100 then
            prev
        else
            timeBound cancel res (n + 1) board
    let minMax board =
        use cancellation = new CancellationTokenSource()
        cancellation.CancelAfter(thinkTime * 1000)
        timeBound cancellation.Token ({ Axis = None; Allies = None }, Ongoing 0.0f) 1 board
    minMax board
    |> fun ({ Axis = m1; Allies = m2 }, _) -> (Option.toList m1 @ Option.toList m2) |> List.map (realizeMove world state)

/// Move tanks from a rear region (where they typically are in excess) closer to the front line (where they typically are in need)
let allTankReinforcements (world : World) (state : WorldState) (coalition : CoalitionId) =
    let vehicleMinValue = GroundAttackVehicle.HeavyTank.Cost * 5.0f
    let sg = state.FastAccess
    let distanceToEnemy = computeDistance false (fun world -> world.Roads @ world.Rails @ world.SeaWays) (fun region -> sg.GetRegion(region).Owner) (fun region -> sg.GetRegion(region).Owner = Some coalition.Other) world
    [|
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && regState.TotalVehicleValue > vehicleMinValue then
                match Map.tryFind region.RegionId distanceToEnemy with
                | Some regionDistance ->
                    for ngh in region.Neighbours do
                        let nghState = sg.GetRegion(ngh)
                        match Map.tryFind ngh distanceToEnemy with
                        | Some nghDistance ->
                            // Note: do not move to a region on the frontline, this may be a poor tactical choice. Let the minmax search take that decision instead.
                            // This also avoids sending reinforcements into an ongoing battle, which is something the battle system does not handle well.
                            // If the battle is lost, the reinforcements will also be lost.
                            if nghState.Owner = Some coalition && nghDistance < regionDistance && nghDistance > 1 then
                                let composition = selectVehicles regState vehicleMinValue
                                let medium = tryGetPathKind true world (region.RegionId, ngh)
                                match medium with
                                | Some medium ->
                                    yield {
                                        OrderId = { Index = -1; Coalition = coalition }
                                        Start = region.RegionId
                                        Destination = ngh
                                        Composition = composition
                                        TransportType = medium
                                    }
                                | None ->
                                    ()
                        | None ->
                            ()
                | None ->
                    ()
    |]

/// Decide what plane to add to the rear airfield
let pickPlaneToProduce (coalition : CoalitionId) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state

    let planeTypeShares = PlaneModel.PlaneTypeShares(coalition)
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
    mostNeeded

/// Decide what vehicles and planes to produce, and how important they are.
let computeProductionPriorities (coalition : CoalitionId) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state

    let supplyNeed =
        let allNeeds = computeSupplyNeeds world state
        state.Regions
        |> Seq.sumBy (fun state ->
            if state.Owner = Some coalition then
                Map.tryFind state.RegionId allNeeds
                |> Option.defaultVal 0.0f<E>
            else
                0.0f<E>)
        |> max 0.0f<E>

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
    let vehicleNeed =
        max vehicleNeed 0.0f<E>
        |> min world.MaxTankNeeds // Limit tank objectives, otherwise it can dwarf the plane needs, depriving players from planes to fly.

    { Vehicle = vehicleToProduce
      PriorityVehicle = vehicleNeed
      PrioritySupplies = supplyNeed
    }

/// Decide how many planes to ferry, where from and where to.
let decidePlaneTransfers (world : World) (state : WorldState) (coalition : CoalitionId) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let targetNumPlanes = 3.0f
    let starts =
        seq {
            for af, afs in List.zip world.Airfields state.Airfields do
                if sg.GetRegion(af.Region).Owner = Some coalition then
                    for plane, count in afs.NumPlanes |> Map.toSeq do
                        let numAvailableToFerry =
                            count - targetNumPlanes
                            |> max 0.0f
                            |> floor
                            |> int
                        if numAvailableToFerry > 0 then
                            yield af.AirfieldId, plane, numAvailableToFerry
        }
        |> Seq.sortByDescending(fun (_, _, n) -> n)
        |> List.ofSeq
    let destinations =
        seq {
            for af, afs in List.zip world.Airfields state.Airfields do
                if sg.GetRegion(af.Region).Owner = Some coalition then
                    // Do not send planes to airfields that are about to be conquered
                    let afUnderThreat =
                        let reg = wg.GetRegion(af.Region)
                        reg.Neighbours
                        |> Seq.exists(fun ngh ->
                            let regs = sg.GetRegion(ngh)
                            regs.Owner <> Some coalition && regs.TotalVehicleValue >= 0.75f * sg.GetRegion(reg.RegionId).TotalVehicleValue)
                    if not afUnderThreat then
                        for (plane, count) in afs.NumPlanes |> Map.toSeq do
                            let numRequestedToFerry =
                                targetNumPlanes - count
                                |> max 0.0f
                                |> ceil
                                |> int
                            yield af.AirfieldId, plane, numRequestedToFerry
        }
        |> Seq.sortByDescending(fun (_, _, n) -> n)
        |> Seq.filter (fun (_, _, n) -> n > 0)
        |> List.ofSeq
    let rec tryFindMatchingStart ((af0, pt0, _) as destination) starts =
        match starts with
        | [] -> None
        | (af, pt, num) as hd :: rest ->
            if af <> af0 && pt = pt0 then
                Some hd
            else
                tryFindMatchingStart destination rest
    let rec matchAirfields starts destinations =
        seq {
            match destinations with
            | [] -> ()
            | destination :: rest ->
                match tryFindMatchingStart destination starts with
                | Some ((af0, plane0, _) as start) ->
                    yield start, destination
                    // Each airfield can only be used for one transport flight type
                    let starts =
                        starts
                        |> List.filter (fun (af, plane, _) -> (af, plane) <> (af0, plane0))
                    yield! matchAirfields starts rest
                | None ->
                    yield! matchAirfields starts rest
        }
    matchAirfields starts destinations
    |> Seq.map (fun ((af, plane, numSend), (af2, _, numReceive)) ->
        let count = sg.GetAirfield(af).NumPlanes.[plane]
        let count =
            count
            |> int
            |> min numSend
            |> min numReceive
        { OrderId = { Coalition = coalition; Index = 0 } // Index to be set when all orders have been decided
          Plane = plane
          Qty = count
          Start = af
          Destination = af2
        })
    |> List.ofSeq
