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
    let frontLine = computeFrontLine false world state.Regions
    // Amounts of extra anti-tank and anti-air canons needed to have the region fully defended.
    // Can be negative.
    let regionCanonNeeds =
        seq {
            for antiTank, antiTankState in Seq.zip world.AntiTankDefenses state.AntiTankDefenses do
                match antiTank.Home with
                | FrontLine(home, ngh) when frontLine.Contains(home, ngh)->
                    yield home, float32(antiTank.MaxNumGuns - antiTankState.NumUnits) * cannonCost
                | _ ->
                    ()
            for antiAir, antiAirState in Seq.zip world.AntiAirDefenses state.AntiAirDefenses do
                match antiAir.Home with
                | Central(home) ->
                    yield home, float32(antiAir.MaxNumGuns - antiAirState.NumUnits) * cannonCost
                | _ ->
                    ()
        }
        |> Seq.groupBy fst
        |> Seq.map (fun (region, costs) -> region, costs |> Seq.sumBy snd)
    // Costs for canons adjusted by storage capacity. Can be negative
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
    let distances = computeDistanceFromFactories true (getPaths >> List.map fst) getOwner world coalition
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
    createConvoyOrders (2.0f * ResupplyOrder.TruckCapacity) (float32 ColumnMovement.MaxColumnSize * ResupplyOrder.TruckCapacity) (fun world -> world.Roads |> List.map (fun x -> x, ())) coalition
    >> List.mapi (fun i (convoy, ()) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByRoad; Convoy = convoy })


let createRailConvoyOrders coalition =
    createConvoyOrders (0.0f<E>) (ResupplyOrder.TrainCapacity) (fun world -> world.Rails |> List.map (fun x -> x, ())) coalition
    >> List.mapi (fun i (convoy, ()) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByRail; Convoy = convoy })


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
    createRoadConvoyOrders coalition x @ createRailConvoyOrders coalition x


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
    sorted

/// Build an order from a move computed by the minmax search.
let realizeMove (world : World) (state : WorldState) (move : Move) =
    let regStart = world.Regions.[move.Start].RegionId
    let regDest = world.Regions.[move.Destination].RegionId
    let regState = state.Regions.[move.Start]
    let owner = regState.Owner
    if owner.IsNone then
        failwith "Cannot start tank column from neutral zone"
    let content =
        regState.NumVehicles
        |> expandMap
        |> Array.shuffle (System.Random())
        |> Array.fold (fun (forceLeft, column) vehicle ->
            if forceLeft > 0.0f<E> then
                forceLeft - vehicle.Cost, vehicle :: column
            else
                forceLeft, column
        ) (move.Force, [])
        |> snd
        |> Array.ofList
    { OrderId = { Index = -1; Coalition = owner.Value }
      Start = regStart
      Destination = regDest
      Composition = content
    }

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
