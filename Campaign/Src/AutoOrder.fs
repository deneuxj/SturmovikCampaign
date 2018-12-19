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

module Campaign.AutoOrder

open System.Threading

open VectorExtension

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Campaign.MinMax
open Util
open Campaign.BasicTypes
open Campaign.PlaneModel
open AiPlanes

let private logger = NLog.LogManager.GetCurrentClassLogger()

/// Compute the full-health storage capacity of each region, including airfields'
let computeStorageCapacity (world : World) =
    let afCapacity =
        seq {
            for af in world.Airfields do
                let capacity =
                    af.Storage
                    |> Seq.sumBy (fun building -> building.Storage world.SubBlockSpecs)
                yield af.Region, capacity
        }
    let regCapacity =
        seq {
            for reg in world.Regions do
                let capacity =
                    reg.Storage
                    |> Seq.sumBy (fun building -> building.Storage world.SubBlockSpecs)
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
                let capacity = buildingsStorageCapacity world.SubBlockSpecs af.Storage afState.StorageHealth
                yield af.Region, capacity
        }
    let regCapacity =
        seq {
            for reg, regState in List.zip world.Regions state.Regions do
                let capacity = buildingsStorageCapacity world.SubBlockSpecs reg.Storage regState.StorageHealth
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
let computeSupplyNeeds (missionDuration : float32<H>) (world : World) (state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let wg = WorldFastAccess.Create world
    // Bombs at airfields. Needs can be negative.
    let afNeeds =
        seq {
            for af, afs in Seq.zip world.Airfields state.Airfields do
                let bombNeed =
                    afs.BombNeeds * bombCost
                    |> min (afs.StorageCapacity(af, world.SubBlockSpecs))
                yield af.Region, bombNeed - afs.Supplies
        }
    // Ammo needs. Can be negative.
    let regionAmmoCost = state.GetSupplyNeeds(world, missionDuration)
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
                yield region, cost
        }
    Seq.concat [ afNeeds ; regionSaturatedCanonNeeds ]
    |> Seq.groupBy fst
    |> Seq.map (fun (region, costs) -> region, costs |> Seq.sumBy snd)
    |> Map.ofSeq

/// Compute repair needs.
let computeRepairNeeds (missionDuration : float32<H>) (world : World) (state : WorldState) =
    let airfieldRepairs =
        seq {
            for af, afs in Seq.zip world.Airfields state.Airfields do
                let repairs =
                    Seq.zip af.Storage afs.StorageHealth
                    |> Seq.sumBy (fun (building, health) -> (1.0f - Array.avg health) * building.RepairCost(world.SubBlockSpecs))
                yield af.Region, repairs
        }
        |> Seq.groupBy fst
        |> Seq.map (fun (region, xs) -> region, xs |> Seq.sumBy snd)
    let regionRepairs =
        seq {
            for region, regState in List.zip world.Regions state.Regions do
                let repairs =
                    List.zip region.Production regState.ProductionHealth
                    |> List.sumBy (fun (building, health) ->
                        (1.0f - Array.avg health) *  building.RepairCost(world.SubBlockSpecs)
                        |> min (world.RepairSpeed * missionDuration))
                yield region.RegionId, repairs
        }
    let totalRepairs =
        Seq.append airfieldRepairs regionRepairs
        |> Seq.groupBy fst
        |> Seq.map (fun (region, xs) -> region, xs |> Seq.sumBy snd)
        |> Seq.map (fun (region, amount) -> region, min (world.RegionRepairSpeed * missionDuration) amount)
        |> Map.ofSeq
    totalRepairs

/// Forward the supply/repair needs of regions at the frontline to regions at the back
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
let createConvoyOrders (missionLength : float32<H>) (minTransfer : float32<E>) (maxTransfer : float32<E>) (getPaths : World -> (Path * 'D) list) (coalition : CoalitionId) (world : World, state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let getOwner = sg.GetRegion >> (fun x -> x.Owner)
    let distances = computeDistanceFromFactories (getPaths >> List.map fst) getOwner world coalition
    let areConnected(start, destination) =
        getPaths world
        |> List.choose (fun (path, data) -> if path.MatchesEndpoints(start, destination).IsSome then Some data else None)
    let capacities = computeStorageCapacity world
    let storages = computeStorage world state
    let needs = computeSupplyNeeds missionLength world state
    let forwardedNeeds = computeForwardedNeeds world state needs
    let repairs = computeRepairNeeds missionLength world state
    let forwardedRepairs = computeForwardedNeeds world state repairs
    let tryFind x y = Map.tryFind x y |> Option.defaultVal 0.0f<E>
    [
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && regState.Supplies > 0.0f<E> then
                let senderOwnNeeds = tryFind region.RegionId needs + tryFind region.RegionId repairs 
                let senderForwardedNeeds = tryFind region.RegionId forwardedNeeds + tryFind region.RegionId forwardedRepairs
                let senderDistance =
                    Map.tryFind region.RegionId distances
                    |> Option.defaultVal System.Int32.MaxValue
                for ngh in region.Neighbours do
                    if getOwner ngh = Some coalition then
                        for data in areConnected(region.RegionId, ngh) do
                            let receiverDistance =
                                Map.tryFind ngh distances
                                |> Option.defaultVal System.Int32.MaxValue
                            let receiverOwnNeeds = tryFind ngh needs + tryFind ngh repairs
                            let receiverForwardedNeeds = tryFind ngh forwardedNeeds + tryFind ngh forwardedRepairs
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


let createRoadConvoyOrders missionLength coalition =
    createConvoyOrders missionLength (6.0f * ResupplyOrder.TruckCapacity) (float32 ColumnMovement.MaxColumnSize * ResupplyOrder.TruckCapacity) (fun world -> world.Roads |> List.map (fun x -> x, ())) coalition
    >> List.mapi (fun i (convoy, ()) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByRoad; Convoy = convoy })


let createRailConvoyOrders missionLength coalition =
    createConvoyOrders missionLength (0.0f<E>) (ResupplyOrder.TrainCapacity) (fun world -> world.Rails |> List.map (fun x -> x, ())) coalition
    >> List.mapi (fun i (convoy, ()) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = ByRail; Convoy = convoy })

let createShipConvoyOrders missionLength coalition =
    createConvoyOrders
        missionLength
        (0.0f<E>)
        (2.0f * ResupplyOrder.ShipCapacity)
        (fun world -> (world.SeaWays |> List.map (fun x -> x, BySeaShip)) @ (world.RiverWays |> List.map (fun x -> x, ByRiverShip)))
        coalition
    >> List.mapi (fun i (convoy, means) -> { OrderId = { Index = i + 1; Coalition = coalition }; Means = means; Convoy = convoy })

let createAirConvoyOrders missionLength coalition =
    let exactCapacity = (PlaneModel.Ju52.CargoCapacity * bombCost)
    createConvoyOrders
        missionLength
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

let createAllConvoyOrders missionLength coalition x =
    createShipConvoyOrders missionLength coalition x @ createRoadConvoyOrders missionLength coalition x @ createRailConvoyOrders missionLength coalition x @ createAirConvoyOrders missionLength coalition x

/// Prioritize convoys according to needs of destination
let prioritizeConvoys (missionLength : float32<H>) (world : World) (state : WorldState) (orders : ResupplyOrder list) =
    let needs = state.GetSupplyNeeds(world, missionLength)
    let repairs = computeRepairNeeds missionLength world state
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
        // Sort by supply and repair needs
        |> List.sortByDescending (fun order ->
            (needs.TryFind(order.Convoy.Destination) |> Option.defaultValue 0.0f<E>) +
            (repairs.TryFind(order.Convoy.Destination) |> Option.defaultValue 0.0f<E>))
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

/// Select vehicles among those available in a region. Pick up vehicles up to a specified force.
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

/// Select vehicles among those available in a region. Pick up to a specified number of vehicles.
let selectLimitedNumberOfVehicles (regState : RegionState) (maxNumVehicles : int) =
    let content =
        regState.NumVehicles
        |> expandMap
        |> Array.shuffle (System.Random())
        |> Array.truncate maxNumVehicles
    content

/// Check if a set of paths contains a path between two regions
let hasPath (start, destination) (paths : Path list) =
    paths
    |> List.exists (fun path -> path.MatchesEndpoints(start, destination).IsSome)

/// Try to get a kind of path that connects two regions
/// Prioritize in that order: trains (if allowed), roads, sea ways
let tryGetPathKind transport (world : World) (start, destination) =
    let hasPath = hasPath (start, destination)
    if hasPath (world.PathsFor transport) then
        Some transport
    elif hasPath world.Rails then
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
    match tryGetPathKind move.Transport world (regStart, regDest) with
    | Some medium ->
        { OrderId = { Index = -1; Coalition = owner.Value }
          Start = regStart
          Destination = regDest
          Composition = content
          TransportType = medium
        }
    | None ->
        failwithf "Cannot realize move between %s and %s" (string regStart) (string regDest)

type CoalitionsDecision =
    | Surrender of CoalitionId * string
    | Continue of ColumnMovement list

/// Run a minmax search for the best column moves for each coalition.
let decideColumnMovements (world : World) (state : WorldState) thinkTime =
    let board, neighboursOf = BoardState.Create(world, state)
    let minMax cancel n = minMax cancel n (fun (x, y) -> neighboursOf x y)
    let rec timeBound cancel prev n board =
        //printfn "Max depth: %d" n
        let res = minMax cancel n board
        if cancel.IsCancellationRequested || n >= 100 then
            prev
        else
            logger.Info(sprintf "Board value at depth %d: %A" n res)
            timeBound cancel res (n + 1) board
    let minMax board =
        use cancellation = new CancellationTokenSource()
        cancellation.CancelAfter(thinkTime * 1000)
        timeBound cancellation.Token ({ Axis = None; Allies = None }, Ongoing 0.0f) 1 board
    let moves, mark =
        minMax board
    match mark with
    | Defeat(coalition, depth, reason) when (state.Date - world.StartDate).TotalDays >= 7.0 && depth <= 4 ->
        Surrender(coalition, reason)
    | Defeat(coalition, depth, reason) when (state.Date - world.StartDate).TotalDays >= 2.0 && depth <= 2 ->
        Surrender(coalition, reason)
    | _ ->
        let { Axis = m1; Allies = m2 } = moves
        (Option.toList m1 @ Option.toList m2)
        |> List.map (realizeMove world state)
        |> Continue

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
                            if nghState.Owner = Some coalition && nghDistance < regionDistance && not nghState.HasInvaders then
                                let hasPath = hasPath (region.RegionId, ngh)
                                for medium in ColumnTransportType.All do
                                    if hasPath (world.PathsFor medium) then
                                        let maxVehicleNumber = medium.MaxNumVehicles
                                        let composition = selectLimitedNumberOfVehicles regState maxVehicleNumber
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
    |]
    |> Array.sortBy (fun order -> -sg.GetRegion(order.Start).TotalVehicleValue)

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
let computeProductionPriorities (missionLength : float32<H>) (coalition : CoalitionId) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state

    let supplyNeed =
        let allNeeds = computeSupplyNeeds missionLength world state
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
        let target =
            state.Regions
            |> Seq.filter (fun state -> // Regions at the front
                let region = wg.GetRegion state.RegionId
                region.Neighbours
                |> Seq.exists (fun ngh -> sg.GetRegion(ngh).Owner <> Some coalition))
            |> Seq.sumBy (fun state ->
                let desiredValue = 3.0f * GroundAttackVehicle.HeavyTankCost + 9.0f * GroundAttackVehicle.MediumTankCost + 3.0f * GroundAttackVehicle.LightArmorCost
                let desiredValue = (float32 world.TankTargetNumber / 15.0f) * desiredValue
                desiredValue)
        let available =
            state.Regions
            |> Seq.sumBy (fun reg -> reg.TotalVehicleValue)
        let need = target - available
        // Choice of vehicle type depends of current vehicle type balance
        let getNumVehicles(v) =
            state.Regions
            |> Seq.sumBy (fun reg -> reg.GetNumVehicles(coalition, v))
        let numLight = getNumVehicles(LightArmor)
        let numMedium = getNumVehicles(MediumTank)
        let numHeavy = getNumVehicles(HeavyTank)
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

    { Vehicle = vehicleToProduce
      PriorityVehicle = vehicleNeed
      PrioritySupplies = supplyNeed
    }

/// Decide how many planes to ferry, where from and where to.
let decidePlaneTransfers (world : World) (state : WorldState) (coalition : CoalitionId) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let targetNumPlanes = float32 world.TransferNumPlaneTarget
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
                let destination = sg.GetRegion(af.Region)
                if destination.Owner = Some coalition then
                    // Do not send planes to airfields that are about to be conquered
                    let threatFromNeighbour =
                        let reg = wg.GetRegion(af.Region)
                        reg.Neighbours
                        |> Seq.exists(fun ngh ->
                            let regs = sg.GetRegion(ngh)
                            regs.Owner <> Some coalition && regs.TotalVehicleValue >= 0.75f * sg.GetRegion(reg.RegionId).TotalVehicleValue)
                    if not threatFromNeighbour && not destination.HasInvaders then
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
