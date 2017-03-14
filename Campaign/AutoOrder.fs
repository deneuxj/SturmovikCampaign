module Campaign.AutoOrder

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Vector
open Util

let createConvoyOrders (getPaths : World -> Path list) (coalition : CoalitionId) (world : World, state : WorldState) =
    let getRegion =
        let m =
            world.Regions
            |> List.map (fun region -> region.RegionId, region)
            |> dict
        fun x -> m.[x]
    let getOwner =
        let m =
            state.Regions
            |> List.map (fun state -> state.RegionId, state.Owner)
            |> dict
        fun x -> m.[x]
    let distances = computeRegionDistances getPaths getOwner (coalition, world)
    let areConnectedByRoad(start, destination) =
        getPaths world
        |> List.exists (fun path ->
            path.StartId = start && path.EndId = destination || path.StartId = destination && path.EndId = start
        )
    [
        for region in world.Regions do
            match Map.tryFind region.RegionId distances with
            | Some level ->
                for ngh in region.Neighbours do
                    match Map.tryFind ngh distances with
                    | Some other when other > level && areConnectedByRoad(region.RegionId, ngh)->
                        yield { Start = region.RegionId ; Destination = ngh ; Size = 8 }
                    | _ ->
                        ()
            | _ ->
                ()
    ]


let createRoadConvoyOrders coalition =
    createConvoyOrders (fun world -> world.Roads) coalition
    >> List.mapi (fun i convoy -> { Index = i + 1; Coalition = coalition; Means = ByRoad; Convoy = convoy })


let createRailConvoyOrders coalition =
    createConvoyOrders (fun world -> world.Rails) coalition
    >> List.mapi (fun i convoy -> { Index = i + 1; Coalition = coalition; Means = ByRail; Convoy = convoy })


let createAllConvoyOrders coalition x =
    createRoadConvoyOrders coalition x @ createRailConvoyOrders coalition x


let prioritizeConvoys (maxConvoys : int) (dt : float32<H>) (world : World) (state : WorldState) (orders : ResupplyOrder list) =
    let getState =
        let m =
            state.Regions
            |> Seq.map (fun state -> state.RegionId, state)
            |> dict
        fun x -> m.[x]
    let getAirfield =
        let m =
            world.Airfields
            |> Seq.map (fun af -> af.AirfieldId, af)
            |> dict
        fun x -> m.[x]
    let getStorageCapacity =
        let m =
            seq {
                for region in world.Regions do
                    let afStorage =
                        state.Airfields
                        |> Seq.filter (fun af -> (getAirfield af.AirfieldId).Region = region.RegionId)
                        |> Seq.sumBy (fun af ->
                            Seq.zip (getAirfield af.AirfieldId).Storage af.StorageHealth
                            |> Seq.sumBy (fun (building, health) -> health * getWeightCapacityPerBuilding building.Model))
                    let regionStorage =
                        let state = getState region.RegionId
                        Seq.zip region.Storage state.StorageHealth
                        |> Seq.sumBy (fun (building, health) -> health * getWeightCapacityPerBuilding building.Model)
                    yield region.RegionId, regionStorage + afStorage
            }
            |> dict
        fun x -> m.[x]
    let getStorageContent =
        let m =
            seq {
                for region in state.Regions do
                    let afStorage =
                        state.Airfields
                        |> Seq.filter (fun af -> (getAirfield af.AirfieldId).Region = region.RegionId)
                        |> Seq.sumBy (fun af -> af.BombWeight + (float32 af.NumRockets) * rocketWeight)
                    yield region.RegionId, region.ShellCount * shellWeight + afStorage
            }
            |> dict
        fun x -> m.[x]
    let sorted =
        orders
        // Remove convoys that start from poorly filled regions
        |> List.filter (fun order ->
            let filledUp = (getStorageContent order.Convoy.Start) / (getStorageCapacity order.Convoy.Start)
            filledUp > 0.5f)
        // Sort by supply capacity
        |> List.sortBy (fun order ->
            let receiveCapacity = (getStorageCapacity order.Convoy.Destination) - (getStorageContent order.Convoy.Destination)
            let sendCapacity =
                getStorageContent order.Convoy.Start
                |> min (shellWeight * order.Capacity / shellCost)
            -1.0f * (min sendCapacity receiveCapacity))
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
    |> List.take (min maxConvoys (List.length sorted))


let createColumnMovementOrders criterion (coalition : CoalitionId, world : World, state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    seq {
        for region in world.Regions do
            let regState = sg.GetRegion region.RegionId
            if regState.Owner = Some coalition then
                let target =
                    region.Neighbours
                    |> Seq.tryFind(fun ngh ->
                        let nghState = sg.GetRegion ngh
                        criterion(nghState.Owner, coalition)
                    )
                match target with
                | Some target ->
                    let total_vehicles =
                        regState.NumVehicles
                        |> Map.toSeq
                        |> Seq.sumBy snd
                    let factor =
                        if total_vehicles > 15 then
                            15.0f / float32 total_vehicles
                        else
                            1.0f
                    let rec adjust(acc, items) =
                        match items with
                        | [] -> []
                        | (veh, num) :: rest ->
                            let adjusted = factor * float32 num
                            let waste = adjusted - floor adjusted
                            let adjusted, acc =
                                if acc + waste < 1.0f then
                                    floor adjusted, acc + waste
                                else
                                    ceil adjusted, acc + waste - 1.0f
                            (veh, int adjusted) :: adjust(acc, rest)
                    let composition =
                        regState.NumVehicles
                        |> Map.toList
                        |> fun items -> adjust(0.0f, items)
                        |> Map.ofList
                    assert((composition |> Map.toSeq |> Seq.sumBy snd) <= 15)
                    let composition = expandMap composition
                    yield {
                        Index = 0 // Set later to a unique value
                        Coalition = coalition
                        Start = region.RegionId
                        Destination = target
                        Composition = composition
                    }
                | None ->
                    ()
    }
    |> Seq.mapi (fun i order -> { order with Index = i + 1 })
    |> List.ofSeq


let createGroundInvasionOrders =
    createColumnMovementOrders (fun (a, b) -> match a with Some a -> a <> b | None -> false)


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


let getInvasionSuccessProbablity(world, state) (order : ColumnMovement) =
    let sg = WorldStateFastAccess.Create state
    let defenseArea =
        world.AntiTankDefenses
        |> List.tryFind (fun area -> area.Home = FrontLine(order.Destination, order.Start))
    match defenseArea with
    | Some defenseArea ->
        let defenses = sg.GetDefenseArea(defenseArea.DefenseAreaId)

        let attackValue =
            order.Composition
            |> valueOfVehicles
        let staticDefenseValue =
            float defenses.NumUnits
        let mobileDefenseValue =
            sg.GetRegion(order.Destination).NumVehicles
            |> valueOfVehicles2
        let defenseValue = staticDefenseValue + mobileDefenseValue
        let ratio = defenseValue / (0.1 + attackValue)
        System.Math.Exp(-ratio)
    | None ->
        0.5


let getInvasionValue (world, state) =
    let sg = WorldStateFastAccess.Create state
    let getRegionDistances =
        let distanceOwnedByAxis =
            computeRegionDistances (fun world -> world.Roads) (fun region -> sg.GetRegion(region).Owner) (Axis, world)
        let distanceOwnedByAllies =
            computeRegionDistances (fun world -> world.Roads) (fun region -> sg.GetRegion(region).Owner) (Allies, world)
        function
        | Some Axis -> distanceOwnedByAxis
        | Some Allies -> distanceOwnedByAllies
        | None -> failwith "Start of attacking column has no owner"
    fun order ->
        let distances = getRegionDistances (sg.GetRegion(order.Start).Owner |> Option.map (fun x -> x.Other))
        match Map.tryFind order.Destination distances with
        | Some dist -> 1.0 / float(1 + dist)
        | None -> 0.0


let prioritizeGroundInvasionOrders(world : World, state : WorldState) (orders : ColumnMovement list) =
    let targetValue = getInvasionValue(world, state)
    let successProbability = getInvasionSuccessProbablity(world, state)
    orders
    |> List.map (fun order -> order, successProbability order, targetValue order)
    |> List.sortByDescending (fun (order, probability, gain) -> probability * gain)


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
        { Index = i + 1
          Coalition = coalition
          Start = src
          Destination = dest
          Composition = composition
        }
    )


let filterIncompatible(reinforcements, invasions) =
    let safeInvasions =
        invasions
        |> List.filter (fun (invasion, probability, value) ->
            let hasAltReinforcement =
                reinforcements
                |> List.exists (fun reinforcement -> reinforcement.Start = invasion.Start)
            not hasAltReinforcement || probability > 0.5)
    let compatibleReinforcements =
        reinforcements
        |> List.filter (fun reinforcement ->
            safeInvasions
            |> List.exists (fun (invasion, _, _) -> invasion.Start = reinforcement.Start)
            |> not)
    compatibleReinforcements, safeInvasions
