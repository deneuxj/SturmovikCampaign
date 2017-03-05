module Campaign.AutoOrder

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Vector

let createConvoyOrders (getPaths : World -> Path list) (coalition : CoalitionId, world : World, state : WorldState) =
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
    let distances = Functions.computeRegionDistances getPaths getOwner (coalition, world)
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


let createRoadConvoyOrders =
    createConvoyOrders (fun world -> world.Roads)
    >> List.map (fun convoy -> { Means = ByRoad; Convoy = convoy })


let createRailConvoyOrders =
    createConvoyOrders (fun world -> world.Rails)
    >> List.map (fun convoy -> { Means = ByRail; Convoy = convoy })


let createAllConvoyOrders x =
    createRoadConvoyOrders x @ createRailConvoyOrders x


let prioritizeConvoys (maxConvoys : int) (world : World) (state : WorldState) (orders : ResupplyOrder list) =
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
                            |> Seq.sumBy (fun (building, health) -> health * Functions.getWeightCapacityPerBuilding building.Model))
                    let regionStorage =
                        let state = getState region.RegionId
                        Seq.zip region.Storage state.StorageHealth
                        |> Seq.sumBy (fun (building, health) -> health * Functions.getWeightCapacityPerBuilding building.Model)
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
                        |> Seq.sumBy (fun af -> af.BombWeight + (float32 af.NumRockets) * AirfieldState.RocketWeight)
                    yield region.RegionId, region.ShellCount * RegionState.ShellWeight + afStorage
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
                |> min order.Capacity
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


let createColumnMovementOrders criterion (coalition : CoalitionId option, world : World, state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    [
        for region in world.Regions do
            let regState = sg.GetRegion region.RegionId
            if regState.Owner = coalition then
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
                    yield {
                        Start = region.RegionId
                        Destination = target
                        Composition = composition
                    }
                | None ->
                    ()
    ]


let createGroundInvasionOrders =
    createColumnMovementOrders (fun (a, b) -> a <> b)


let createReinforcementOrders =
    createColumnMovementOrders (fun (a, b) -> a = b)


let getInvasionSuccessProbablity(world, state) (order : ColumnMovement) =
    let sg = WorldStateFastAccess.Create state
    let defenseArea =
        world.AntiTankDefenses
        |> List.tryFind (fun area -> area.Home = FrontLine(order.Destination, order.Start))
    match defenseArea with
    | Some defenseArea ->
        let defenses = sg.GetDefenseArea(defenseArea.DefenseAreaId)
        let valueOfVehicles =
            Map.toSeq
            >> Seq.sumBy (fun (vehicle, number) ->
                let k =
                    match vehicle with
                    | HeavyTank -> 3.0
                    | MediumTank -> 2.0
                    | LightArmor -> 1.0
                k * float number)

        let attackValue =
            order.Composition
            |> valueOfVehicles
        let staticDefenseValue =
            float defenses.NumUnits
        let mobileDefenseValue =
            sg.GetRegion(order.Destination).NumVehicles
            |> valueOfVehicles
        let defenseValue = staticDefenseValue + mobileDefenseValue
        let ratio = defenseValue / (0.1 + attackValue)
        System.Math.Exp(-ratio)
    | None ->
        0.5


let getInvasionValue (world, state) =
    let sg = WorldStateFastAccess.Create state
    let getRegionDistances =
        let distanceOwnedByAxis =
            Functions.computeRegionDistances (fun world -> world.Roads) (fun region -> sg.GetRegion(region).Owner) (Axis, world)
        let distanceOwnedByAllies =
            Functions.computeRegionDistances (fun world -> world.Roads) (fun region -> sg.GetRegion(region).Owner) (Allies, world)
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


let prioritizeReinforcementOrders(world : World, state : WorldState) (reinforcements, invasions) =
    let invasions =
        invasions
        |> Seq.groupBy (fun (invasion, _, _) -> invasion.Start)
        |> dict
    let importanceOfReinforcement reinforcement =
        match invasions.TryGetValue reinforcement.Start with
        | true, invasions ->
            invasions
            |> Seq.map (fun (invasion, probability, value) -> value / probability)
            |> Seq.max
        | false, _ ->
            0.0
    reinforcements
    |> List.sortByDescending importanceOfReinforcement


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
