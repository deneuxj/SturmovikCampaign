module Campaign.AutoOrder

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Vector

let createConvoyOrders (getPaths : World -> Path list) (coalition : CoalitionId option, world : World, state : WorldState) =
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
    let areConnectedByRoad(start, destination) =
        getPaths world
        |> List.exists (fun path ->
            path.StartId = start && path.EndId = destination || path.StartId = destination && path.EndId = start
        )
    let rec work (distances : Map<RegionId, int>) (working : RegionId list) =
        match working with
        | [] -> distances
        | current :: rest ->
            let distance = distances.[current]
            let region = getRegion current
            let nghs =
                region.Neighbours
                |> Seq.filter (fun ngh -> coalition = getOwner ngh) // It belongs to the coalition
                |> Seq.filter (fun ngh -> areConnectedByRoad(region.RegionId, ngh))
                |> List.ofSeq
            let distances, working =
                nghs
                |> Seq.fold (fun (distances, working) ngh ->
                    match Map.tryFind ngh distances with
                    | Some oldDist when oldDist > distance + 1 ->
                        (Map.add ngh (distance + 1) distances, ngh :: working)
                    | Some _ -> (distances, working)
                    | None ->
                        (Map.add ngh (distance + 1) distances, ngh :: working)
                ) (distances, rest)
            work distances working
    let sources =
        world.Regions
        |> Seq.filter (fun region -> not <| List.isEmpty region.Production)
        |> Seq.filter (fun region -> coalition = getOwner region.RegionId)
        |> Seq.map (fun region -> region.RegionId)
        |> List.ofSeq
    let distances0 =
        sources
        |> Seq.map (fun region -> region, 0)
        |> Map.ofSeq
    let distances = work distances0 sources
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


let createColumns (coalition : CoalitionId option, world : World, state : WorldState) =
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
                        nghState.Owner <> coalition
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
