module Campaign.AutoOrder

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders

let createConvoyOrders coalition (world : World) (state : WorldState) =
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
        world.Roads
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
                        yield { Start = region.RegionId ; Destination = ngh }
                    | _ ->
                        ()
            | _ ->
                ()
    ]


let prioritizeConvoys (maxConvoys : int) (world : World) (state : WorldState) (convoys : ConvoyOrder list) =
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
        convoys
        // Remove convoys that start from poorly filled regions
        |> List.filter (fun convoy ->
            let filledUp = (getStorageContent convoy.Start) / (getStorageCapacity convoy.Start)
            filledUp > 0.5f)
        // Sort by supply capacity
        |> List.sortBy (fun convoy ->
            let receiveCapacity = (getStorageCapacity convoy.Destination) - (getStorageContent convoy.Destination)
            let sendCapacity = getStorageContent convoy.Start
            -1.0f * (min sendCapacity receiveCapacity))
        // At most one convoy from each region
        |> List.fold (fun (starts, ok) convoy ->
            if Set.contains convoy.Start starts then
                (starts, ok)
            else
                (Set.add convoy.Start starts, convoy :: ok)
        ) (Set.empty, [])
        |> snd
        |> List.rev
        // At most one convoy to each region
        |> List.fold (fun (destinations, ok) convoy ->
            if Set.contains convoy.Destination destinations then
                (destinations, ok)
            else
                (Set.add convoy.Destination destinations, convoy :: ok)
        ) (Set.empty, [])
        |> snd
        |> List.rev
    sorted
    |> List.take (min maxConvoys (List.length sorted))