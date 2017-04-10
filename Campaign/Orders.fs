module Campaign.Orders

open Campaign.WorldDescription
open Campaign.WorldState

/// A truck convoy or a train, in movement.
type ConvoyOrder = {
    Start : RegionId
    Destination : RegionId
    TransportedSupplies : float32<E>
}

type ResupplyMeans =
    | ByRoad
    | ByRail

type OrderId = {
    Index : int
    Coalition : CoalitionId
}

let private tryExtractNumberSuffix (prefix : string) (name : string) =
    let number =
        lazy
            try
                match System.Int32.TryParse(name.Substring(prefix.Length)) with
                | true, x -> Some x
                | false, _ -> None
            with
            | _ -> None
    if name.StartsWith(prefix + "-") && number.Value.IsSome then
        number.Value
    else
        None

type ResupplyOrder = {
    OrderId : OrderId
    Means : ResupplyMeans
    Convoy : ConvoyOrder
}
with
    static member TruckCapacity = 100.0f<E>
    static member TrainCapacity = 2000.0f<E>

    member this.MissionLogEventName =
        let meansLetter =
            match this.Means with
            | ByRoad -> "R"
            | ByRail -> "T"
        sprintf "CNV-%s-%d-%d" meansLetter (int this.OrderId.Coalition.ToCoalition) this.OrderId.Index

    member this.MatchesMissionLogArrivalEventName(name : string) =
        name.StartsWith(this.MissionLogEventName + "-A-")

    member this.MatchesMissionLogDepartureEventName(name : string) =
        name.StartsWith(this.MissionLogEventName + "-D")

    member this.MatchesVehicleName(name : string) =
        tryExtractNumberSuffix this.MissionLogEventName name

let private tryExtractNumberPairSuffix (prefix : string) (name : string) =
    let number =
        lazy
            try
                let suffix = name.Substring(prefix.Length)
                let nums = suffix.Split('-')
                match nums with
                | [|num1; num2|] ->
                    match System.Int32.TryParse(num1), System.Int32.TryParse(num2) with
                    | (true, n1), (true, n2) -> Some(n1, n2)
                    | _ -> None
                | _ -> None
            with
            | _ -> None
    if name.StartsWith(prefix + "-") && number.Value.IsSome then
        number.Value
    else
        None

/// A column of armored vehicles in movement.
type ColumnMovement = {
    OrderId : OrderId
    Start : RegionId
    Destination : RegionId
    Composition : GroundAttackVehicle[]
}
with
    member this.MissionLogEventName =
        sprintf "COL-%d-%d" (int this.OrderId.Coalition.ToCoalition) this.OrderId.Index

    /// <summary>
    /// Try to get the rank of a vehicle in a column by its name
    /// </summary>
    member this.MatchesVehicleName(name : string) =
        tryExtractNumberPairSuffix this.MissionLogEventName name
        |> Option.map (fun (n1, n2) -> n1 + n2)

    member this.MatchesMissionLogArrivalEventName(name : string) =
        if name.StartsWith(this.MissionLogEventName + "-A-") then
            // Return the number past the last -, which denotes the vehicle rank in the column.
            // Used to credit the destination region with an additional vehicle
            match name.LastIndexOf("-") with
            | n when n >= 0 ->
                match System.Int32.TryParse(name.Substring(n + 1)) with
                | true, x -> Some x
                | _ -> None
            | _ ->
                None
        else
            None

    member this.MatchesMissionLogDepartureEventName(name : string) =
        if name.StartsWith(this.MissionLogEventName + "-D-") then
            // Return the rank offset
            // When a large column has been split into smaller groups, this makes it possible to identify the group which has departed.
            match name.LastIndexOf("-") with
            | n when n >= 0 ->
                match System.Int32.TryParse(name.Substring(n + 1)) with
                | true, x -> Some x
                | _ -> None
            | _ ->
                None
        else
            None

    // The maximum number of vehicles following the leader in a column.
    static member MaxColumnSize = SturmovikMission.Blocks.VirtualConvoy.Factory.VirtualConvoy.MaxConvoySize


/// Groups all orders for a faction.
type OrderPackage = {
    Resupply : ResupplyOrder list
    Columns : ColumnMovement list
}