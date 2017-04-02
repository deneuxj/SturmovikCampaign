module Campaign.Orders

open Campaign.WorldDescription
open Campaign.WorldState

/// A truck convoy or a train, in movement.
type ConvoyOrder = {
    Start : RegionId
    Destination : RegionId
    /// For a truck convoy: Number of trucks.
    Size : int
}

type ResupplyMeans =
    | ByRoad
    | ByRail

type OrderId = {
    Index : int
    Coalition : CoalitionId
}

type ResupplyOrder = {
    OrderId : OrderId
    Means : ResupplyMeans
    Convoy : ConvoyOrder
}
with
    static member TruckCapacity = 100.0f<E>
    static member TrainCapacity = 2000.0f<E>

    member this.Capacity =
        match this.Means with
        | ByRoad -> float32 this.Convoy.Size * ResupplyOrder.TruckCapacity
        | ByRail -> ResupplyOrder.TrainCapacity

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