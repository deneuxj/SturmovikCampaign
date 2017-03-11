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

type ResupplyOrder = {
    Index : int
    Means : ResupplyMeans
    Convoy : ConvoyOrder
}
with
    static member TruckCapacity = 1000.0f<M>
    static member TrainCapacity = 20000.0f<M>

    member this.Capacity =
        match this.Means with
        | ByRoad -> float32 this.Convoy.Size * ResupplyOrder.TruckCapacity
        | ByRail -> ResupplyOrder.TrainCapacity

/// A column of armored vehicles in movement.
type ColumnMovement = {
    Index : int
    Start : RegionId
    Destination : RegionId
    Composition : Map<GroundAttackVehicle, int>
}

/// Groups all orders for a faction.
type OrderPackage = {
    Resupply : ResupplyOrder list
    Reinforcements : ColumnMovement list
    Invasions : ColumnMovement list
}