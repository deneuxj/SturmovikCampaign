module Campaign.Orders

open Campaign.WorldDescription
open Campaign.WorldState

type ConvoyOrder = {
    Start : RegionId
    Destination : RegionId
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
    member this.Capacity =
        match this.Means with
        | ByRoad -> float32 this.Convoy.Size * 1000.0f
        | ByRail -> 20000.0f

type ColumnMovement = {
    Index : int
    Start : RegionId
    Destination : RegionId
    Composition : Map<GroundAttackVehicle, int>
}
