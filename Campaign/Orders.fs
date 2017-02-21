module Campaign.Orders

open Campaign.WorldDescription

type ConvoyOrder = {
    Start : RegionId
    Destination : RegionId
    Size : int
}

type ResupplyMeans =
    | ByRoad
    | ByRail

type ResupplyOrder = {
    Means : ResupplyMeans
    Convoy : ConvoyOrder
}
with
    member this.Capacity =
        match this.Means with
        | ByRoad -> float32 this.Convoy.Size * 1000.0f
        | ByRail -> 20000.0f
