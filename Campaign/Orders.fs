module Campaign.Orders

open Campaign.WorldDescription

type ConvoyOrder = {
    Start : RegionId
    Destination : RegionId
}

