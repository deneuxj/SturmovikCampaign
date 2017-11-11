module Campaign.Orders

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.BasicTypes
open Campaign.PlaneModel

/// A truck convoy or a train, in movement.
type ConvoyOrder = {
    Start : RegionId
    Destination : RegionId
    TransportedSupplies : float32<E>
}
with
    member this.EndPoints =
        [ this.Start; this.Destination ]
        |> List.sort

type ResupplyMeans =
    | ByRoad
    | ByRail
    | ByAir of AirfieldId * AirfieldId

type OrderId = {
    Index : int
    Coalition : CoalitionId
}
with
    member this.AsString() =
        sprintf "%d-%d" this.Index (int this.Coalition.ToCoalition)

let private tryExtractNumberSuffix (prefix : string) (name : string) =
    let number =
        lazy
            try
                match System.Int32.TryParse(name.Substring(prefix.Length)) with
                | true, x -> Some x
                | false, _ -> None
            with
            | _ -> None
    if name.StartsWith(prefix) && number.Value.IsSome then
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
            | ByAir _ -> "A"
        sprintf "CNV-%s-%d-%d" meansLetter (int this.OrderId.Coalition.ToCoalition) this.OrderId.Index

    member this.MatchesMissionLogArrivalEventName(name : string) =
        name.StartsWith(this.MissionLogEventName + "-A-")

    member this.MatchesMissionLogDepartureEventName(name : string) =
        name.StartsWith(this.MissionLogEventName + "-D")

    member this.MatchesMissionLogVehicleKilledEventName(name : string) =
        if name.StartsWith(this.MissionLogEventName + "-K-") then
            tryExtractNumberSuffix (this.MissionLogEventName + "-K-") name
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
    /// Try to extract the rank of a vehicle that has been killed from a mission log event name.
    /// </summary>
    member this.MatchesMissionLogVehicleKilledEventName(name : string) =
        if name.Contains("K") then
            if name.StartsWith(this.MissionLogEventName + "-K-") then
                tryExtractNumberSuffix (this.MissionLogEventName + "-K-") name
            else
                None
        else
            None

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

    /// Return true iff this movement order sends troops into (non-neutral) enemy territory.
    member this.IsInvasion(state : WorldState) =
        match state.GetRegion(this.Start).Owner, state.GetRegion(this.Destination).Owner with
        | Some x, Some y -> x <> y
        | _ -> false

    // The maximum number of vehicles following the leader in a column.
    static member MaxColumnSize = SturmovikMission.Blocks.VirtualConvoy.Factory.VirtualConvoy.MaxConvoySize

/// Ferry planes from airfields to airfields
type PlaneFerryOrder = {
    OrderId : OrderId
    Plane : PlaneModel
    Qty : int
    Start : AirfieldId
    Destination : AirfieldId
}
with
    member this.SpawnedEventName = sprintf "FerryPlane-D-%s" (this.OrderId.AsString())
    member this.LandedEventName = sprintf "FerryPlane-A-%s" (this.OrderId.AsString())
    member this.KilledEventName = sprintf "FerryPlane-K-%s" (this.OrderId.AsString())

    /// What to produce in each category of production, and how much does each category need
type ProductionPriorities = {
    Vehicle : GroundAttackVehicle
    PriorityVehicle : float32<E>
    PrioritySupplies : float32<E>
}

/// Groups all orders for a faction.
type OrderPackage = {
    Resupply : ResupplyOrder list
    Columns : ColumnMovement list
    Patrols : AiPlanes.AiPatrol list
    Attacks : AiPlanes.AiAttack list
    PlaneFerries : PlaneFerryOrder list
    Production : ProductionPriorities
}
with
    /// Make sure every order has an unique index.
    member this.Renumber() =
        let resupply = this.Resupply |> List.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = i }})
        let offset = List.length resupply
        let columns = this.Columns |> List.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = offset + i }})
        let offset = offset + List.length columns
        let ferries = this.PlaneFerries |> List.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = offset + i }})
        { this with
            Resupply = resupply
            Columns = columns
            PlaneFerries = ferries
        }
