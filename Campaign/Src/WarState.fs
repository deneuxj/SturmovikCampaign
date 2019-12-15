// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2019 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

namespace Campaign.WarState

open System
open System.Numerics
open System.Collections.Generic

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription
open Campaign.NewWorldDescription
open Util

type TargetType =
    | Truck | Train | Ship | Battleship | Artillery | Tank | ArmoredCar | Bridge
    | Building of BuildingProperties * int
    | ParkedPlane of PlaneType
    | Air of PlaneType

type Target =
    {
        Kind : TargetType
        Pos : Vector2
    }

type AmmoType =
    Rocket | Bullets | Bomb

type ReturnType =
    CrashedInEnemyTerritory | CrashedInFriendlyTerritory | AtAirfield of AirfieldId

/// The results of a flight by a player, used to build success rates of virtual missions.
type MissionRecord =
    {
        Date : DateTime
        Plane : PlaneModel
        Start : AirfieldId
        TargetsDestroyed : (Target * AmmoType) list
        Return : ReturnType
    }

/// How resources in a region should be used
type ResourceUsagePriority =
    | RepairRegion of TargetStorageCapacity: float32<E>
    | RepairBridges of TargetTransportCapacity: float32<E/H>
    | RepairAirfield of TargetStorageCapacity: float32<E>
    | RetainForRegion of TargetStorage: float32<E>
    | RefillAirfield of TargetStorage: float32<E>

/// The overall status of the war.
type WarState =
    {
        World : World
        RegionOwners : IDictionary<RegionId, CoalitionId>
        BuildingPartFillLevel : IDictionary<BuildingInstanceId * int, float32>
        BuildingPartHealthLevel : IDictionary<BuildingInstanceId * int, float32>
        AirfieldPlanes : IDictionary<AirfieldId, Dictionary<PlaneModel, float32>>
        ResourceUsagePriorities : IDictionary<RegionId, ResourceUsagePriority list>
        Date : DateTime
        MissionRecords : MissionRecord list
    }
with
    /// Relative level of space used in a subpart of a building, relatively to full health availability
    member this.GetBuildingPartFillLevel(bid, part) =
        assert(this.World.Buildings.ContainsKey(bid))
        match this.BuildingPartFillLevel.TryGetValue((bid, part)) with
        | true, x -> x
        | false, _ -> 0.0f

    /// Set relative occupancy of a building part.
    member this.SetBuildingPartFillLevel(bid, part, x) =
        assert(this.World.Buildings.ContainsKey(bid))
        if x = 0.0f then
            this.BuildingPartFillLevel.Remove((bid, part)) |> ignore
        else
            this.BuildingPartFillLevel.[(bid, part)] <- x

    /// Amount of resources stored in a building.
    member this.GetBuildingStorage(bid) =
        let building = this.World.Buildings.[bid]
        building.Properties.SubParts
        |> List.sumBy (fun part -> this.GetBuildingPartFillLevel(bid, part))
        |> (*) building.Properties.PartCapacity

    /// Level of health of a subpart of a building or bridge
    member this.GetBuildingPartHealthLevel(bid, part) =
        match this.BuildingPartHealthLevel.TryGetValue((bid, part)) with
        | true, x -> x
        | false, _ -> 1.0f

    /// Max amount of resources that can be stored in a building, depending on health but not taking into account current amounts.
    member this.GetBuildingCapacity(bid) =
        assert(this.World.Buildings.ContainsKey(bid))
        let building = this.World.Buildings.[bid]
        building.Properties.SubParts
        |> List.sumBy (fun part -> this.GetBuildingPartFunctionalityLevel(bid, part))
        |> (*) building.Properties.PartCapacity

    /// Level of functionality of a subpart of a building or bridge
    member this.GetBuildingPartFunctionalityLevel(bid, part) =
        let health = this.GetBuildingPartHealthLevel(bid, part)
        if health < 0.5f then
            0.0f
        else
            health

    /// Set the level of health of a subpart of a building or a bridge
    member this.SetBuildingPartHealthLevel(bid, part, x) =
        if x = 1.0f then
            this.BuildingPartHealthLevel.Remove((bid, part)) |> ignore
        else
            this.BuildingPartHealthLevel.[(bid, part)] <- x

    /// Level of functionality of a bridge
    member this.GetBridgeFunctionalityLevel(bid) =
        let building = this.World.Bridges.[bid]
        building.Properties.SubParts
        |> List.fold (fun level part ->
            this.GetBuildingPartFunctionalityLevel(bid, part)
            |> min level
        ) 1.0f

    /// Get transport link capacity
    member this.GetFlowCapacity(link : NetworkLink) =
        let functionality =
            link.Bridges
            |> Seq.map (fun bid -> this.GetBridgeFunctionalityLevel(bid))
            |> Seq.fold min 1.0f
        functionality * link.FlowCapacity

module private WarState_ =
    let CachedRoads = Util.cachedProperty (fun x -> x.World.Roads.QuickAccess)
    let CachedRails = Util.cachedProperty (fun x -> x.World.Rails.QuickAccess)

    /// Cached computation of a mapping from a coalition to distances to regions owned by this coalition
    let DistancesToCoalition =
        Util.cachedProperty (fun (this : WarState) ->
            [Allies; Axis]
            |> List.map (fun coalition ->
                let sources =
                    this.RegionOwners
                    |> Seq.choose (fun kvp ->
                        if kvp.Value = coalition then
                            Some kvp.Key
                        else None)
                    |> Set
                let distances =
                    this.World.Regions
                    |> Seq.map (fun region -> region.RegionId, System.Int32.MaxValue)
                    |> Seq.mutableDict
                let nghOf =
                    this.World.Regions
                    |> Seq.map (fun region -> region.RegionId, Set region.Neighbours)
                    |> dict
                for region in sources do
                    distances.[region] <- 0
                let rec work (xs : Set<RegionId>) dist =
                    let nghs =
                        xs
                        |> Seq.map (fun region -> nghOf.[region])
                        |> Set.unionMany
                    let next =
                        nghs
                        |> Set.filter (fun region -> distances.[region] > dist)
                    for ngh in next do
                        distances.[ngh] <- dist
                    work next (dist + 1)
                work sources 1
                coalition, distances)
            |> Map.ofList
        )

    /// Compute the flow capacity of a transport network between a set of starting nodes (sources) and destinations (sinks),
    /// constrained to the nodes inside a set of regions
    let ComputeTransportCapacity(this : WarState, network : NetworkQuickAccess, regions : Set<RegionId>, sources : Set<int>, sinks : Set<int>) =
        let flow = Seq.mutableDict []
        let pred = Seq.mutableDict []
        // Utility function to iterate over predecessor links in pred
        let rec walkPred action link =
            match link with
            | None -> ()
            | Some link ->
                action link
                let link = pred.TryGetValue(link.NodeA) |> Option.ofPair
                walkPred action link
        // Edmonds-Karp algorithm
        let mutable ret = 0.0f<E/H>
        let rec forEachAugmentationPath() =
            let queue = System.Collections.Generic.Queue()
            // Run a breadth-first-search to find the shortest path from the sources to the sinks
            for s in sources do
                queue.Enqueue(s)
            while queue.Count > 0 do
                let node = queue.Dequeue()
                let successors, getLink = network.GetLink(node)
                // Constrain the path search inside the provided regions
                let successors =
                    successors
                    |> Seq.filter (fun node ->
                        let region = network.GetNode(node).Region
                        regions.Contains(region))
                for succ in successors do
                    let link = getLink succ
                    let flow =
                        flow.TryGetValue((node, succ))
                        |> Option.ofPair
                        |> Option.defaultValue 0.0f<E/H>
                    if not(sinks.Contains(succ)) && this.GetFlowCapacity(link) > flow then
                        if not (pred.ContainsKey succ) then
                            pred.[succ] <- link
                            queue.Enqueue(succ)
            let mutable stop = true
            for sink in sinks do
                match pred.TryGetValue(sink) with
                | true, prec ->
                    stop <- false
                    // See how much more flow we can send
                    let mutable df = 1.0f<E/H> * System.Single.PositiveInfinity
                    Some prec
                    |> walkPred (fun link ->
                            let flow =
                                flow.TryGetValue((link.NodeA, link.NodeB))
                                |> Option.ofPair
                                |> Option.defaultValue 0.0f<E/H>
                            df <- min df (this.GetFlowCapacity(link) - flow))
                    // Update flow by that amount
                    Some prec
                    |> walkPred (fun link ->
                        let link = link.NodeA, link.NodeB
                        let x =
                            flow.TryGetValue(link)
                            |> Option.ofPair
                            |> Option.defaultValue 0.0f<E/H>
                        flow.[link] <- x + df
                        let link = snd link, fst link
                        let x =
                            flow.TryGetValue(link)
                            |> Option.ofPair
                            |> Option.defaultValue 0.0f<E/H>
                        flow.[link] <- x - df)
                    ret <- ret + df
                | false, _ ->
                    ()
            if not stop then
                forEachAugmentationPath()
        forEachAugmentationPath()
        ret

    let ComputeTransportCapacityCached x network =
        SturmovikMission.DataProvider.Cached.cached
            (Seq.mutableDict [])
            (fun (regionA, regionB) ->
                let regions = Set [regionA; regionB]
                let regionA, regionB =
                    min regionA regionB, max regionA regionB
                let terminalsInRegion region =
                    network.Data.Nodes
                    |> Seq.filter (fun node ->
                        node.Region = region && node.HasTerminal)
                    |> Seq.map (fun node -> node.Id)
                    |> Set.ofSeq
                let sources = terminalsInRegion regionA
                let sinks = terminalsInRegion regionB
                ComputeTransportCapacity(x, network, regions, sources, sinks)
            )

    let RoadTransportCapacity =
        Util.cachedProperty (fun this -> ComputeTransportCapacityCached this (CachedRoads this))

    let RailTransportCapacity =
        Util.cachedProperty (fun this -> ComputeTransportCapacityCached this (CachedRails this))


type WarState with
    /// Assign resources to a building. Returns unassigned resources.
    member this.AssignResources (bid : BuildingInstanceId, rsc : float32<E>) =
        assert(this.World.Buildings.ContainsKey(bid))
        let building = this.World.Buildings.[bid]
        let partCapacity = building.Properties.PartCapacity
        let rsc =
            building.Properties.SubParts
            |> List.fold (fun rsc subpart ->
                let stored = partCapacity * this.GetBuildingPartFillLevel(bid, subpart)
                let capacity = partCapacity * this.GetBuildingPartFunctionalityLevel(bid, subpart)
                let transferred =
                    min rsc (capacity - stored)
                    |> max 0.0f<E>
                this.SetBuildingPartFillLevel(bid, subpart, (stored + transferred) / partCapacity)
                rsc - transferred
            ) rsc
        rsc

    /// Assign resources to multiple buildings. Returns unassigned resources.
    member this.DistributeResources (rsc : float32<E>) (buildings : BuildingInstanceId list) =
        // Group buildings by proximity
        let groups =
            let areClose (b1 : BuildingInstanceId) (b2 : BuildingInstanceId) =
                let b1 = this.World.Buildings.[b1]
                let b2 = this.World.Buildings.[b2]
                (b1.Pos.Pos - b2.Pos.Pos).Length() < 100.0f
            Util.Algo.computePartition areClose buildings
        // Order by fill level. We distribute in priority to group that already have 50% to 75% fill level,
        // then to groups that are less than 50%, and finally to groups that are more than 75%
        let groups =
            groups
            |> List.sortByDescending (fun group ->
                let stored = group |> List.sumBy this.GetBuildingStorage
                let capacity = group |> List.sumBy this.GetBuildingCapacity
                if capacity <= 0.0f<E> then
                    1, None
                else
                    match stored / capacity with
                    | x when x > 0.75f -> 2, Some (1.0f - x)
                    | x when x < 0.5f -> 3, Some x
                    | x -> 4, Some x)
        // Assign resources to sorted buildings
        let rsc =
            groups
            |> List.concat
            |> List.fold (fun rsc bid ->
                let rsc = this.AssignResources(bid, rsc)
                rsc) rsc
        // Result
        rsc

    /// Get the list of priorities on how to spend resources in a region
    member this.GetResourceUsagePriorities(region) =
        match this.ResourceUsagePriorities.TryGetValue(region) with
        | true, xs -> xs
        | false, _ -> []

    /// Set the list of priorities on how to spend resources in a region
    member this.SetResourceUsagePriorities(region, prios) =
        this.ResourceUsagePriorities.[region] <- prios

    /// Get the mapping from a coalition to distances to regions owned by this coalition
    member this.DistanceToCoalition = WarState_.DistancesToCoalition this

    /// Get the mappings for quick access to road nodes and links
    member this.Roads = WarState_.CachedRoads this

    /// Get the mappings for quick access to rail nodes and links
    member this.Rails = WarState_.CachedRails this

    /// Get the roads transport capacity between two adjacent regions
    member this.ComputeRoadsCapacity =
        WarState_.RoadTransportCapacity this

    /// Get the rails transport capacity between two adjacent regions
    member this.ComputeRailsCapacity =
        WarState_.RailTransportCapacity this


[<RequireQualifiedAccess>]
module Init =
    /// Create the initial status of the war
    let mkWar (world : World) =
        let regionOwners =
            world.Regions
            |> List.choose (fun desc-> desc.InitialOwner |> Option.map (fun owner -> desc.RegionId, owner))
            |> Seq.mutableDict
        let airfields =
            world.Airfields
            |> Seq.map (fun af -> af.AirfieldId, Seq.mutableDict [])
            |> Seq.mutableDict
        let allFilled = Seq.mutableDict []
        for building in world.Buildings.Values do
            for part in building.Properties.SubParts do
                allFilled.Add((building.Id, part), 1.0f)
        {
            World = world
            RegionOwners = regionOwners
            ResourceUsagePriorities = Seq.mutableDict []
            BuildingPartFillLevel = allFilled
            BuildingPartHealthLevel = Seq.mutableDict []
            AirfieldPlanes = airfields
            Date = world.StartDate
            MissionRecords = []
        }