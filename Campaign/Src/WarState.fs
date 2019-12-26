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

/// Shorthand for SturmovikMission.DataProvider.Cached
module private Cached =
    let cached = SturmovikMission.DataProvider.Cached.cached

/// Complex algorithms using data from WarState
module private Algo =
    /// Compute the flow capacity of a transport network between a set of starting nodes (sources) and destinations (sinks),
    /// constrained to the nodes inside a set of regions
    let computeTransportCapacity(getFlowCapacity, network : NetworkQuickAccess, regions : Set<RegionId>, sources : Set<int>, sinks : Set<int>) =
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
        let mutable ret = 0.0f<M^3/H>
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
                        |> Option.defaultValue 0.0f<M^3/H>
                    if not(sinks.Contains(succ)) && getFlowCapacity(link) > flow then
                        if not (pred.ContainsKey succ) then
                            pred.[succ] <- link
                            queue.Enqueue(succ)
            let mutable stop = true
            for sink in sinks do
                match pred.TryGetValue(sink) with
                | true, prec ->
                    stop <- false
                    // See how much more flow we can send
                    let mutable df = 1.0f<M^3/H> * System.Single.PositiveInfinity
                    Some prec
                    |> walkPred (fun link ->
                            let flow =
                                flow.TryGetValue((link.NodeA, link.NodeB))
                                |> Option.ofPair
                                |> Option.defaultValue 0.0f<M^3/H>
                            df <- min df (getFlowCapacity(link) - flow))
                    // Update flow by that amount
                    Some prec
                    |> walkPred (fun link ->
                        let link = link.NodeA, link.NodeB
                        let x =
                            flow.TryGetValue(link)
                            |> Option.ofPair
                            |> Option.defaultValue 0.0f<M^3/H>
                        flow.[link] <- x + df
                        let link = snd link, fst link
                        let x =
                            flow.TryGetValue(link)
                            |> Option.ofPair
                            |> Option.defaultValue 0.0f<M^3/H>
                        flow.[link] <- x - df)
                    ret <- ret + df
                | false, _ ->
                    ()
            if not stop then
                forEachAugmentationPath()
        forEachAugmentationPath()
        ret

    let terminalsInRegion network region =
        network.Data.Nodes
        |> Seq.filter (fun node ->
            node.Region = region && node.HasTerminal)
        |> Seq.map (fun node -> node.Id)
        |> Set.ofSeq

    /// Compute transport capacity between two adjacent regions
    let computeTransportCapacityBetweenRegions getFlowCapacity network =
        fun (regionA, regionB) ->
            let regions = Set [regionA; regionB]
            let regionA, regionB =
                min regionA regionB, max regionA regionB
            let sources = terminalsInRegion network regionA
            let sinks = terminalsInRegion network regionB
            computeTransportCapacity(getFlowCapacity, network, regions, sources, sinks)

    /// Compute transport capacity over multiple networks to all neighbours of a region
    let computeTransportCapacityToNeighbours getFlowCapacity networks neighboursOf filter =
        fun region ->
            let others =
                neighboursOf region
                |> Seq.filter (fun ngh -> filter region ngh)
                |> Set.ofSeq
            let regions = others.Add region
            networks
            |> List.sumBy (fun network ->
                let sources = terminalsInRegion network region
                let sinks =
                    others
                    |> Set.map (terminalsInRegion network)
                    |> Set.unionMany
                computeTransportCapacity(getFlowCapacity, network, regions, sources, sinks))

/// The overall status of the war.
type WarState(world, owners, buildingPartHealthLevel, airfieldPlanes, date, missionRecords) =

    let buildingPartHealthLevel = Seq.mutableDict buildingPartHealthLevel
    let owners = Seq.mutableDict owners
    let airfieldPlanes =
        airfieldPlanes
        |> Seq.map (fun (af, planes : (PlaneModel * float32) list) -> af, Seq.mutableDict planes)
        |> Seq.mutableDict
    let roads = world.Roads.QuickAccess
    let rails = world.Rails.QuickAccess
    // Must be cleared whenever owners change
    let regionDistancesToEnemy = Seq.mutableDict []
    // Must be cleared whenever bridges are damaged or repaired
    let roadsCapacities = Seq.mutableDict []
    let railsCapacities = Seq.mutableDict []
    // Must be cleared whenever bridges are damaged or repaired, or owners change
    // Capacity to enemy or neutral regions
    let invasionCapacity = Seq.mutableDict []
    // Capacity to friendly regions
    let transportCapacity = Seq.mutableDict []

    /// Method to be called after the owner of a region changes
    member this.ClearCachesAfterOwnerChanged() =
        owners.Clear()
        invasionCapacity.Clear()
        transportCapacity.Clear()

    /// Method to be called after the health of a bridge changes
    member this.ClearCachesAfterBridgeHealthChanged() =
        roadsCapacities.Clear()
        railsCapacities.Clear()
        invasionCapacity.Clear()
        transportCapacity.Clear()

    member this.World : World = world

    member this.Date : DateTime = date

    member this.MissionRecords : MissionRecord list = missionRecords

    /// Level of health of a subpart of a building or bridge
    member this.GetBuildingPartHealthLevel(bid, part) =
        match buildingPartHealthLevel.TryGetValue((bid, part)) with
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
            buildingPartHealthLevel.Remove((bid, part)) |> ignore
        else
            buildingPartHealthLevel.[(bid, part)] <- x
        if this.World.Bridges.ContainsKey(bid) then
            this.ClearCachesAfterBridgeHealthChanged()

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

    /// Compute a mapping from a coalition to distances to regions owned by this coalition
    member this.ComputeDistancesToCoalition =
        Cached.cached
            regionDistancesToEnemy
            (fun coalition ->
                let sources =
                    owners
                    |> Seq.choose (fun kvp ->
                        if kvp.Value = coalition then
                            Some kvp.Key
                        else None)
                    |> Set
                let distances =
                    world.Regions.Values
                    |> Seq.map (fun region -> region.RegionId, System.Int32.MaxValue)
                    |> Seq.mutableDict
                let nghOf =
                    world.Regions.Values
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
                coalition, distances
            )

    member this.ComputeRoadCapacity =
        Cached.cached
            roadsCapacities
            (Algo.computeTransportCapacityBetweenRegions this.GetFlowCapacity roads)

    member this.ComputeRailCapacity =
        Cached.cached
            railsCapacities
            (Algo.computeTransportCapacityBetweenRegions this.GetFlowCapacity rails)

    member this.ComputeInvasionCapacity =
        let neighboursOf regionId =
            this.World.Regions.[regionId].Neighbours
        let filter owner other =
            owner <> other
        Cached.cached
            invasionCapacity
            (Algo.computeTransportCapacityToNeighbours this.GetFlowCapacity [roads] neighboursOf filter)

    member this.ComputeTransportCapacity =
        let neighboursOf regionId =
            this.World.Regions.[regionId].Neighbours
        let filter owner other =
            owner = other
        Cached.cached
            invasionCapacity
            (Algo.computeTransportCapacityToNeighbours this.GetFlowCapacity [roads; rails] neighboursOf filter)

[<RequireQualifiedAccess>]
module Init =
    /// Create the initial status of the war
    let mkWar (world : World) =
        let regionOwners =
            world.Regions.Values
            |> Seq.choose (fun desc-> desc.InitialOwner |> Option.map (fun owner -> desc.RegionId, owner))
            |> List.ofSeq
        let airfields =
            world.Airfields
            |> Seq.map (fun af -> af.AirfieldId, [])
        WarState(world, regionOwners, [], airfields, world.StartDate, [])
