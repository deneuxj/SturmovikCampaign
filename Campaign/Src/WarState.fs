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
        let queue = System.Collections.Generic.Queue()
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
        let rec startAugmentationPath() =
            pred.Clear()
            queue.Clear()
            // Run a breadth-first-search to find the shortest path from the sources to the sinks
            for s in sources do
                queue.Enqueue(s)
            bfs()
        and bfs() =
            if queue.Count > 0 then
                let node = queue.Dequeue()
                let successors, getLink = network.GetLink(node)
                // Constrain the path search inside the provided regions
                let successors =
                    successors
                    |> Seq.filter (fun node ->
                        let region = network.GetNode(node).Region
                        regions.Contains(region))
                    |> List.ofSeq
                handleSuccessors getLink node successors
        and handleSuccessors getLink node successors =
            match successors with
            | [] -> bfs()
            | succ :: successors ->
                let link = getLink succ
                let flow =
                    flow.TryGetValue((node, succ))
                    |> Option.ofPair
                    |> Option.defaultValue 0.0f<M^3/H>
                if not(sources.Contains(succ)) && getFlowCapacity(link) > flow then
                    if not (pred.ContainsKey succ) then
                        pred.[succ] <- link
                        queue.Enqueue(succ)
                if sinks.Contains succ && pred.ContainsKey succ then
                    updateFlows succ
                else
                    handleSuccessors getLink node successors
        and updateFlows sink =
            let prec = pred.[sink]
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
            startAugmentationPath()
        startAugmentationPath()
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
type WarState(world, owners, buildingPartHealthLevel, airfieldPlanes, groundForces : ((CoalitionId * RegionId) * float32<MGF>) seq, date) =

    let buildingPartHealthLevel = Seq.mutableDict buildingPartHealthLevel
    let owners = Seq.mutableDict owners
    let airfieldPlanes =
        airfieldPlanes
        |> Seq.map (fun (af, planes : (PlaneModelId * float32) list) -> af, Seq.mutableDict planes)
        |> Seq.mutableDict
    let roads = world.Roads.QuickAccess
    let rails = world.Rails.QuickAccess
    // Must be cleared whenever owners change
    let regionDistancesToEnemy = Seq.mutableDict []
    // Must be cleared whenever bridges are damaged or repaired
    let roadsCapacities = Seq.mutableDict []
    let railsCapacities = Seq.mutableDict []
    // Must be cleared whenever bridges are damaged or repaired, or owners change
    let supplyAvailability = Seq.mutableDict []
    // Distances (in number of regions) of regions to regions with airfields, need never be cleared
    let mutable regionDistancesToAirfields = None

    // Ground forces
    let groundForces = Seq.mutableDict groundForces

    let distancesToSources sources =
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
            if not(next.IsEmpty) then
                work next (dist + 1)
        work sources 1
        distances

    /// Method to be called after the owner of a region changes
    member private this.ClearCachesAfterOwnerChanged() =
        regionDistancesToEnemy.Clear()
        supplyAvailability.Clear()

    /// Method to be called after the health of a bridge changes
    member private this.ClearCachesAfterBridgeHealthChanged() =
        roadsCapacities.Clear()
        railsCapacities.Clear()
        supplyAvailability.Clear()

    member this.World : World = world

    member this.Date : DateTime = date

    /// Level of health of a subpart of a building or bridge
    member this.GetBuildingPartHealthLevel(bid, part) =
        match buildingPartHealthLevel.TryGetValue((bid, part)) with
        | true, x -> x
        | false, _ -> 1.0f

    /// Storage room in a building, taking health into account.
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

    /// Level of functionality of a building
    member this.GetBuildingFunctionalityLevel(bid) =
        assert(this.World.Buildings.ContainsKey(bid))
        let building = this.World.Buildings.[bid]
        building.Properties.SubParts
        |> List.sumBy (fun part -> this.GetBuildingPartFunctionalityLevel(bid, part))
        |> (*) (1.0f / (building.Properties.SubParts.Length |> max 1 |> float32))

    /// Set the level of health of a subpart of a building or a bridge
    member this.SetBuildingPartHealthLevel(bid, part, x) =
        if x = 1.0f then
            buildingPartHealthLevel.Remove((bid, part)) |> ignore
        else
            buildingPartHealthLevel.[(bid, part)] <- x
        if this.World.Bridges.ContainsKey(bid) then
            this.ClearCachesAfterBridgeHealthChanged()

    /// Storage room in a region, taking health into account.
    member this.GetRegionBuildingCapacity(rid : RegionId) =
        this.World.Regions.[rid].IndustryBuildings
        |> Seq.sumBy this.GetBuildingCapacity

    /// Level of functionality of a bridge
    member this.GetBridgeFunctionalityLevel(bid) =
        let building = this.World.Bridges.[bid]
        building.Properties.SubParts
        |> List.fold (fun level part ->
            this.GetBuildingPartFunctionalityLevel(bid, part)
            |> min level
        ) 1.0f

    /// Get the ground forces of a coalition in a region
    member this.GetGroundForces(coalition, region) =
        groundForces.TryGetValue((coalition, region))
        |> Option.ofPair
        |> Option.defaultValue 0.0f<MGF>

    /// Set the ground forces of a coalition in a region
    member this.SetGroundForces(coalition, region, forces) =
        groundForces.[(coalition, region)] <- forces

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
                distancesToSources sources)

    member this.ComputeDistancesToAirfields() =
        match regionDistancesToAirfields with
        | None ->
            let x =
                let sources =
                    world.Airfields.Values
                    |> Seq.map (fun af -> af.Region)
                    |> Set
                distancesToSources sources
            regionDistancesToAirfields <- Some x
            x
        | Some x ->
            x

    member this.ComputeRoadCapacity =
        Cached.cached
            roadsCapacities
            (Algo.computeTransportCapacityBetweenRegions this.GetFlowCapacity roads)

    member this.ComputeRailCapacity =
        Cached.cached
            railsCapacities
            (Algo.computeTransportCapacityBetweenRegions this.GetFlowCapacity rails)

    member this.ComputeSupplyAvailability =
        Cached.cached
            supplyAvailability
            (fun region ->
                match owners.TryGetValue region with
                | false, _ -> 0.0f<E/H>
                | true, owner ->
                    let regions =
                        owners
                        |> Seq.filter (fun kvp -> kvp.Value = owner)
                        |> Seq.map (fun kvp -> kvp.Key)
                        |> Set
                    let sourceRegions =
                        world.Regions
                        |> Seq.filter (fun kvp -> kvp.Value.IsEntry)
                        |> Seq.map (fun kvp -> kvp.Key)
                        |> Set
                    let computeFlow (network : NetworkQuickAccess) =
                        let sources = 
                            network.Data.Nodes
                            |> List.filter (fun node -> node.HasTerminal && sourceRegions.Contains node.Region)
                            |> Seq.map (fun node -> node.Id)
                            |> Set
                        let sinks = Algo.terminalsInRegion network region
                        let flow = Algo.computeTransportCapacity(this.GetFlowCapacity, network, regions, sources, sinks)
                        flow / world.ResourceVolume
                    let production =
                        owners
                        |> Seq.filter (fun kvp -> kvp.Value = owner && world.Regions.[kvp.Key].IsEntry)
                        |> Seq.sumBy (fun kvp -> this.GetRegionBuildingCapacity(kvp.Key) * world.ResourceProductionRate)
                    // This is not strictly correct: A production region that is cut from the network should not be able to contribute.
                    // To do things properly, the production should be added as sources in the graph
                    let limit = computeFlow rails + computeFlow roads
                    min limit production)

    member this.GetNumPlanes(afid) =
        airfieldPlanes.TryGetValue(afid)
        |> Option.ofPair
        |> Option.map (fun dict -> dict |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq)
        |> Option.defaultValue Map.empty

    member this.GetNumPlanes(afid, model) =
        airfieldPlanes.TryGetValue(afid)
        |> Option.ofPair
        |> Option.map (fun d -> d.TryGetValue(model) |> Option.ofPair |> Option.defaultValue 0.0f)
        |> Option.defaultValue 0.0f

    member this.SetNumPlanes(afid, model, qty) =
        let inner =
            airfieldPlanes.TryGetValue(afid)
            |> Option.ofPair
            |> Option.defaultValue (Seq.mutableDict [])
        inner.[model] <- qty

    member this.GetOwner(rid) =
        owners.TryGetValue(rid)
        |> Option.ofPair

    member this.SetOwner(rid, coalition) =
        match coalition with
        | None ->
            if owners.Remove(rid) then
                this.ClearCachesAfterOwnerChanged()
        | Some x ->
            let needClear = this.GetOwner(rid) <> coalition
            owners.[rid] <- x
            if needClear then
                this.ClearCachesAfterOwnerChanged()

    member this.GetAirfieldCapacity(afid) =
        this.World.Airfields.[afid].Facilities
        |> List.sumBy this.GetBuildingCapacity

[<RequireQualifiedAccess>]
module Init =
    /// Create the initial status of the war
    let mkWar (world : World) =
        let regionOwners =
            world.Regions.Values
            |> Seq.choose (fun desc-> desc.InitialOwner |> Option.map (fun owner -> desc.RegionId, owner))
            |> List.ofSeq
        let airfields =
            world.Airfields.Keys
            |> Seq.map (fun afid -> afid, [])
        let frontGroundForces =
            let regionOwners = dict regionOwners
            let getOwner rid =
                regionOwners.TryGetValue(rid)
                |> Option.ofPair
            world.Regions.Values
            |> Seq.choose (fun region ->
                getOwner region.RegionId
                |> Option.bind (fun owner ->
                    let isOnBorder = 
                        region.Neighbours
                        |> Seq.exists (fun ngh -> getOwner ngh <> Some owner)
                    if isOnBorder then
                        Some ((owner, region.RegionId), 0.0f<MGF>)
                    else
                        None))
        let war =
            WarState(world, regionOwners, [], airfields, frontGroundForces, world.StartDate)
        // Set forces on the frontline according to the region's storage capacity
        for (coalition, rid), _ in frontGroundForces do
            let capacity =
                world.Regions.[rid].IndustryBuildings
                |> Seq.sumBy war.GetBuildingCapacity
            let battleDuration = 10.0f<H>
            let optimalForces = capacity / (battleDuration * world.GroundForcesCost * world.ResourceVolume)
            war.SetGroundForces(coalition, rid, optimalForces)
        war
