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

/// The state of a campaign
namespace Campaign.WarState

open System
open System.Numerics
open System.Collections.Generic
open FSharp.Json
open Util
open Util.Json

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel
open Campaign.Common.Targets
open Campaign.Common.Buildings
open Campaign.Common.Weather

open Campaign.NewWorldDescription
open SturmovikMission
open Campaign.Pilots

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
                        match network.GetNode(node).Region with
                        | Some region ->
                            regions.Contains(region)
                        | None ->
                            // Retain nodes outside regions. As there can be space between the borders of regions,
                            // there's a risk than a node in that space might cut the search, which we don't want
                            true)
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
            node.Region = Some region && node.HasTerminal)
        |> Seq.map (fun node -> node.Id)
        |> Set.ofSeq

    /// Compute transport capacity between two adjacent regions
    let computeTransportCapacityBetweenRegions getFlowCapacity network =
        fun (regionA : RegionId, regionB : RegionId) ->
            let regions = Set [regionA; regionB]
            let regionA, regionB =
                min regionA regionB, max regionA regionB
            let sources = terminalsInRegion network regionA
            let sinks = terminalsInRegion network regionB
            computeTransportCapacity(getFlowCapacity, network, regions, sources, sinks)

type WarStateSerialization =
    {
        FormatVersionMajor : int
        FormatVersionMinor : int
        Date : DateTime
        Weather : WeatherState
        BuildingPartHealthLevel : ((BuildingInstanceId * int) * float32) list
        Owners : Map<string, CoalitionId>
        GroundForces : ((CoalitionId * RegionId) * float32) list
        AirfieldPlanes : Map<string, Map<string, float32>>
        Players : Map<string, Player>
        Pilots : Pilot list
    }
with
    static member Default =
        {
            FormatVersionMajor = 2
            FormatVersionMinor = 0
            Date = DateTime(0L)
            Weather = WeatherState.Default
            BuildingPartHealthLevel = []
            Owners = Map.empty
            GroundForces = []
            AirfieldPlanes = Map.empty
            Players = Map.empty
            Pilots = []
        }

    member this.Version = sprintf "%d.%d" this.FormatVersionMajor this.FormatVersionMinor

/// The overall status of the war.
type WarState
    (
        world,
        owners : (RegionId * CoalitionId) seq,
        buildingPartHealthLevel : ((BuildingInstanceId * int) * float32) seq,
        airfieldPlanes : (AirfieldId * (PlaneModelId * float32) seq) seq,
        groundForces : ((CoalitionId * RegionId) * float32<MGF>) list,
        date : DateTime,
        weather : WeatherState,
        players : (string * Player) seq,
        pilots : (PilotId * Pilot) seq) =

    let mutable date = date
    let mutable weather = weather
    let buildingPartHealthLevel = Seq.mutableDict buildingPartHealthLevel
    let owners = Seq.mutableDict owners
    let airfieldPlanes =
        airfieldPlanes
        |> Seq.map (fun (af : AirfieldId, planes : (PlaneModelId * float32) seq) -> af, Seq.mutableDict planes)
        |> Seq.mutableDict
    let roads = world.Roads.GetQuickAccess()
    let rails = world.Rails.GetQuickAccess()
    let seaways = world.Seaways.GetQuickAccess()
    let rivers = world.Rivers.GetQuickAccess()

    // Must be cleared whenever owners change
    let regionDistancesToEnemy = Seq.mutableDict []
    // Must be cleared whenever bridges are damaged or repaired
    let roadsCapacities = Seq.mutableDict []
    let railsCapacities = Seq.mutableDict []
    // No need to clear
    let seaCapacities = Seq.mutableDict []
    let riverCapacities = Seq.mutableDict []
    // Must be cleared whenever bridges are damaged or repaired, or owners change
    let supplyAvailability = Seq.mutableDict []
    // Distances (in number of regions) of regions to regions with airfields, need never be cleared
    let mutable regionDistancesToAirfields = None

    // Ground forces
    let groundForces = Seq.mutableDict groundForces

    // Players and pilots
    let players = Seq.mutableDict players
    let pilots = Seq.mutableDict pilots

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

    /// Make a deep copy of this object
    member this.Clone() =
        use inMemory = new System.IO.MemoryStream()
        this.Serialize(inMemory)
        inMemory.Seek(0L, IO.SeekOrigin.Begin) |> ignore
        WarState.Deserialize(inMemory, this.World)

    member private this.PrepareIOData() =
        let asPairSeq x = x |> Seq.map (fun (kvp : KeyValuePair<_, _>) -> kvp.Key, kvp.Value)
        let toStringMap x = x |> Seq.map (fun (k, v) -> k.ToString(), v) |> Map.ofSeq
        let unmeasure = asPairSeq >> Seq.map (fun (k, v) -> k, float32 v) >> List.ofSeq
        let listify = asPairSeq >> List.ofSeq
        let airfieldPlanes =
            airfieldPlanes
            |> Seq.map (fun kvp ->
                string kvp.Key,
                kvp.Value
                |> asPairSeq
                |> toStringMap)
            |> Map.ofSeq
        let data =
            { WarStateSerialization.Default with
                Date = date
                Weather = weather
                BuildingPartHealthLevel = listify buildingPartHealthLevel
                Owners = owners |> asPairSeq |> Seq.map (fun (k, v) -> string k, v) |> Map.ofSeq
                GroundForces = groundForces |> unmeasure
                AirfieldPlanes = airfieldPlanes
                Players = players |> asPairSeq |> Map.ofSeq
                Pilots = pilots |> asPairSeq |> Seq.map snd |> List.ofSeq
            }
        data

    member this.Serialize(stream : System.IO.Stream) =
        let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
        let data = this.PrepareIOData()
        serializer.Serialize(stream, data, leaveOpen=true)

    member this.Serialize(writer : System.IO.TextWriter) =
        let data = this.PrepareIOData()
        let json = Json.serializeEx JsonConfig.IL2Default data
        writer.Write(json)

    static member private FromIOData(data : WarStateSerialization, world) =
        let expected = WarStateSerialization.Default
        if data.FormatVersionMajor <> expected.FormatVersionMajor then
            failwithf "Cannot load data with version %s, incompatible with %s" data.Version expected.Version
        let owners =
            data.Owners
            |> Map.toList
            |> List.map (fun (k, v) -> RegionId k, v)
        let airfieldPlanes =
            data.AirfieldPlanes
            |> Map.toSeq
            |> Seq.map (fun (k, v) -> AirfieldId k, v |> Map.toSeq |> Seq.map (fun (k, v) -> PlaneModelId k, v))
        let groundForces =
            data.GroundForces
            |> List.map (fun (k, v) -> k, v * 1.0f<MGF>)
        let players = data.Players |> Map.toSeq
        let pilots = data.Pilots |> List.map (fun pilot -> pilot.Id, pilot)
        WarState(world, owners, data.BuildingPartHealthLevel, airfieldPlanes, groundForces, data.Date, data.Weather, players, pilots)

    static member Deserialize(stream : System.IO.Stream, world) =
        let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
        let data = serializer.Deserialize<WarStateSerialization>(stream)
        WarState.FromIOData(data, world)

    static member Deserialize(reader : System.IO.TextReader, world) =
        let json = reader.ReadToEnd()
        let data = Json.deserializeEx JsonConfig.IL2Default json
        WarState.FromIOData(data, world)

    member this.World : World = world

    member this.Date : DateTime = date

    member this.SetDate(date2) = date <- date2

    member this.Weather = weather

    member this.SetWeather(weather2) = weather <- weather2

    member this.GetBuildingPartHealthLevel(bid, part) =
        match buildingPartHealthLevel.TryGetValue((bid, part)) with
        | true, x -> x
        | false, _ -> 1.0f

    member this.BuildingDamages =
        buildingPartHealthLevel
        |> Seq.map (fun kvp -> fst kvp.Key, snd kvp.Key, 1.0f - kvp.Value)

    member this.GetBuildingFullCapacity(bid) =
        match this.World.Buildings.TryGetValue(bid) with
        | true, building ->
            match this.World.BuildingProperties.TryGetValue(building.Script) with
            | true, properties ->
                (float32 properties.SubParts.Length) * properties.PartCapacity
            | false, _ ->
                0.0f<M^3>
        | false, _ ->
            0.0f<M^3>

    member this.GetBuildingCapacity(bid) =
        match this.World.Buildings.TryGetValue(bid) with
        | true, building ->
            match this.World.BuildingProperties.TryGetValue(building.Script) with
            | true, properties ->
                properties.SubParts
                |> List.sumBy (fun part -> this.GetBuildingPartFunctionalityLevel(bid, part))
                |> (*) properties.PartCapacity
            | false, _ ->
                0.0f<M^3>
        | false, _ ->
            0.0f<M^3>

    member this.GetBuildingPartFunctionalityLevel(bid, part) =
        let health = this.GetBuildingPartHealthLevel(bid, part)
        if health < 0.5f then
            0.0f
        else
            health

    member this.GetBuildingFunctionalityLevel(bid) =
        match this.World.Buildings.TryGetValue(bid) with
        | true, building ->
            let subParts =
                this.World.BuildingProperties.TryGetValue(building.Script)
                |> Option.ofPair
                |> Option.map (fun props -> props.SubParts)
                |> Option.defaultValue []
            match subParts.Length with
            | n when n > 0 ->
                subParts
                |> List.sumBy (fun part -> this.GetBuildingPartFunctionalityLevel(bid, part))
                |> (*) (1.0f / (float32 n))
            | _ ->
                1.0f
        | false, _ ->
            1.0f

    member this.SetBuildingPartHealthLevel(bid, part, x) =
        if x = 1.0f then
            buildingPartHealthLevel.Remove((bid, part)) |> ignore
        else
            buildingPartHealthLevel.[(bid, part)] <- x
        if this.World.Bridges.ContainsKey(bid) then
            this.ClearCachesAfterBridgeHealthChanged()

    member this.GetRegionProcessingLevel(rId : RegionId) =
        match this.GetRegionBuildingFullCapacity(rId) with
        | full when full > 0.0f<M^3> ->
            0.5f + 0.5f * this.GetRegionBuildingCapacity(rId) / full
        | _ ->
            1.0f

    member this.GetBridgeFunctionalityLevel(bid) =
        match this.World.Bridges.TryGetValue(bid) |> Option.ofPair with
        | Some building ->
            let subParts =
                this.World.BuildingProperties.TryGetValue(building.Script)
                |> Option.ofPair
                |> Option.map (fun props -> props.SubParts)
                |> Option.defaultValue []
            subParts
            |> List.fold (fun level part ->
                this.GetBuildingPartFunctionalityLevel(bid, part)
                |> min level
            ) 1.0f
        | None ->
            1.0f

    member this.GetGroundForces(coalition, region) =
        groundForces.TryGetValue((coalition, region))
        |> Option.ofPair
        |> Option.defaultValue 0.0f<MGF>

    member this.SetGroundForces(coalition, region, forces) =
        groundForces.[(coalition, region)] <- forces

    member this.GetFlowCapacity(link : NetworkLink) =
        let functionality =
            link.Bridges
            |> Seq.map (fun bid -> this.GetBridgeFunctionalityLevel(bid))
            |> Seq.fold min 1.0f
        functionality * link.FlowCapacity

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

    member this.ComputeRoadCapacity() =
        Cached.cached
            roadsCapacities
            (Algo.computeTransportCapacityBetweenRegions this.GetFlowCapacity roads)

    member this.ComputeRailCapacity() =
        Cached.cached
            railsCapacities
            (Algo.computeTransportCapacityBetweenRegions this.GetFlowCapacity rails)

    member this.ComputeSeaCapacity() =
        Cached.cached
            seaCapacities
            (Algo.computeTransportCapacityBetweenRegions this.GetFlowCapacity seaways)

    member this.ComputeRiverCapacity() =
        Cached.cached
            riverCapacities
            (Algo.computeTransportCapacityBetweenRegions this.GetFlowCapacity rivers)

    member this.ComputeSupplyAvailability() =
        Cached.cached
            supplyAvailability
            (fun rId ->
                match owners.TryGetValue rId with
                | false, _ -> 0.0f<E/H>
                | true, owner ->
                    if world.Regions.[rId].IsEntry then
                        world.ResupplyingOfCoalition.[owner] * this.GetRegionProcessingLevel(rId)
                    else
                    // Regions through which the owner coalition can travel: neutral, and the ones under one's control.
                    let regions =
                        world.Regions.Keys
                        |> Seq.filter (fun regId -> match this.GetOwner(regId) with Some coalition -> coalition = owner | None -> true)
                        |> Set
                    // Regions that can produce supplies: entry regions under the owner's control.
                    let sourceRegions =
                        world.Regions
                        |> Seq.filter (fun kvp -> kvp.Value.IsEntry && this.GetOwner(kvp.Key) = Some owner)
                        |> Seq.map (fun kvp -> kvp.Key)
                        |> Set
                    let computeFlow (network : NetworkQuickAccess) =
                        let sources = 
                            network.Data.Nodes
                            |> List.filter (fun node ->
                                match node.Region with
                                | Some region ->
                                    node.HasTerminal && sourceRegions.Contains region
                                | None ->
                                    false)
                            |> Seq.map (fun node -> node.Id)
                            |> Set
                        let sinks = Algo.terminalsInRegion network rId
                        // Note: Swap sources and sinks. For isolated regions, it typically takes
                        //       less time to go over all nodes in that region than to go over all
                        //       the nodes connected to the entry region.
                        let getFlowModifier =
                            Cached.cached
                                (Dictionary<RegionId, float32>())
                                this.GetRegionProcessingLevel
                        let getFlowModifier =
                            Option.map getFlowModifier
                            >> Option.defaultValue 1.0f
                        let getFlowCapacity(link : NetworkLink) =
                            let regionNodeA = network.GetNode(link.NodeA).Region
                            let regionNodeB = network.GetNode(link.NodeB).Region
                            this.GetFlowCapacity(link) * min (getFlowModifier regionNodeA) (getFlowModifier regionNodeB)
                        let flow = Algo.computeTransportCapacity(getFlowCapacity, network, regions, sinks, sources)
                        flow / world.ResourceVolume
                    let production =
                        owners
                        |> Seq.filter (fun kvp -> kvp.Value = owner && world.Regions.[kvp.Key].IsEntry)
                        |> Seq.sumBy (fun kvp -> this.GetRegionProcessingLevel(kvp.Key))
                        |> (*) (world.ResupplyingOfCoalition.[owner])
                    // This is not strictly correct: A production region that is cut from the network should not be able to contribute.
                    // To do things properly, the production should be added as sources in the graph
                    let limit = computeFlow rails + computeFlow roads + computeFlow rivers + computeFlow seaways
                    min limit production)

    member this.GetNumPlanes(afid) =
        airfieldPlanes.TryGetValue(afid)
        |> Option.ofPair
        |> Option.map (fun dict -> dict |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq)
        |> Option.defaultValue Map.empty

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

    member this.UpdatePlayer(player : Player) =
        players.[player.Guid] <- player

    member this.UpdatePilot(pilot : Pilot, updateFlights : bool) =
        if updateFlights then
            pilots.[pilot.Id] <- pilot
        else
            match pilots.TryGetValue(pilot.Id) with
            | true, oldPilot ->
                pilots.[pilot.Id] <- { pilot with Flights = oldPilot.Flights }
            | false, _ ->
                pilots.[pilot.Id] <- pilot

    member this.TryGetPlayer(guid : string) =
        players.TryGetValue(guid)
        |> Option.ofPair

    member this.Pilots =
        pilots.Values
        |> List.ofSeq

    member this.NumPilots =
        pilots.Count

    member this.GetPilot(id : PilotId) =
        pilots.[id]

    member this.RefreshPilotHealths() =
        for pilot in pilots.Values |> Array.ofSeq do
            match pilot.Health with
            | Injured recoveryDate when recoveryDate <= this.Date ->
                pilots.[pilot.Id] <- { pilot with Health = Healthy }
            | _ -> ()

    member this.TryFindPath(network : Network, starts : NetworkNode list, objectives : NetworkNode list, coalition : CoalitionId option) =
        let nodesToRemove =
            // Wrong coalition
            match coalition with
            | None -> Set.empty
            | Some coalition ->
                network.Nodes
                |> Seq.filter (fun node ->
                    match node.Region |> Option.bind this.GetOwner with
                    | None -> false
                    | Some c -> c <> coalition)
                |> Seq.map (fun node -> node.Id)
                |> Set.ofSeq
            |> fun rm ->
                // Damaged bridges
                network.Links
                |> Seq.filter (fun link ->
                    link.Bridges
                    |> List.exists (fun bid -> this.GetBridgeFunctionalityLevel(bid) < 0.5f))
                |> Seq.collect (fun link -> [link.NodeA; link.NodeB])
                |> Set.ofSeq
                |> Set.union rm
        let network = network.RemoveNodes nodesToRemove
        // Find path
        let starts =
            starts
            |> Seq.map (fun node -> node.Id)
            |> Seq.filter (nodesToRemove.Contains >> not)
            |> Set.ofSeq
        let objectives =
            objectives
            |> Seq.map (fun node -> node.Id)
            |> Seq.filter (nodesToRemove.Contains >> not)
            |> Set.ofSeq
        network.GetQuickAccess().FindPath(starts, objectives)

    member this.Seed =
        hash (this.Date, this.World.Scenario, this.Weather)

    member this.Players = players.Values |> List.ofSeq

    interface IWarState with
        member this.TryFindPath(network, sources, objectives, coalition) = this.TryFindPath(network, sources, objectives, coalition)
        member this.ComputeDistancesToAirfields() = upcast(this.ComputeDistancesToAirfields())
        member this.ComputeDistancesToCoalition(arg1) = upcast(this.ComputeDistancesToCoalition(arg1))
        member this.ComputeRailCapacity() = this.ComputeRailCapacity()
        member this.ComputeRoadCapacity() = this.ComputeRoadCapacity()
        member this.ComputeSeaCapacity() = this.ComputeSeaCapacity()
        member this.ComputeRiverCapacity() = this.ComputeRiverCapacity()
        member this.ComputeSupplyAvailability() = this.ComputeSupplyAvailability()
        member this.Date = this.Date
        member this.GetBridgeFunctionalityLevel(arg1) = this.GetBridgeFunctionalityLevel(arg1)
        member this.GetBuildingCapacity(arg1) = this.GetBuildingCapacity(arg1)
        member this.GetBuildingFullCapacity(arg1) = this.GetBuildingFullCapacity(arg1)
        member this.GetBuildingFunctionalityLevel(arg1) = this.GetBuildingFunctionalityLevel(arg1)
        member this.GetBuildingPartFunctionalityLevel(arg1, arg2) = this.GetBuildingPartFunctionalityLevel(arg1, arg2)
        member this.GetBuildingPartHealthLevel(arg1, arg2) = this.GetBuildingPartHealthLevel(arg1, arg2)
        member this.BuildingDamages = this.BuildingDamages
        member this.GetFlowCapacity(arg1) = this.GetFlowCapacity(arg1)
        member this.GetGroundForces(arg1, arg2) = this.GetGroundForces(arg1, arg2)
        member this.GetNumPlanes(arg1) = this.GetNumPlanes(arg1)
        member this.GetOwner(arg1) = this.GetOwner(arg1)
        member this.GetRegionProcessingLevel(rid) = this.GetRegionProcessingLevel(rid)
        member this.SetBuildingPartHealthLevel(arg1, arg2, arg3) = this.SetBuildingPartHealthLevel(arg1, arg2, arg3)
        member this.SetDate(arg1) = this.SetDate(arg1)
        member this.SetGroundForces(coalition, region, forces) = this.SetGroundForces(coalition, region, forces)
        member this.SetNumPlanes(arg1, arg2, arg3) = this.SetNumPlanes(arg1, arg2, arg3)
        member this.SetOwner(arg1, arg2) = this.SetOwner(arg1, arg2)
        member this.SetWeather(arg) = this.SetWeather(arg)
        member this.Weather = this.Weather
        member this.World = this.World
        member this.UpdatePlayer(player : Player) = this.UpdatePlayer(player)
        member this.UpdatePilot(pilot : Pilot, updateFlights : bool) = this.UpdatePilot(pilot, updateFlights)
        member this.TryGetPlayer(guid : string) = this.TryGetPlayer(guid)
        member this.GetPilot(id : PilotId) = this.GetPilot(id)
        member this.Pilots = this.Pilots
        member this.Players = this.Players
        member this.NumPilots = this.NumPilots
        member this.RefreshPilotHealths() = this.RefreshPilotHealths()
        member this.Seed = this.Seed
        member this.GetCoalitionBudget(_) = 5000.0f


[<RequireQualifiedAccess>]
module Init =
    /// Create the initial status of the war, without planes or ground forces.
    let mkWar (world : World, players : Player list, pilots : Pilot list) =
        let regionOwners =
            world.Regions.Values
            |> Seq.choose (fun desc-> desc.InitialOwner |> Option.map (fun owner -> desc.RegionId, owner))
            |> List.ofSeq
        let airfields =
            world.Airfields.Keys
            |> Seq.map (fun afid -> afid, Seq.empty)
        let weather = getWeather (System.Random(world.Seed)) (world.StartDate + System.TimeSpan.FromDays(world.WeatherDaysOffset))
        let players =
            players
            |> Seq.map (fun player -> player.Guid, player)
        let pilots =
            pilots
            |> Seq.map (fun pilot -> pilot.Id, pilot)
        let war =
            WarState(world, regionOwners, [], airfields, [], world.StartDate, weather, players, pilots)
        war

module IO =
    open System.IO

    type WarState with
        static member LoadFromFile(path : string, world) =
            use reader = new StreamReader(path)
            WarState.Deserialize(reader, world)

        member this.SaveToFile(path : string) =
            if File.Exists(path) then
                File.Delete(path)
            use writer = new StreamWriter(path)
            this.Serialize(writer)