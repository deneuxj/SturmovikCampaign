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

open Newtonsoft.Json

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription
open Campaign.NewWorldDescription
open Campaign.Weather
open SturmovikMission
open Util
open Campaign.Buildings
open Campaign.Pilots
open Campaign.Targets

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

type IWarStateQuery =
    /// Get the immutable description of the world
    abstract member World : World
    /// Get the current date and time
    abstract member Date : DateTime
    /// Get the weather
    abstract member Weather : WeatherState
    /// Level of health of a subpart of a building or bridge
    abstract member GetBuildingPartHealthLevel : BuildingInstanceId * int -> float32
    /// Get buildings and parts that have damages
    abstract member BuildingDamages : (BuildingInstanceId * int * float32) seq
    /// Storage room in a building, taking health into account.
    abstract member GetBuildingCapacity : BuildingInstanceId -> float32<M^3>
    /// Level of functionality of a subpart of a building or bridge
    abstract member GetBuildingPartFunctionalityLevel : BuildingInstanceId * int -> float32
    /// Level of functionality of a building
    abstract member GetBuildingFunctionalityLevel : BuildingInstanceId -> float32
    /// Storage room in a region, taking health into account.
    abstract member GetRegionBuildingCapacity : RegionId -> float32<M^3>
    /// Storage room in a region, assuming full health
    abstract member GetRegionBuildingFullCapacity : RegionId -> float32<M^3>
    /// Level of functionality of a bridge
    abstract member GetBridgeFunctionalityLevel : BuildingInstanceId -> float32
    /// Get the ground forces of a coalition in a region
    abstract member GetGroundForces : CoalitionId * RegionId -> float32<MGF>
    /// Get transport link capacity
    abstract member GetFlowCapacity : NetworkLink -> float32<M^3/H>
    /// Compute a mapping from a coalition to distances (in regions) to regions owned by this coalition
    abstract member ComputeDistancesToCoalition : CoalitionId -> IDictionary<RegionId, int>
    /// Compute a mapping from a regions to distances (in regions) to the closest region with an airfield
    abstract member ComputeDistancesToAirfields : unit -> IDictionary<RegionId, int>
    /// Compute a mapping from pairs of regions to road transport capacity between these two regions
    abstract member ComputeRoadCapacity : unit -> (RegionId * RegionId -> float32<M^3/H>)
    /// Compute a mapping from pairs of regions to rail transport capacity between these two regions
    abstract member ComputeRailCapacity : unit -> (RegionId * RegionId -> float32<M^3/H>)
    /// Compute a mapping from regions to amount of supplies that can reach it from the rear regions every hour
    abstract member ComputeSupplyAvailability : unit -> (RegionId -> float32<E/H>)
    /// Get a mapping denoting the number of each plane model at an airfield
    abstract member GetNumPlanes : AirfieldId -> Map<PlaneModelId, float32>
    /// Get the owner of a region
    abstract member GetOwner : RegionId -> CoalitionId option
    /// Try to get a player by its GUID
    abstract member TryGetPlayer : string -> Player option
    /// Get all pilots
    abstract member Pilots : Pilot list
    /// Get a pilot by its ID
    abstract member GetPilot : PilotId -> Pilot
    /// Get next available pilot ID
    abstract member NextPilotId : unit -> PilotId

[<AutoOpen>]
module IWarStateExtensions = 
    type IWarStateQuery with
        member this.GetNumPlanes(afId, model) =
            this.GetNumPlanes(afId)
            |> Map.tryFind model
            |> Option.defaultValue 0.0f

        member this.GetAirfieldCapacity(afid) =
            this.World.Airfields.[afid].Facilities
            |> List.sumBy this.GetBuildingCapacity

        member this.GetBuildingHealth(bid) =
            let building = this.World.GetBuildingInstance(bid)
            building.Properties.SubParts
            |> List.sumBy (fun part -> this.GetBuildingPartHealthLevel(bid, part))

        member this.IsPilotHealty(pilotId : PilotId) =
            match this.GetPilot(pilotId).Health with
            | Healthy -> true
            | Dead -> false
            | Injured(until) -> until < this.Date

        member this.AirfieldsOfCoalition(coalition : CoalitionId) =
            this.World.Airfields.Values
            |> Seq.filter(fun af -> this.GetOwner(af.Region) = Some coalition)

        member this.IsPilotAvailableFrom(pilotId : PilotId, afId : AirfieldId) =
            let pilot = this.GetPilot(pilotId)
            // Early exit if the owner of the airfield is not the pilot's coalition
            match this.GetOwner(this.World.Airfields.[afId].Region) with
            | None -> false
            | Some coalition when this.World.Countries.[pilot.Country] <> coalition -> false
            | _ ->
            match pilot.Flights |> List.tryLast with
            | None ->
                // No flights yet: pilot can start from any airfield
                true
            | Some { Return = AtAirfield afId2 } ->
                // Can start if airfield is same as landing of latest recorded mission
                afId2 = afId
            | Some { Return = CrashedInFriendlyTerritory pos } ->
                // Can start if crash-landed within 25km of airfield, or ...
                (this.World.Airfields.[afId].Position - pos).Length() < 25000.0f ||
                try
                    // ... airfield is the closest one to the crash site
                    this.AirfieldsOfCoalition(this.World.Countries.[pilot.Country])
                    |> Seq.minBy (fun af -> (af.Position - pos).LengthSquared())
                    |> fun af -> af.AirfieldId = afId
                with _ -> false
            | Some { Return = CrashedInEnemyTerritory } ->
                false

        member this.TryGetReturnAirfield(flight : FlightRecord, coalition) =
            match flight.Return with
            | CrashedInEnemyTerritory -> None
            | CrashedInFriendlyTerritory pos ->
                try
                    this.World.Airfields.Values
                    |> Seq.filter (fun af -> // Airfields controlled by the player's side
                        this.GetOwner(af.Region)
                        |> Option.map (fun coalition2 -> coalition2 = coalition)
                        |> Option.defaultValue false)
                    |> Seq.minBy (fun af -> (af.Position - pos).LengthSquared()) // Pick closest airfield
                    |> fun af -> af.AirfieldId
                    |> Some
                with _ -> None
            | AtAirfield afId ->
                Some afId

        member this.TryGetPilotHome(pilotId) =
            let pilot = this.GetPilot(pilotId)
            pilot.Flights
            |> List.tryLast
            |> Option.bind (fun flight -> this.TryGetReturnAirfield(flight, this.World.Countries.[pilot.Country]))

        member this.GetNewNames(country : CountryId, seed : int) : string * string =
            let random = System.Random(seed)
            let firstNames = this.World.Names.FirstNames.[country] |> Set.toArray
            let lastNames = this.World.Names.LastNames.[country] |> Set.toArray
            firstNames.[random.Next(firstNames.Length)], lastNames.[random.Next(lastNames.Length)]

        /// Return a new pilot, without registering it. The name is deterministically selected in a pseudo-random manner.
        /// The seed is computed from the date of the war state, the numeric id of the pilot, the scenario, the country and the player GUID.
        member this.NewPilot(guid : string, country : CountryId) : Pilot =
            let id = this.NextPilotId()
            let seed = hash(this.Date, id, this.World.Scenario, country, guid)
            let firstName, lastName = this.GetNewNames(country, seed)
            { Id = id
              Country = country
              PilotFirstName = firstName
              PilotLastName = lastName
              PlayerGuid = guid
              Health = Healthy
              Flights = []
              InitialAwards = []
              InitialAirKills = 0
              InitialNumFlights = 0
            }

        member this.GetPlayerPilots(guid : string) =
            this.Pilots
            |> Seq.filter (fun pilot -> pilot.PlayerGuid = guid)
            |> Seq.sortBy (fun pilot -> pilot.Id)


type IWarStateUpdate =
    /// Set the date and time
    abstract member SetDate : DateTime -> unit
    /// Set the weather
    abstract member SetWeather : WeatherState -> unit
    /// Set the level of health of a subpart of a building or a bridge
    abstract member SetBuildingPartHealthLevel : BuildingInstanceId * int * float32 -> unit
    /// Set the ground forces of a coalition in a region
    abstract member SetGroundForces : CoalitionId * RegionId * float32<MGF> -> unit
    /// Set the number of planes of a given model available at a given airfield
    abstract member SetNumPlanes : AirfieldId * PlaneModelId * float32 -> unit
    /// Set the owner of a region
    abstract member SetOwner : RegionId * CoalitionId option -> unit
    /// Add/update player
    abstract member UpdatePlayer : Player -> unit
    /// Add/update pilot
    abstract member UpdatePilot : Pilot -> unit

type IWarState =
    inherit IWarStateQuery
    inherit IWarStateUpdate

type private WarStateSerialization =
    {
        FormatVersionMajor : int
        FormatVersionMinor : int
        Date : DateTime
        Weather : WeatherState
        BuildingPartHealthLevel : IDictionary<BuildingInstanceId * int, float32>
        Owners : IDictionary<RegionId, CoalitionId>
        GroundForces : IDictionary<CoalitionId * RegionId, float32>
        AirfieldPlanes : IDictionary<AirfieldId, IDictionary<PlaneModelId, float32>>
        Players : IDictionary<string, Player>
        Pilots : IDictionary<PilotId, Pilot>
    }
with
    static member Default =
        {
            FormatVersionMajor = 1
            FormatVersionMinor = 1
            Date = DateTime(0L)
            Weather = WeatherState.Default
            BuildingPartHealthLevel = dict []
            Owners = dict []
            GroundForces = dict []
            AirfieldPlanes = dict []
            Players = dict []
            Pilots = dict []
        }

    member this.Version = sprintf "%d.%d" this.FormatVersionMajor this.FormatVersionMinor

/// The overall status of the war.
type WarState(world, owners, buildingPartHealthLevel, airfieldPlanes, groundForces, date, weather, players, pilots) =

    let mutable date = date
    let mutable weather = weather
    let buildingPartHealthLevel = Seq.mutableDict buildingPartHealthLevel
    let owners = Seq.mutableDict owners
    let airfieldPlanes =
        airfieldPlanes
        |> Seq.map (fun (af, planes : (PlaneModelId * float32) list) -> af, Seq.mutableDict planes)
        |> Seq.mutableDict
    let roads = world.Roads.GetQuickAccess()
    let rails = world.Rails.GetQuickAccess()
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
        let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer()
        serializer.Serialize(inMemory, this, leaveOpen=true)
        inMemory.Seek(0L, IO.SeekOrigin.Begin) |> ignore
        serializer.Deserialize<WarState>(inMemory)

    member this.Serialize(writer : System.IO.StreamWriter) =
        let unmeasure = Seq.map (fun (kvp : KeyValuePair<_, _>) -> kvp.Key, float32 kvp.Value) >> dict
        let data =
            {
                FormatVersionMajor = 1
                FormatVersionMinor = 0
                Date = date
                Weather = weather
                BuildingPartHealthLevel = buildingPartHealthLevel
                Owners = owners
                GroundForces = groundForces |> unmeasure
                AirfieldPlanes = airfieldPlanes |> Seq.map (fun kvp -> kvp.Key, kvp.Value :> IDictionary<_, _>) |> dict
                Players = players
                Pilots = pilots
            }
        let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
        serializer.Serialize(writer, data)

    static member Deserialize(reader : System.IO.StreamReader, world) =
        let serializer = MBrace.FsPickler.FsPickler.CreateXmlSerializer(indent = true)
        let data = serializer.Deserialize<WarStateSerialization>(reader)
        let expected = WarStateSerialization.Default
        if data.FormatVersionMajor <> expected.FormatVersionMajor then
            failwithf "Cannot load data with version %s, incompatible with %s" data.Version expected.Version
        let inline toList (x : IDictionary<'K, 'V>) =
            x
            |> Seq.map (fun kvp -> kvp.Key, kvp.Value)
            |> List.ofSeq
        let airfieldPlanes =
            data.AirfieldPlanes
            |> toList
            |> List.map (fun (k, v) -> k, toList v)
        let groundForces =
            data.GroundForces
            |> toList
            |> List.map (fun (k, v) -> k, v * 1.0f<MGF>)
        let players = toList data.Players
        let pilots = toList data.Pilots
        WarState(world, toList data.Owners, toList data.BuildingPartHealthLevel, airfieldPlanes, groundForces, data.Date, data.Weather, players, pilots)

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
        |> Seq.map (fun kvp -> fst kvp.Key, snd kvp.Key, kvp.Value)

    member this.GetBuildingFullCapacity(bid) =
        assert(this.World.Buildings.ContainsKey(bid))
        let building = this.World.Buildings.[bid]
        (float32 building.Properties.SubParts.Length) * building.Properties.PartCapacity

    member this.GetBuildingCapacity(bid) =
        assert(this.World.Buildings.ContainsKey(bid))
        let building = this.World.Buildings.[bid]
        building.Properties.SubParts
        |> List.sumBy (fun part -> this.GetBuildingPartFunctionalityLevel(bid, part))
        |> (*) building.Properties.PartCapacity

    member this.GetBuildingPartFunctionalityLevel(bid, part) =
        let health = this.GetBuildingPartHealthLevel(bid, part)
        if health < 0.5f then
            0.0f
        else
            health

    member this.GetBuildingFunctionalityLevel(bid) =
        assert(this.World.Buildings.ContainsKey(bid))
        let building = this.World.Buildings.[bid]
        building.Properties.SubParts
        |> List.sumBy (fun part -> this.GetBuildingPartFunctionalityLevel(bid, part))
        |> (*) (1.0f / (building.Properties.SubParts.Length |> max 1 |> float32))

    member this.SetBuildingPartHealthLevel(bid, part, x) =
        if x = 1.0f then
            buildingPartHealthLevel.Remove((bid, part)) |> ignore
        else
            buildingPartHealthLevel.[(bid, part)] <- x
        if this.World.Bridges.ContainsKey(bid) then
            this.ClearCachesAfterBridgeHealthChanged()

    member this.GetRegionBuildingCapacity(rid : RegionId) =
        this.World.Regions.[rid].IndustryBuildings
        |> Seq.sumBy this.GetBuildingCapacity

    member this.GetRegionBuildingFullCapacity(rid : RegionId) =
        this.World.Regions.[rid].IndustryBuildings
        |> Seq.sumBy this.GetBuildingFullCapacity

    member this.GetBridgeFunctionalityLevel(bid) =
        let building = this.World.Bridges.[bid]
        building.Properties.SubParts
        |> List.fold (fun level part ->
            this.GetBuildingPartFunctionalityLevel(bid, part)
            |> min level
        ) 1.0f

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

    member this.ComputeSupplyAvailability() =
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

    member this.UpdatePilot(pilot : Pilot) =
        pilots.[pilot.Id] <- pilot

    member this.TryGetPlayer(guid : string) =
        players.TryGetValue(guid)
        |> Option.ofPair

    member this.Pilots =
        pilots.Values
        |> List.ofSeq

    member this.GetPilot(id : PilotId) =
        pilots.[id]

    member this.NextPilotId() = PilotId(pilots.Count)

    interface IWarState with
        member this.ComputeDistancesToAirfields() = upcast(this.ComputeDistancesToAirfields())
        member this.ComputeDistancesToCoalition(arg1) = upcast(this.ComputeDistancesToCoalition(arg1))
        member this.ComputeRailCapacity() = this.ComputeRailCapacity()
        member this.ComputeRoadCapacity() = this.ComputeRoadCapacity()
        member this.ComputeSupplyAvailability() = this.ComputeSupplyAvailability()
        member this.Date = this.Date
        member this.GetBridgeFunctionalityLevel(arg1) = this.GetBridgeFunctionalityLevel(arg1)
        member this.GetBuildingCapacity(arg1) = this.GetBuildingCapacity(arg1)
        member this.GetBuildingFunctionalityLevel(arg1) = this.GetBuildingFunctionalityLevel(arg1)
        member this.GetBuildingPartFunctionalityLevel(arg1, arg2) = this.GetBuildingPartFunctionalityLevel(arg1, arg2)
        member this.GetBuildingPartHealthLevel(arg1, arg2) = this.GetBuildingPartHealthLevel(arg1, arg2)
        member this.BuildingDamages = this.BuildingDamages
        member this.GetFlowCapacity(arg1) = this.GetFlowCapacity(arg1)
        member this.GetGroundForces(arg1, arg2) = this.GetGroundForces(arg1, arg2)
        member this.GetNumPlanes(arg1) = this.GetNumPlanes(arg1)
        member this.GetOwner(arg1) = this.GetOwner(arg1)
        member this.GetRegionBuildingCapacity(arg1) = this.GetRegionBuildingCapacity(arg1)
        member this.GetRegionBuildingFullCapacity(arg1) = this.GetRegionBuildingFullCapacity(arg1)
        member this.SetBuildingPartHealthLevel(arg1, arg2, arg3) = this.SetBuildingPartHealthLevel(arg1, arg2, arg3)
        member this.SetDate(arg1) = this.SetDate(arg1)
        member this.SetGroundForces(coalition, region, forces) = this.SetGroundForces(coalition, region, forces)
        member this.SetNumPlanes(arg1, arg2, arg3) = this.SetNumPlanes(arg1, arg2, arg3)
        member this.SetOwner(arg1, arg2) = this.SetOwner(arg1, arg2)
        member this.SetWeather(arg) = this.SetWeather(arg)
        member this.Weather = this.Weather
        member this.World = this.World
        member this.UpdatePlayer(player : Player) = this.UpdatePlayer(player)
        member this.UpdatePilot(pilot : Pilot) = this.UpdatePilot(pilot)
        member this.TryGetPlayer(guid : string) = this.TryGetPlayer(guid)
        member this.GetPilot(id : PilotId) = this.GetPilot(id)
        member this.NextPilotId() = this.NextPilotId()
        member this.Pilots = this.Pilots


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
            |> List.ofSeq
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
            |> List.ofSeq
        let weather = getWeather (System.Random()) world.StartDate
        let war =
            WarState(world, regionOwners, [], airfields, frontGroundForces, world.StartDate, weather, [], [])
        // Set forces on the frontline according to the region's storage capacity
        for (coalition, rid), _ in frontGroundForces do
            let capacity =
                world.Regions.[rid].IndustryBuildings
                |> Seq.sumBy war.GetBuildingCapacity
            let battleDuration = 10.0f<H>
            let optimalForces = capacity / (battleDuration * world.GroundForcesCost * world.ResourceVolume)
            war.SetGroundForces(coalition, rid, optimalForces)
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