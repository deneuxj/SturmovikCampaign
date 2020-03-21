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

module Campaign.NewWorldDescription

open System.Numerics
open VectorExtension
open System.Collections.Generic

open SturmovikMission.Blocks
open Util

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription

type BuildingProperties = {
    Model : string
    Script : string
    Boundary : Vector2 list
    SubParts : int list
    Durability : int
}

module private BuildingProperties_ =
    let Areas = Util.cachedProperty (fun this -> 1.0f<M^2> * Vector2.ConvexPolygonArea this.Boundary)

type BuildingProperties with
    /// Volume of storage per surface unit
    static member CapacityDensity = 1.0f<M^3/M^2>

    member this.Area = BuildingProperties_.Areas this

    /// Total capacity of all parts in the building
    member this.Capacity =
        BuildingProperties.CapacityDensity * this.Area

    /// Volume of storage in a single part
    member this.PartCapacity =
        match this.SubParts.Length with
        | 0 -> 0.0f<M^3>
        | n -> this.Capacity / float32 n

/// Identify buildings by their position.
[<Struct>]
type BuildingInstanceId = BuildingInstanceId of OrientedPosition

type BuildingInstance = {
    Pos : OrientedPosition
    Properties : BuildingProperties
}
with
    member this.Id = BuildingInstanceId this.Pos

type Region = {
    RegionId : RegionId
    Boundary : Vector2 list
    Position : Vector2
    Neighbours : RegionId list
    InitialOwner : CoalitionId option
    /// Whether supplies arrive/are produced in this region. Also called "rear region" if true.
    IsEntry : bool
    IndustryBuildings : BuildingInstanceId list
}

/// A node in the logistics network
type NetworkNode = {
    Id : int
    Pos : Vector2
    Region : RegionId
    HasTerminal : bool
}

/// An undirected edge between two nodes
type NetworkLink = {
    NodeA : int
    NodeB : int
    Bridges : BuildingInstanceId list
    FlowCapacity : float32<M^3/H>
}

type Network = {
    Nodes : NetworkNode list
    Links : NetworkLink list
}
with
    /// Set the region of each node, and remove nodes that are not in a region
    member this.SetRegions(regions : Region list) =
        // Set region, drop nodes outside all regions
        let nodes =
            this.Nodes
            |> List.choose (fun node ->
                let region =
                    regions
                    |> List.tryFind (fun region -> node.Pos.IsInConvexPolygon(region.Boundary))
                match region with
                | Some region ->
                    Some { node with Region = region.RegionId }
                | None -> None
            )
        // Remove links to nodes that were dropped
        let retained =
            nodes
            |> Seq.map (fun node -> node.Id)
            |> Set.ofSeq
        let links =
            this.Links
            |> List.filter (fun link -> retained.Contains(link.NodeA) && retained.Contains(link.NodeB))
        // Result
        { this with Nodes = nodes; Links = links }

    /// Set the bridges located on each link
    member this.SetBridges(bridges : BuildingInstance list) =
        // Find bridges by their exact position
        let byPos =
            bridges
            |> Seq.map (fun instance -> instance.Pos, instance)
            |> dict
        // Get the boundary of each bridge, and cache values
        let getBoundary pos =
            byPos.[pos].Properties.Boundary
            |> List.map (fun v -> v.Rotate(pos.Rotation) + pos.Pos)
        let cache = System.Collections.Generic.Dictionary()
        let cachedGetBoundary = SturmovikMission.Cached.cached cache getBoundary
        // Quick intersection tests for bridges
        let tree = Campaign.SpacePartition.QuadTree.fromBoundaryOjects cachedGetBoundary 10 10 (bridges |> Seq.map (fun instance -> instance.Pos))
        // Area between nodes to "capture" bridges
        let roadSegment (v1 : Vector2, v2 : Vector2) : Vector2 list =
            let dir = v2 - v1
            let len = dir.Length()
            let dir = dir / len
            let side = 3.0f * dir.Rotate(90.0f)
            [
                v1 - side
                v2 - side
                v2 + side
                v1 + side
            ]
        let finder = Campaign.SpacePartition.QuadTreeItemFinder.create cachedGetBoundary roadSegment tree
        // Quick node lookup
        let nodeById =
            this.Nodes
            |> Seq.map (fun node -> node.Id, node)
            |> dict
        // Find bridges between nodes of each link
        let links =
            this.Links
            |> List.map (fun link ->
                let v1 = nodeById.[link.NodeA].Pos
                let v2 = nodeById.[link.NodeB].Pos
                let bridges =
                    finder.FindIntersectingItems (v1, v2)
                    |> Seq.distinct
                    |> Seq.map BuildingInstanceId
                    |> List.ofSeq
                { link with Bridges = bridges }
            )
        { this with
            Links = links }

    /// Set the nodes that are in terminal areas
    member this.SetTerminals(areas : Vector2 list list) =
        let nodes =
            this.Nodes
            |> List.map (fun node ->
                let hasTerminal =
                    areas
                    |> List.exists(fun area -> node.Pos.IsInConvexPolygon area)
                { node with HasTerminal = hasTerminal }
            )
        { this with
            Nodes = nodes
        }

/// A network with functions to quickly access nodes and links
type NetworkQuickAccess =
    {
        Data : Network
        GetNode : int -> NetworkNode
        GetLink : int -> int seq * (int -> NetworkLink)
    }

type Network with
    member this.QuickAccess =
        let nodes =
            this.Nodes
            |> Seq.map (fun node -> node.Id, node)
            |> dict
        let links =
            this.Links
            |> Seq.map (fun link -> { link with NodeA = link.NodeB; NodeB = link.NodeA })
            |> Seq.append this.Links
            |> Seq.groupBy (fun link -> link.NodeA)
            |> Seq.map (fun (nodeA, links) ->
                nodeA,
                links
                |> Seq.map (fun link -> link.NodeB, link)
                |> dict)
            |> dict
        {
            Data = this
            GetNode = fun x -> nodes.[x]
            GetLink = fun x ->
                let dict = links.[x]
                upcast dict.Keys, fun y -> dict.[y]
        }

type Runway = {
    SpawnPos : OrientedPosition
    PathToRunway : Vector2 list
    Start : Vector2
    End : Vector2
}

type Airfield = {
    AirfieldId : AirfieldId
    Position : Vector2
    Region : RegionId
    Boundary : Vector2 list
    Runways : Runway list
    Facilities : BuildingInstanceId list
}
with
    // Airfields without runways can be used by players for emergency landings, but are otherwise unused.
    member this.IsActive = not this.Runways.IsEmpty

type World = {
    /// Base name of scenario file
    Scenario : string
    /// Name of the map where the scenario takes place
    Map : string
    /// Date of the first mission.
    StartDate : System.DateTime
    /// Weather offset: affects how late or early the weather pattern is.
    WeatherDaysOffset : float
    /// Resources that can be spent per region or airfield on repairing buildings
    RepairSpeed: float32<E/H>
    /// Amount of resources to repair 1 unit of storage capacity
    RepairCostRatio : float32<E/M^3>
    /// Amount of resources needed to repair 1 unit of transport capacity
    TransportRepairCostRatio : float32<E/(M^3/H)>
    /// Volume of resources
    ResourceVolume : float32<M^3/E>
    /// Resources that can be produced per hour, per storage volume
    ResourceProductionRate : float32<E/H/M^3>
    /// Amount of resources for a unit of ground force to work optimally, per hour
    GroundForcesCost : float32<E/MGF/H>
    /// Transport capacity required per unit of ground force
    GroundForcesTransportCost : float32<M^3/H/MGF>
    /// Descriptions of regions
    Regions : IDictionary<RegionId, Region>
    /// The road network
    Roads : Network
    /// The rail network
    Rails : Network
    /// Descriptions of all airfields
    Airfields : IDictionary<AirfieldId, Airfield>
    /// Mapping from building instance identifiers to building instances
    Buildings : IDictionary<BuildingInstanceId, BuildingInstance>
    /// Mapping from bridge instance identifiers to bridge instances
    Bridges : IDictionary<BuildingInstanceId, BuildingInstance>
    /// Mapping from plane model identifiers to plane model descriptions
    PlaneSet : IDictionary<PlaneModelId, PlaneModel>
}
with
    /// Get building or bridge instance by its ID
    member this.GetBuildingInstance(bid : BuildingInstanceId) =
        [this.Buildings; this.Bridges]
        |> Seq.pick (fun d -> d.TryGetValue(bid) |> Option.ofPair)

module Init =
    open System.IO
    open System.Reflection
    open SturmovikMission.DataProvider.Parsing
    open FSharp.Data

    open SturmovikMission.Blocks.BlocksMissionData
    open SturmovikMission.Blocks.BlocksMissionData.CommonMethods

    [<Literal>]
    let private sampleFile = __SOURCE_DIRECTORY__ + @"\..\Config\Roads-Sample.json"
    type private JsonNetwork = JsonProvider<sampleFile>

    /// Extract BuildingProperties from a block inside a delimiting influence area
    let inline extractBuildingProperties(building : ^Block, boundary : T.MCU_TR_InfluenceArea) =
        let pos = Vector2.FromPos(building)
        let rot = float32(building |> getYOri |> valueOf)
        let vertices =
            boundary.GetBoundary().Value
            |> Seq.map (fun floats ->
                (Vector2.FromPair floats - pos).Rotate(-rot))
            |> List.ofSeq
        let subparts =
            building
            |> getDamaged
            |> valueOf
            |> Map.toSeq
            |> Seq.map fst
            |> List.ofSeq
        let durability = building |> getDurability |> valueOf
        {
            Model = building |> getModel |> valueOf
            Script = building |> getScript |> valueOf
            Boundary = vertices
            SubParts = subparts
            Durability = durability
        }

    /// Load a list of BuildingProperties from a .Mission file
    let loadBuildingPropertiesList(path : string) =
        let data =
            try
                T.GroupData.Parse(Stream.FromFile path)
            with
            | :? ParseError as e ->
                printParseError e
                |> String.concat "\n"
                |> failwithf "%s"
        let blocks = data.ListOfBlock
        let bridges = data.ListOfBridge
        let zones = data.ListOfMCU_TR_InfluenceArea
        let zones =
            zones
            |> Seq.map (fun area ->
                let vertices =
                    area.GetBoundary().Value
                    |> Seq.map (fun floats ->
                        let x, y = floats.Value
                        Vector2(float32 x, float32 y))
                    |> List.ofSeq
                (fun (v : Vector2) -> v.IsInConvexPolygon(vertices)), area)
            |> List.ofSeq
        let inline build blocks =
            [
                for block in blocks do
                    let pos = Vector2.FromPos block
                    match zones |> List.tryFind (fun (f, _) -> f pos) with
                    | Some (_, data) ->
                        yield extractBuildingProperties(block, data)
                    | None ->   ()
            ]
        [ build blocks ; build bridges ]
        |> List.concat

    /// Extract a list of building instances from a list of blocks from a .Mission file using a database of known building types 
    let inline extractBuildingInstances(db : BuildingProperties list, blocks : ^Block list) =
        let db =
            db
            |> Seq.map (fun building -> building.Script, building)
            |> dict
        [
            for block in blocks do
                match db.TryGetValue(block |> getScript |> valueOf) with
                | true, props ->
                    yield { Pos =
                                { Pos = Vector2.FromPos block
                                  Rotation = block |> getYOri |> valueOf |> float32
                                  Altitude = block |> getAlt |> valueOf |> float32 }
                            Properties = props }
                | false, _ ->
                    ()
        ]

    /// Extract a list of regions from a list of influence areas and a list of building instances
    let extractRegions(regions : T.MCU_TR_InfluenceArea list, buildings : BuildingInstance list) =
        let extractOne (region : T.MCU_TR_InfluenceArea) : Region =
            let coalition = CoalitionId.FromCountry (enum(region.GetCountry().Value))
            let boundary = region.GetBoundary().Value |> Seq.map(fun coord -> Vector2.FromPair(coord)) |> List.ofSeq
            let buildings =
                buildings
                |> List.filter (fun building -> building.Pos.Pos.IsInConvexPolygon boundary)
            { RegionId = RegionId(region.GetName().Value)
              Position = Vector2.FromPos(region)
              Boundary = boundary
              Neighbours = []
              IsEntry = region.GetDesc().Value.Contains("***")
              InitialOwner = coalition
              IndustryBuildings = buildings |> List.map (fun b -> b.Id)
            }
        let withBoundaries =
            regions
            |> List.map extractOne
        let cellRadius = 1000.0f
        let cellRadius2 = cellRadius * cellRadius
        let floor1 x : float32 =
            let un1t = 2.0f * cellRadius
            floor(x / un1t) * un1t
        let ceil1 x : float32 =
            let un1t = 2.0f * cellRadius
            ceil(x / un1t) * un1t
        let nearestCenters (v : Vector2) =
            let x0 = floor1 v.X
            let x1 = ceil1 v.X
            let y0 = floor1 v.Y
            let y1 = ceil1 v.Y
            [
                (x0, y0)
                (x0, y1)
                (x1, y0)
                (x1, y1)
            ]
        let located =
            withBoundaries
            |> List.map (fun region ->
                region.Boundary
                |> List.mapi(fun i v ->
                    nearestCenters v
                    |> List.map (fun center -> center, (region.RegionId, i, v))
                )
            )
            |> List.concat
            |> List.concat
            |> Seq.groupBy fst
            |> Seq.map (fun (k, items) -> k, items |> Seq.map snd |> List.ofSeq)
            |> dict
        let neighbours =
            located
            |> Seq.map (fun kvp ->
                [
                    for region1, i1, v1 in kvp.Value do
                        for region2, i2, v2 in kvp.Value do
                            if region1 <> region2 && (v1 - v2).LengthSquared() < cellRadius2 then
                                yield (region1, i1), region2
                ]
            )
            |> List.concat
            |> Seq.groupBy fst
            |> Seq.map (fun (key, items) -> key, items |> Seq.map snd |> Set.ofSeq)
            |> dict
        let getNeighbours(regionId, i) =
            match neighbours.TryGetValue((regionId, i)) with
            | true, items -> items
            | false, _ -> Set.empty
        let setNeighbours (region : Region) =
            let indices = (region.Boundary |> List.mapi(fun i _ -> i)) @ [0]
            let ngh =
                indices
                |> Seq.pairwise
                |> Seq.map (fun (i, j) ->
                    let s1 = getNeighbours(region.RegionId, i)
                    let s2 = getNeighbours(region.RegionId, j)
                    Set.intersect s1 s2
                    |> List.ofSeq
                )
                |> List.concat
                |> Seq.distinct
                |> List.ofSeq
            { region with Neighbours = ngh }
        withBoundaries
        |> List.map setNeighbours

    /// Load a road network from a JSON file. Only the coordinates and the graph information is set.
    let loadRoadGraph (path : string, flowCapacity) =
        let data = JsonNetwork.Load(path)
        let nodesAndLinks =
            [
                for node in data.Nodes do
                    yield Choice1Of2
                        {
                            Id = node.Id
                            Pos = Vector2(float32 node.Pos.[0], float32 node.Pos.[1])
                            Region = RegionId ""
                            HasTerminal = false
                        }
                    for ngh in node.Neighbours do
                        if node.Id < ngh then
                            yield Choice2Of2
                                {
                                    NodeA = node.Id
                                    NodeB = ngh
                                    Bridges = []
                                    FlowCapacity = flowCapacity
                                }
            ]
        {
            Nodes =
                nodesAndLinks
                |> List.choose (function Choice1Of2 x -> Some x | _ -> None)
            Links =
                nodesAndLinks
                |> List.choose (function Choice2Of2 x -> Some x | _ -> None)
        }

    /// Extract runway information from a plane spawn
    let extractRunway(spawn : T.Airfield) =
        match spawn.TryGetChart() with
        | Some chart ->
            let pos = OrientedPosition.FromMission spawn
            let taxi = chart.GetPoints()
            let vecs =
                taxi
                |> Seq.map (fun point -> Vector2(float32(point.GetX().Value), float32(point.GetY().Value)), point.GetType().Value)
                |> Seq.map (fun (v, t) -> v.Rotate(pos.Rotation) + pos.Pos, t)
                |> List.ofSeq
            let path =
                vecs
                |> List.takeWhile (fun (_, t) -> t < 2)
                |> List.map fst
            let p1, _ =
                vecs
                |> List.find (fun (_, t) -> t = 2)
            let p2, _ =
                vecs
                |> List.findBack (fun (_, t) -> t = 2)
            {
                SpawnPos = pos
                PathToRunway = path
                Start = p1
                End = p2
            }
        | None ->
            failwithf "Airfield spawn '%s' lacks a chart" (spawn.GetName().Value)

    /// Extract airfields and their runways from spawns, airfield areas and regions
    let extractAirfields(spawns : T.Airfield list, areas : T.MCU_TR_InfluenceArea list, regions : Region list, getBuilding : BuildingInstanceId -> BuildingInstance) =
        [
            for area in areas do
                let boundary =
                    area.GetBoundary().Value
                    |> Seq.map Vector2.FromPair
                    |> List.ofSeq
                let runways =
                    [
                        for spawn in spawns do
                            let pos = Vector2.FromPos spawn
                            if pos.IsInConvexPolygon boundary then
                                yield extractRunway spawn
                    ]
                let pos = Vector2.FromPos area
                match regions |> List.tryFind (fun region -> pos.IsInConvexPolygon region.Boundary) with
                | Some region ->
                    let buildings =
                        region.IndustryBuildings
                        |> List.filter (fun building -> (getBuilding building).Pos.Pos.IsInConvexPolygon boundary)
                    yield {
                        AirfieldId = AirfieldId (area.GetName().Value)
                        Position = pos
                        Region = region.RegionId
                        Boundary = boundary
                        Runways = runways
                        Facilities = buildings
                    }
                | None ->
                    ()
        ]

    /// Remove buildings that are assigned to airfields from regions
    let cleanRegionBuildings(regions : Region list, airfields : Airfield list) =
        let buildingsInAirfields =
            airfields
            |> Seq.fold (fun buildings airfield ->
                airfield.Facilities
                |> Set.ofSeq
                |> Set.union buildings
            ) Set.empty
        let filter (building : BuildingInstanceId) =
            not <| Set.contains building buildingsInAirfields
        regions
        |> List.map (fun region ->
            let buildings =
                region.IndustryBuildings
                |> List.filter filter
            { region with
                IndustryBuildings = buildings
            })

    /// Load a scenario mission file and create a world description.
    let mkWorld(scenario : string, roadsCapacity : float32<M^3/H>, railsCapacity : float32<M^3/H>) =
        let exeDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let buildingDb = loadBuildingPropertiesList (Path.Combine(exeDir, "Buildings.Mission"))
        let missionData =
            try
                T.GroupData.Parse(Stream.FromFile scenario)
            with
            | :? ParseError as err ->
                printParseError err
                |> String.concat "\n"
                |> eprintfn "%s"
                failwithf "Failed to parse scenario '%s'" scenario
        // Region boundaries
        let regionAreas =
            missionData.GetGroup("Regions").ListOfMCU_TR_InfluenceArea
            |> List.ofSeq
        let regionBoundaries =
            regionAreas
            |> List.map (fun area ->
                area.GetBoundary().Value
                |> Seq.map Vector2.FromPair
                |> List.ofSeq)
        // Blocks inside regions
        let blocks =
            missionData.GetGroup("Static").ListOfBlock
            |> Seq.filter (fun block ->
                let pos = Vector2.FromPos block
                regionBoundaries
                |> Seq.exists (fun boundary -> pos.IsInConvexPolygon boundary))
            |> List.ofSeq
        // Building instances
        let buildings = extractBuildingInstances(buildingDb, blocks)
        let buildingsDict =
            buildings
            |> Seq.map (fun b -> b.Id, b)
            |> dict
        // Regions
        let regions = extractRegions(regionAreas, buildings)
        // Airfields
        let airfieldAreas = missionData.GetGroup("Airfields").ListOfMCU_TR_InfluenceArea |> List.ofSeq
        let airfieldSpawns = missionData.GetGroup("Airfields").ListOfAirfield |> List.ofSeq
        let airfields = extractAirfields(airfieldSpawns, airfieldAreas, regions, fun bid -> buildingsDict.[bid])
        let regions = cleanRegionBuildings(regions, airfields)
        // Map name
        let options = Seq.head missionData.ListOfOptions
        let mapName = options.GetGuiMap().Value
        // Terminal areas
        let terminals =
            missionData.GetGroup("Terminals").ListOfMCU_TR_InfluenceArea
            |> Seq.map (fun area ->
                area.GetBoundary().Value
                |> Seq.map Vector2.FromPair
                |> List.ofSeq)
            |> List.ofSeq
        // Function to load roads and bridges
        let loadRoads group graph capacity =
            // Bridges
            let blocks =
                missionData.GetGroup(group).ListOfBridge
                |> List.ofSeq
            let bridges = extractBuildingInstances(buildingDb, blocks)
            // Roads
            let graph =
                loadRoadGraph(graph, capacity)
            let roads0 = graph.SetRegions regions
            let roads1 = roads0.SetBridges bridges
            let roads2 = roads1.SetTerminals terminals
            roads2, bridges
        // Roads
        let roads, roadBridges = loadRoads "BridgesHW" (Path.Combine(exeDir, "Config", sprintf "Roads-%s.json" mapName)) roadsCapacity
        // Railroads
        let rails, railBridges = loadRoads "BridgesRW" (Path.Combine(exeDir, "Config", sprintf "Rails-%s.json" mapName)) railsCapacity
        let bridges =
            Seq.concat [roadBridges; railBridges]
            |> Seq.map (fun bridge -> bridge.Id, bridge)
            |> dict
        // Misc data
        let scenario = System.IO.Path.GetFileNameWithoutExtension(scenario)
        let startDate = options.GetDate()
        let hour, minute, second = options.GetTime().Value
        let regions =
            regions
            |> Seq.map (fun r -> r.RegionId, r)
            |> dict
        let airfields =
            airfields
            |> Seq.map (fun af -> af.AirfieldId, af)
            |> dict
        {
            Scenario = scenario
            Map = mapName
            StartDate = System.DateTime(startDate.Year, startDate.Month, startDate.Day, hour.Value, minute.Value, second.Value)
            WeatherDaysOffset = 0.0
            RepairSpeed = 1.0f<E/H>
            RepairCostRatio = 2.0f<E/M^3>
            TransportRepairCostRatio = 1.0f<E/(M^3/H)>
            GroundForcesCost = 10.0f<E/MGF/H>
            GroundForcesTransportCost = 5.0f<M^3/H/MGF>
            ResourceVolume = 1.0f<M^3/E>
            ResourceProductionRate = 1.0f<E/H/M^3>
            Regions = regions
            Roads = roads
            Rails = rails
            Airfields = airfields
            Buildings = buildingsDict
            Bridges = bridges
            PlaneSet = dict[]
        }

module IO =
    open Newtonsoft.Json
    open System.IO

    type World with
        member this.SaveToFile(path : string) =
            let serializer = new JsonSerializer()
            use outStream = File.CreateText(path)
            serializer.Serialize(outStream, this)

        static member LoadFromFile(path : string) =
            let serializer = new JsonSerializer()
            use inStream = File.OpenText(path)
            serializer.Deserialize(inStream, typeof<World>) :?> World