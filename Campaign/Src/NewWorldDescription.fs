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

type BuildingInstance = {
    Pos : OrientedPosition
    Properties : BuildingProperties
}

type Region = {
    RegionId : RegionId
    Boundary : Vector2 list
    Position : Vector2
    Neighbours : RegionId list
    InitialOwner : CoalitionId option
    // Production in the region; Typically only the rearmost region should have production.
    // It represents the entry point into the game for supplies, and cannot be affected.
    Production : float32<E/H>
    // Supplies can pass through the region, affected by industry
    FlowCapacity : float32<E/H>
    IndustryBuildings : BuildingInstance list
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
    Bridges : BuildingInstance list
    FlowCapacity : float32<E/H>
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
        let cachedGetBoundary = SturmovikMission.DataProvider.Cached.cached cache getBoundary
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
        // Find bridges between nodes of each link
        let links =
            this.Links
            |> List.map (fun link ->
                let v1 = this.Nodes.[link.NodeA].Pos
                let v2 = this.Nodes.[link.NodeB].Pos
                let bridges =
                    finder.FindIntersectingItems (v1, v2)
                    |> Seq.distinct
                    |> Seq.map (fun pos -> byPos.[pos])
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


type Runway = {
    SpawnPos : OrientedPosition
    PathToRunway : Vector2 list
    Start : Vector2
    End : Vector2
}

type Airfield = {
    AirfieldId : AirfieldId
    Region : RegionId
    Boundary : Vector2 list
    Runways : Runway list
    FlowCapacity : float32<E/H>
    Facilities : BuildingInstance list
}

type World = {
    /// Base name of scenario file
    Scenario : string
    /// Name of the map where the scenario takes place
    Map : string
    /// Date of the first mission.
    StartDate : System.DateTime
    /// Weather offset: affects how late or early the weather pattern is.
    WeatherDaysOffset : float
    Planes : PlaneModel list
    Regions : Region list
    Roads : Network
    Rails : Network
    Airfields : Airfield list
}

module Loading =
    open System.IO
    open SturmovikMission.DataProvider.Parsing
    open FSharp.Data

    open SturmovikMission.Blocks.BlocksMissionData

    [<Literal>]
    let private sampleFile = __SOURCE_DIRECTORY__ + @"\..\Config\Roads-Sample.json"
    type private JsonNetwork = JsonProvider<sampleFile>

    /// Extract BuildingProperties from a block inside a delimiting influence area
    let extractBuildingProperties(building : T.Block, boundary : T.MCU_TR_InfluenceArea) =
        let pos = Vector2.FromPos(building)
        let rot = float32(building.GetYOri().Value)
        let vertices =
            boundary.GetBoundary().Value
            |> List.map (fun floats ->
                let x, y = floats.Value
                (Vector2(float32 x, float32 y) - pos).Rotate(-rot))
        let subparts =
            building.GetDamaged().Value
            |> Map.toSeq
            |> Seq.map fst
            |> List.ofSeq
        let durability = building.GetDurability().Value
        {
            Model = building.GetModel().Value
            Script = building.GetScript().Value
            Boundary = vertices
            SubParts = subparts
            Durability = durability
        }

    /// Load a list of BuildingProperties from a .Mission file
    let loadBuildingPropertiesList(path : string) =
        let data = T.GroupData(Stream.FromFile path)
        let blocks = data.ListOfBlock
        let zones = data.ListOfMCU_TR_InfluenceArea
        let zones =
            zones
            |> List.map (fun area ->
                let vertices =
                    area.GetBoundary().Value
                    |> List.map (fun floats ->
                        let x, y = floats.Value
                        Vector2(float32 x, float32 y))
                (fun (v : Vector2) -> v.IsInConvexPolygon(vertices)), area)
        [
            for block in blocks do
                let pos = Vector2.FromPos block
                match zones |> List.tryFind (fun (f, _) -> f pos) with
                | Some (_, data) ->
                    yield extractBuildingProperties(block, data)
                | None ->   ()
        ]

    /// Extract a list of building instances from a list of blocks from a .Mission file using a database of known building types 
    let extractBuildingInstances(db : BuildingProperties list, blocks : T.Block list) =
        let db =
            db
            |> Seq.map (fun building -> building.Script, building)
            |> dict
        [
            for block in blocks do
                match db.TryGetValue(block.GetScript().Value) with
                | true, props ->
                    yield { Pos =
                                { Pos = Vector2.FromPos block
                                  Rotation = block.GetYOri().Value |> float32
                                  Altitude = block.GetYPos().Value |> float32 }
                            Properties = props }
                | false, _ ->
                    ()
        ]

    /// Extract a list of regions from a list of influence areas and a list of building instances
    let extractRegions(regions : T.MCU_TR_InfluenceArea list, buildings : BuildingInstance list, strongProduction : float32<E/H>, buildingFlowCapacity : float32<E/H>) =
        let extractOne (region : T.MCU_TR_InfluenceArea) : Region =
            let coalition = CoalitionId.FromCountry (enum(region.GetCountry().Value))
            let boundary = region.GetBoundary().Value |> List.map(fun coord -> Vector2.FromPair(coord))
            let buildings =
                buildings
                |> List.filter (fun building -> building.Pos.Pos.IsInConvexPolygon boundary)
            { RegionId = RegionId(region.GetName().Value)
              Position = Vector2.FromPos(region)
              Boundary = boundary
              Neighbours = []
              Production = if region.GetDesc().Value.Contains("***") then strongProduction else 0.0f<E/H>
              InitialOwner = coalition
              FlowCapacity = buildingFlowCapacity * float32 buildings.Length
              IndustryBuildings = buildings
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
                |> List.map (fun point -> Vector2(float32(point.GetX().Value), float32(point.GetY().Value)), point.GetType().Value)
                |> List.map (fun (v, t) -> v.Rotate(pos.Rotation) + pos.Pos, t)
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
    let extractAirfields(spawns : T.Airfield list, areas : T.MCU_TR_InfluenceArea list, regions : Region list) =
        [
            for area in areas do
                let boundary =
                    area.GetBoundary().Value
                    |> List.map Vector2.FromPair
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
                        |> List.filter (fun building -> building.Pos.Pos.IsInConvexPolygon boundary)
                    let flowCapacityPerBuilding =
                        match region.IndustryBuildings |> List.length with
                        | 0 -> 0.0f<E/H>
                        | n -> region.FlowCapacity / float32 n
                    yield {
                        AirfieldId = AirfieldId (area.GetName().Value)
                        Region = region.RegionId
                        Boundary = boundary
                        Runways = runways
                        FlowCapacity = flowCapacityPerBuilding * float32(List.length buildings)
                        Facilities = buildings
                    }
                | None ->
                    ()
        ]

    /// Load a scenario mission file and create a world description.
    let loadWorld(scenario : string, strongProduction : float32<E/H>, buildingFlowCapacity : float32<E/H>, roadsCapacity : float32<E/H>, railsCapacity : float32<E/H>) =
        let buildingDb = loadBuildingPropertiesList "Buildings.Mission"
        let missionData = T.GroupData(Stream.FromFile scenario)
        // Region boundaries
        let regionAreas = missionData.GetGroup("Regions").ListOfMCU_TR_InfluenceArea
        let regionBoundaries =
            regionAreas
            |> List.map (fun area ->
                area.GetBoundary().Value
                |> List.map Vector2.FromPair)
        // Blocks inside regions
        let blocks =
            missionData.GetGroup("Static").ListOfBlock
            |> List.filter (fun block ->
                let pos = Vector2.FromPos block
                regionBoundaries
                |> List.exists (fun boundary -> pos.IsInConvexPolygon boundary))
        // Building instances
        let buildings = extractBuildingInstances(buildingDb, blocks)
        // Regions
        let regions = extractRegions(regionAreas, buildings, strongProduction, buildingFlowCapacity)
        // Airfields
        let airfieldAreas = missionData.GetGroup("Airfields").ListOfMCU_TR_InfluenceArea
        let airfieldSpawns = missionData.GetGroup("Airfields").ListOfAirfield
        let airfields = extractAirfields(airfieldSpawns, airfieldAreas, regions)
        // Map name
        let options = missionData.ListOfOptions.[0]
        let mapName = options.GetGuiMap().Value
        // Terminal areas
        let terminals =
            missionData.GetGroup("Terminals").ListOfMCU_TR_InfluenceArea
            |> List.map (fun area ->
                area.GetBoundary().Value
                |> List.map Vector2.FromPair)
        // Function to load roads and bridges
        let loadRoads group graph capacity =
            // Bridges
            let blocks =
                missionData.GetGroup(group).ListOfBlock
            let bridges = extractBuildingInstances(buildingDb, blocks)
            // Roads
            let graph =
                loadRoadGraph(graph, capacity)
            let roads0 = graph.SetRegions regions
            let roads1 = roads0.SetBridges bridges
            let roads2 = roads1.SetTerminals terminals
            roads2
        // Roads
        let roads = loadRoads "BridgesHW" (sprintf "Roads-%s.json" mapName) roadsCapacity
        // Railroads
        let rails = loadRoads "BridgesRW" (sprintf "Rails-%s.json" mapName) railsCapacity
        // Misc data
        let scenario = System.IO.Path.GetFileNameWithoutExtension(scenario)
        let startDate = options.GetDate()
        let hour, minute, second = options.GetTime().Value
        {
            Scenario = scenario
            Map = mapName
            StartDate = System.DateTime(startDate.Year, startDate.Month, startDate.Day, hour.Value, minute.Value, second.Value)
            WeatherDaysOffset = 0.0
            Planes = []
            Regions = regions
            Roads = roads
            Rails = rails
            Airfields = airfields
        }