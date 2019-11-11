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

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks
open SturmovikMission.Blocks.StaticDefenses.Types
open SturmovikMission.Blocks.BlocksMissionData.CommonMethods
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.VirtualConvoy.Factory
open Util

open Campaign.BasicTypes
open Campaign.PlaneSet
open Campaign.WorldDescription

type BuildingProperties = {
    Model : string
    Script : string
    Boundary : Vector2 list
    SubParts : int list
}
with
    /// Extract BuildingProperties from a block inside a delimiting influence area
    static member FromMission(building : T.Block, boundary : T.MCU_TR_InfluenceArea) =
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
        {
            Model = building.GetModel().Value
            Script = building.GetScript().Value
            Boundary = vertices
            SubParts = subparts
        }

    /// Extract a list of BuildingProperties from a .Mission file
    static member FromFile(path : string) =
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
                    yield BuildingProperties.FromMission(block, data)
                | None ->   ()
        ]

type BuildingInstance = {
    Pos : OrientedPosition
    Properties : BuildingProperties
}
with
    /// Create a list of building instances from a database of known building types, and a list of blocks from a .Mission file
    static member CreateInstances(db : BuildingProperties list, blocks : T.Block list) =
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
with
    /// Create a list of regions from a list of influence areas and a list of building instances
    static member ExtractRegions(regions : T.MCU_TR_InfluenceArea list, buildings : BuildingInstance list, strongProduction : float32<E/H>, buildingFlowCapacity : float32<E/H>) =
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

/// A node in the logistics network
type NetworkNode = {
    Id : int
    Pos : Vector2
    // Bridges, train stations... Affect the flow capacity when damaged
    Facilities : BuildingInstance list
    FlowCapacity : float32<E/H>
    Region : RegionId
    Neighbours : int list 
}

type Network = {
    Nodes : NetworkNode list
}
with
    /// Build a partially initialized graph from map ini files (roadssystem.ini and roads.ini or railroads.ini)
    /// Only the layout information is filled in, facilities and regions are not set
    static member NodesFromIni(system : string, roads : string, flowCapacity) =
        // parse a float using the invariant culture
        let parseFloat s =
            try
                System.Single.Parse(s, System.Globalization.CultureInfo.InvariantCulture)
            with _ -> failwithf "Failed to parse '%s' as a float" s
        // Shorthand for Regex.Match
        let matchf(line, pat) = System.Text.RegularExpressions.Regex.Match(line, pat)
        // Helper function to extract a float value from an ini file
        let getValue key lines =
            let line =
                try
                    lines
                    |> Array.find (fun (line : string) -> line.StartsWith(key))
                with _ -> failwithf "Could not find entry '%s'" key
            let posEqual = line.IndexOf("=")
            if posEqual = -1 then
                failwithf "Entry '%s' found but ill-formed" key
            parseFloat(line.Substring(posEqual + 1))
        // Relevant values extracted from roadssystem.ini
        let parseStep, mapHeight, scaleFactor =
            let lines =
                try
                    System.IO.File.ReadAllLines(system)
                with _ ->
                    failwithf "Failed to read road system ini file '%s'" system
            getValue "RoadSegmentParseStep" lines,
            getValue "Map_Height" lines,
            getValue "Map_ScaleFactor" lines
        // Transformation to apply to each coordinate component
        let truncateAndScale x =
            floor(x / parseStep) * parseStep * float32 scaleFactor
        // Extract coordinates from roads.ini
        let coords =
            [
                for line in System.IO.File.ReadAllLines(roads) do
                    let mutable s = line
                    while not(System.String.IsNullOrWhiteSpace(s)) do
                        let m = matchf(s, @"(\d+(\.\d*)?),(\d+(\.\d*)?)(.*)")
                        if m.Success then
                            yield parseFloat(m.Groups.[1].Value), parseFloat(m.Groups.[3].Value)
                            s <- m.Groups.[5].Value
                        else
                            s <- ""
            ]
            |> List.map (fun (x, y) -> truncateAndScale (mapHeight - y), truncateAndScale x)
        // Create all nodes
        let nodes =
            coords
            |> List.fold (fun nodes (x, y) ->
                let node =
                    Map.tryFind (x, y) nodes
                    |> Option.defaultValue
                        { Id = nodes.Count
                          Pos = Vector2(x, y)
                          Facilities = []
                          FlowCapacity = flowCapacity
                          Region = RegionId ""
                          Neighbours = []
                        }
                Map.add (x, y) node nodes
            ) Map.empty
        // Set neighbours
        let nodes =
            coords
            |> Seq.pairwise
            |> Seq.fold (fun (nodes : Map<_, NetworkNode>) (v1, v2) ->
                let node1 = nodes.[v1]
                let node2 = nodes.[v2]
                nodes
                |> Map.add v1 { node1 with Neighbours = node2.Id :: node1.Neighbours }
                |> Map.add v2 { node2 with Neighbours = node1.Id :: node2.Neighbours }
            ) nodes
        // Retain nodes from the mapping, remove self-references in neighbours
        let nodes =
            nodes
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.map (fun node -> { node with Neighbours = node.Neighbours |> List.filter ((<>) node.Id) })
            |> List.ofSeq
        // Result
        { Nodes = nodes }

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
        // Remove references to neighbours that were dropped
        let retained =
            nodes
            |> Seq.map (fun node -> node.Id)
            |> Set.ofSeq
        let nodes =
            this.Nodes
            |> List.map (fun node ->
                let ngh =
                    node.Neighbours
                    |> List.filter (retained.Contains)
                { node with Neighbours = ngh })
        // Result
        { this with Nodes = nodes }


type Runway = {
    SpawnPos : OrientedPosition
    PathToRunway : Vector2 list
    Start : Vector2
    End : Vector2
    Facilities : BuildingInstance list
}

type Airfield = {
    AirfieldId : AirfieldId
    Region : RegionId
    Runways : Runway list
    Facilities : StaticGroup list
}