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
open Util.MapExt

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel
open Campaign.Common.Buildings
open Campaign.Common.GroundUnit

open PilotRanks
open Campaign.Common.Ship

let private logger = NLog.LogManager.GetCurrentClassLogger()

type Region = {
    RegionId : RegionId
    [<Json.Vector2ListJsonField>]
    Boundary : Vector2 list
    [<Json.Vector2JsonField>]
    Position : Vector2
    Neighbours : RegionId list
    InitialOwner : CoalitionId option
    /// Whether supplies arrive/are produced in this region. Also called "rear region" if true.
    IsEntry : bool
    IndustryBuildings : BuildingInstanceId list
}
with
    interface IRegion with
        member this.Boundary: Vector2 list = 
            this.Boundary
        member this.InitialOwner: CoalitionId option = 
            this.InitialOwner
        member this.Neighbours: RegionId list = 
            this.Neighbours
        member this.Position: Vector2 = 
            this.Position
        member this.RegionId: RegionId = 
            this.RegionId

/// Return the edge in regA's boundary that is shared with regB's boundary, if any.
let commonBorder (regA : Region, regB : Region) =
    (regA.Boundary, regB.Boundary)
    ||> Seq.allPairs
    |> Seq.filter (fun (p1, p2) -> (p1 - p2).Length() < 2000.0f)
    |> List.ofSeq
    |> function
        | [(p1, _); (p2, _)] -> Some (p1, p2)
        | [] | [_] -> None
        | xs ->
            // Normally two regions should have at most one common edge, as their boundaries are convex.
            // If for whatever reason we get more than two vertices, pick the two with the largest distance between them.
            Seq.allPairs xs xs
            |> Seq.maxBy (fun ((p1, _), (p2, _)) -> (p1 - p2).LengthSquared())
            |> fun ((p1, _), (p2, _)) -> Some (p1, p2)

/// A node in the logistics network
type NetworkNode = {
    Id : int
    [<Json.Vector2JsonField>]
    Pos : Vector2
    Region : RegionId option
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
    /// Remove nodes and links refering to these nodes
    member this.RemoveNodes(nodeIds : Set<int>) =
        let nodes =
            this.Nodes
            |> List.filter (fun node -> not(nodeIds.Contains(node.Id)))
        let links =
            this.Links
            |> List.filter (fun link -> not(nodeIds.Contains(link.NodeA)) && not(nodeIds.Contains(link.NodeB)))
        {
            Nodes = nodes
            Links = links
        }

    /// Set the region of each node, and remove nodes that are not in a region
    member this.SetRegions(regions : Region list) =
        // Drop nodes outside hull of all regions
        let hull = VectorExtension.convexHull (regions |> List.collect (fun region -> region.Boundary))
        let outOfHull =
            this.Nodes
            |> Seq.choose (fun node ->
                if node.Pos.IsInConvexPolygon(hull) then
                    None
                else
                    Some node.Id)
            |> Set.ofSeq
        let this = this.RemoveNodes(outOfHull)
        // Set regions
        let nodes =
            this.Nodes
            |> List.map (fun node ->
                let region =
                    regions
                    |> List.tryFind (fun region -> node.Pos.IsInConvexPolygon(region.Boundary))
                match region with
                | Some region ->
                    { node with Region = Some region.RegionId }
                | None ->
                    node
            )
        // Result
        { this with Nodes = nodes }

    /// Set the bridges located on each link
    member this.SetBridges(bridges : BuildingInstance list, buildingDb : BuildingProperties list) =
        // Find bridges by their exact position
        let byPos =
            bridges
            |> Seq.map (fun instance -> instance.Pos, instance.Script)
            |> dict
        let byScript =
            buildingDb
            |> Seq.map (fun building -> building.Script, building)
            |> dict
        // Get the boundary of each bridge, and cache values
        let getBoundary pos =
            pos
            |> (byPos.TryGetValue >> Option.ofPair)
            |> Option.bind (byScript.TryGetValue >> Option.ofPair)
            |> Option.map (fun props ->
                props.Boundary
                |> List.map (fun v -> v.Rotate(pos.Rotation) + pos.Pos))
            |> Option.defaultValue []
        let cache = System.Collections.Generic.Dictionary()
        let cachedGetBoundary = SturmovikMission.Cached.cached cache getBoundary
        // Quick intersection tests for bridges
        let tree = Campaign.SpacePartition.QuadTree.fromBoundaryOjects cachedGetBoundary 10 10 true (bridges |> Seq.map (fun instance -> instance.Pos))
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
        // If a region has nodes, but none are in terminal areas, force some
        let regionsWithoutTerminals =
            nodes
            |> Seq.groupBy (fun node -> node.Region)
            |> Seq.filter (fun (_, nodes) -> nodes |> Seq.exists (fun node -> node.HasTerminal) |> not)
            |> Seq.choose fst
            |> Set.ofSeq
        let nodes =
            ((regionsWithoutTerminals, None), nodes)
            ||> List.scan (fun (regionsWithoutTerminals, _) node ->
                match node.Region with
                | Some region ->
                    if regionsWithoutTerminals.Contains(region) then
                        logger.Warn(sprintf "Region %s has transport nodes but terminal area does not cover any (check both roads and railways)" (string region))
                        regionsWithoutTerminals.Remove(region), Some { node with HasTerminal = true }
                    else
                        regionsWithoutTerminals, Some node
                | None ->
                    regionsWithoutTerminals, Some node
            )
            |> List.choose snd
        // Result
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
with
    /// Get shortest path from set of nodes to another set of nodes
    member this.FindPath(sources : Set<int>, goals : Set<int>) =
        let distToGoals idx =
            let node = this.GetNode idx
            goals
            |> Seq.map (fun goal -> (this.GetNode(goal).Pos - node.Pos).Length())
            |> Seq.min

        let working =
            if goals.IsEmpty then
                Set.empty
            else
                sources
                |> Seq.map (fun src -> distToGoals src, src)
                |> Set

        let prec = Seq.mutableDict []

        let rec walkBack idx =
            [
                yield idx
                if not(sources.Contains idx) then
                    assert prec.ContainsKey(idx)
                    yield! walkBack prec.[idx]
            ]

        let rec work(working : Set<float32 * int>, visited : Set<int>) =
            if Set.isEmpty working then
                logger.Debug(sprintf "Path finding found no path after %d visited nodes" visited.Count)
                None
            else
                let (_, curr) as x = Set.minElement working
                if goals.Contains curr then
                    logger.Debug(sprintf "Path finding walk back started after visiting %d nodes" visited.Count)
                    let path =
                        walkBack curr
                        |> List.rev
                    logger.Debug(sprintf "Path finding walk back done, found %d-steps long path" path.Length)
                    Some path
                else
                    let working = Set.remove x working
                    let visited = Set.add curr visited
                    let succs, _ = this.GetLink curr
                    let working =
                        (working, succs)
                        ||> Seq.fold (fun working succ ->
                            if visited.Contains succ then
                                working
                            else
                                prec.[succ] <- curr
                                working.Add (distToGoals succ, succ)
                        )
                    work(working, visited)

        let res =
            work(working, Set.empty)
            |> Option.map (fun path ->
                path
                |> Seq.pairwise
                |> Seq.map (fun (nodeA, nodeB) ->
                    let asuccs, f = this.GetLink(nodeA)
                    assert(asuccs |> Seq.exists ((=) nodeB))
                    f nodeB)
                |> List.ofSeq
            )
        res

type Network with
    member this.GetQuickAccess() =
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
                assert(nodes.ContainsKey(nodeA))
                nodeA,
                links
                |> Seq.map (fun link ->
                    assert(nodes.ContainsKey(link.NodeB))
                    link.NodeB, link)
                |> dict)
            |> dict
        {
            Data = this
            GetNode = fun x ->
                assert(nodes.ContainsKey(x))
                nodes.[x]
            GetLink = fun x ->
                match links.TryGetValue x with
                | true, dict ->
                    upcast dict.Keys, fun y -> dict.[y]
                | false, _ ->
                    upcast Seq.empty, fun y -> failwithf "No link from %d to %d" x y
        }

type Runway = {
    SpawnPos : OrientedPosition
    [<Json.Vector2ListJsonField>]
    PathToRunway : Vector2 list
    [<Json.Vector2ListJsonField>]
    PathOffRunway : Vector2 list
    [<Json.Vector2JsonField>]
    Start : Vector2
    [<Json.Vector2JsonField>]
    End : Vector2
}
with
    member this.Name =
        int((this.End - this.Start).YOri / 10.0f)
        |> sprintf "%02d"

    interface IRunway with
        member this.End: Vector2 = 
            this.End
        member this.PathOffRunway: Vector2 list = 
            this.PathOffRunway
        member this.PathToRunway: Vector2 list = 
            this.PathToRunway
        member this.SpawnPos: OrientedPosition = 
            this.SpawnPos
        member this.Start: Vector2 = 
            this.Start
        member this.Name =
            this.Name

type Airfield = {
    AirfieldId : AirfieldId
    [<Json.Vector2JsonField>]
    Position : Vector2
    Region : RegionId
    [<Json.Vector2ListJsonField>]
    Boundary : Vector2 list
    Runways : Runway list
    Facilities : BuildingInstanceId list
}
with
    interface IAirfield with
        member this.AirfieldId: AirfieldId = 
            this.AirfieldId
        member this.Boundary: Vector2 list = 
            this.Boundary
        member this.Position: Vector2 = 
            this.Position
        member this.Runways: IRunway list = 
            this.Runways |> List.map (fun runway -> runway :> IRunway)

    // Airfields without runways can be used by players for emergency landings, but are otherwise unused.
    member this.IsActive = not this.Runways.IsEmpty

type World = {
    /// Base name of scenario file
    Scenario : string
    /// Name of the map where the scenario takes place
    Map : string
    /// Date of the first mission.
    StartDate : System.DateTime
    /// Briefing from the scenario file
    Briefing : string option
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
    RegionsList : Region list
    /// Max capacity of a large bridge
    BridgeCapacity : float32<M^3/H>
    /// The road network
    Roads : Network
    /// The rail network
    Rails : Network
    /// Seaways
    Seaways : Network
    /// Rivers
    Rivers : Network
    /// Descriptions of all airfields
    AirfieldsList : Airfield list
    /// Building properties, including bridges
    BuildingPropertiesList : BuildingProperties list
    /// Building instances, excluding bridges
    BuildingsList : BuildingInstance list
    /// Bridge instances
    BridgesList : BuildingInstance list
    /// All plane models
    PlaneModelsList : PlaneModel list
    /// Alternative planes to make available to players, can be used to allow players who don't own a plane to spawn anyway in similar planes.
    /// Can also be used for AI-only planes, such as the B-25.
    PlaneAltsList : (PlaneModelId * PlaneModel list) list
    /// Participating countries and their coalition
    CountriesList : (CountryId * CoalitionId) list
    /// Typical names for each country
    Names : NameDatabase
    /// Ranks in the air force of each country
    Ranks : RanksDatabase
    /// Awards in the air force of each country
    Awards : AwardDatabase
    /// Ground units database
    GroundUnitsList : GroundUnit list
    /// Ground units of each participating country
    GroundUnitsOfCountryList : (CountryId * GroundUnitId list) list
    /// Ships of each partipating country
    ShipsList : (CountryId * ShipProperties list) list
}
with
    /// Portion of ground forces dedicated to anti-air
    member this.AntiAirGroundForcesRatio = 0.15f

    /// Resources available in entry regions for a coalition
    member this.CoalitionEntryResources(_ : CoalitionId) = 50000.0f<E/H>

module private DynProps =
    let cacheBy getXs getKey =
        Util.Caching.cachedProperty (fun (world : World) -> getXs world |> Seq.map (fun x -> getKey x, x) |> dict)

    let cacheByKvp getXs =
        Util.Caching.cachedProperty (fun (world : World) -> getXs world |> dict)
    
    let regions = cacheBy (fun world -> world.RegionsList) (fun region -> region.RegionId)

    let airfields = cacheBy (fun world -> world.AirfieldsList) (fun af -> af.AirfieldId)

    let buildings = cacheBy (fun world -> world.BuildingsList) (fun building -> BuildingInstanceId building.Pos)

    let bridges = cacheBy (fun world -> world.BridgesList) (fun building -> BuildingInstanceId building.Pos)

    let planeSet = cacheBy (fun world -> world.PlaneModelsList) (fun plane -> plane.Id)

    let planeAlts = cacheByKvp (fun world -> world.PlaneAltsList)

    let countries = cacheByKvp (fun world -> world.CountriesList)

    let buildingProps = cacheBy(fun world -> world.BuildingPropertiesList) (fun building -> building.Script)

    let groundUnits = cacheBy (fun world -> world.GroundUnitsList) (fun groundUnit -> groundUnit.Id)

    let groundUnitsCountry =
        cacheByKvp
            (fun world ->
                world.GroundUnitsOfCountryList
                |> List.collect (fun (country, units) -> List.allPairs units [country]))

type World with
    /// Mapping from RegionId to Region
    member this.Regions = DynProps.regions this

    /// Mapping from AirfieldId to Airfield
    member this.Airfields = DynProps.airfields this

    /// Mapping from building instance identifiers to building instances
    member this.Buildings = DynProps.buildings this

    /// Mapping from bridge instance identifiers to bridge instances
    member this.Bridges = DynProps.bridges this

    /// Mapping from plane model identifiers to plane model descriptions
    member this.PlaneSet = DynProps.planeSet this

    /// Mapping from plane model identifiers to alternative plane models
    member this.PlaneAlts = DynProps.planeAlts this

    /// Mapping from country to the coalition they are belong to
    member this.Countries = DynProps.countries this

    /// Mapping from building and bridge script to building properties
    member this.BuildingProperties = DynProps.buildingProps this

    /// Mapping from ground unit ID to ground units
    member this.GroundUnits = DynProps.groundUnits this

    /// Get the ground units of a country
    member this.GroundUnitsOfCountry (country : CountryId) =
        this.GroundUnitsOfCountryList
        |> List.tryPick (fun (c, units) -> if c = country then Some units else None)
        |> Option.defaultValue []

    /// Mapping from ground unit ID to country
    member this.CountryOfGroundUnit =
        DynProps.groundUnitsCountry this

    /// Find the region that covers a coordinate, or failing that the one with the closest boundary vertex.
    member this.FindRegionAt(pos : Vector2) =
        this.Regions.Values
        |> Seq.tryFind (fun region -> pos.IsInConvexPolygon region.Boundary)
        |> Option.defaultWith (fun () ->
            this.Regions.Values
            |> Seq.minBy (fun region ->
                region.Boundary
                |> Seq.map (fun v -> (v - pos).LengthSquared())
                |> Seq.min
            )
        )

    /// Get building or bridge instance by its ID
    member this.TryGetBuildingInstance(bid : BuildingInstanceId) =
        [this.Buildings; this.Bridges]
        |> Seq.tryPick (fun d -> d.TryGetValue(bid) |> Option.ofPair)
        |> Option.map (fun building -> building, this.BuildingProperties.TryGetValue(building.Script) |> Option.ofPair)

    member this.GetAnyCountryInCoalition(coalition) =
        this.CountriesList
        |> Seq.pick(fun (country, coalition2) -> if coalition2 = coalition then Some country else None)

    /// A seed which can be used pseudo-random generation that is reproducible for a given world.
    /// Can be used e.g. for weather updates.
    member this.Seed = hash(this.Scenario, this.Map, this.StartDate, this.WeatherDaysOffset)

    /// Compute the forces that can travel over a transport link during a given time duration.
    member this.GroundForcesTransport(linkCapacity : float32<M^3/H>, forces : float32<MGF>, duration : float32<H>) =
        if forces = 0.0f<MGF> then
            0.0f<MGF>
        else
            let desired = forces * this.GroundForcesTransportCost * duration
            let available = linkCapacity * duration
            let actual = min desired available
            forces * actual / desired

    member this.RegionHasAirfield(region : RegionId) =
        this.AirfieldsList
        |> Seq.exists (fun af -> af.Region = region)

    member this.BoundaryOf(bId : BuildingInstanceId) =
        match this.TryGetBuildingInstance bId with
        | Some(building, Some props) ->
            props.Boundary
            |> List.map (fun v -> v.Rotate(building.Pos.Rotation) + building.Pos.Pos)
        | None | Some(_, None) ->
            []

    member this.CoalitionHasShips(coalition : CoalitionId, roles : ShipRole list) =
        let ships =
            this.ShipsList
            |> List.collect (fun (country, ships) ->
                if this.Countries.TryGetValue(country) = (true, coalition) then
                    ships
                else
                    [])
        roles
        |> List.forall (fun role ->
            ships
            |> List.exists (fun ship -> ship.Roles |> List.exists ((=) role))
        )

module Init =
    open System.IO
    open System.Reflection
    open SturmovikMission.DataProvider.Parsing
    open FSharp.Data

    open Util.StringPatterns
    open Util.RegexActivePatterns

    open SturmovikMission.Blocks.BlocksMissionData
    open SturmovikMission.Blocks.BlocksMissionData.CommonMethods

    [<Literal>]
    let private sampleFile = __SOURCE_DIRECTORY__ + @"\..\Config\Roads-Sample.json"
    type private JsonNetwork = JsonProvider<sampleFile>

    /// Extract BuildingProperties from a block inside a delimiting influence area
    let inline extractBuildingProperties(building : ^Block, boundary : T.MCU_TR_InfluenceArea, tryGetDurability) =
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
        let durability =
            tryGetDurability (CommonMethods.getScript(building) |> CommonMethods.valueOf)
            |> Option.defaultValue (building |> getDurability |> valueOf)
        {
            Model = building |> getModel |> valueOf
            Script = building |> getScript |> valueOf
            Boundary = vertices
            SubParts = subparts
            Durability = durability
            ParkingSpots = []
        }

    /// Load a list of BuildingProperties from a .Mission file
    let loadBuildingPropertiesList(path : string, tryGetDurability) =
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
                        yield extractBuildingProperties(block, data, tryGetDurability)
                    | None ->   ()
            ]
        [ build blocks ; build bridges ]
        |> List.concat

    /// Build a mapping from building script to its parking spots
    let loadBuildingParkingSpots(path : string) =
        // Extract buildings and waypoints from the mission file
        let data =
            try
                T.GroupData.Parse(Stream.FromFile path)
            with
            | :? ParseError as e ->
                printParseError e
                |> String.concat "\n"
                |> failwith
        let buildings = data.ListOfBlock
        let spots = data.ListOfMCU_Waypoint
        /// Find the closest block to a given position
        let buildingAt (pos : Vector2) =
            buildings
            |> Seq.map (fun block ->
                let bpos = Vector2.FromPos block
                (pos - bpos).Length(), block)
            |> Seq.minBy fst
            |> fun (dist, block) ->
                if dist < 1000.0f then
                    Some block
                else
                    None
        // group waypoints, which denote parking spots, by building script.
        // Position of parking spot is adjusted to be relative to the building's position.
        spots
        |> Seq.map (fun spot -> { Pos = OrientedPosition.FromMission spot; Radius = float32(spot.GetArea().Value) })
        |> Seq.groupBy (fun spot -> buildingAt spot.Pos.Pos |> Option.map (fun block -> block.GetScript().Value, Vector2.FromPos block))
        |> Seq.choose (
            function
            | (Some (script, bpos), spots) ->
                Some (
                    script,
                    spots
                    |> Seq.map (fun spot -> { spot with Pos = { spot.Pos with Pos = spot.Pos.Pos - bpos } }))
            | _ -> None)
        |> dict

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
                            Script = props.Script }
                | false, _ ->
                    ()
        ]

    /// Extract a list of regions from a list of influence areas and a list of building instances
    let extractRegions(regions : T.MCU_TR_InfluenceArea list, buildings : BuildingInstance list, coalitionOf : CountryId -> CoalitionId) =
        let extractOne (region : T.MCU_TR_InfluenceArea) : Region =
            let coalition =
                enum(region.GetCountry().Value)
                |> CountryId.FromMcuValue
                |> Option.map coalitionOf
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
        // Assign each vertex to the 4 nearest coordinates that are multiple of cellRadius
        let cellRadius = 500.0f
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
        // Mapping from multiples of cellRadius to Region, vertex index and vertex itself
        let located =
            withBoundaries
            |> List.collect (fun region ->
                region.Boundary
                |> List.mapi(fun i v ->
                    nearestCenters v
                    |> List.map (fun center -> center, (region.RegionId, i, v))
                )
                |> List.concat
            )
            |> Seq.groupBy fst
            |> Seq.map (fun (k, items) -> k, items |> Seq.map snd |> List.ofSeq)
            |> dict
        // Check all vertices and regions that are assigned to the same multiple, retain all pairs that are closer than the cell radius
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
                            Region = None
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
            let exit =
                vecs
                |> List.rev
                |> List.takeWhile (fun (_, t) -> t < 2)
                |> List.map fst
                |> List.rev
            {
                SpawnPos = pos
                PathToRunway = path
                PathOffRunway = exit
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

    let tryGetDurability path =
        if File.Exists path then
            let lines = File.ReadAllLines path
            // The csv file uses the names from the mission editor GUI, which does not always match the script name
            let prefixes = [ "af_"; "arf_"; "arf_eu_"; "bf_" ]
            let m =
                dict [
                    for line in lines do
                        match List.ofSeq(line.Split ';') with
                        | name :: AsInt32 durability :: _ ->
                            yield name, durability
                            for prefix in prefixes do
                                if name.StartsWith prefix then
                                    yield name.Substring(prefix.Length), durability
                        | _ -> ()
                ]
            fun (script : string) ->
                Path.GetFileNameWithoutExtension(script)
                |> m.TryGetValue
                |> Option.ofPair
        else
            fun _ -> None

    let extractPlaneSet(fakefield : T.Airfield, coalition) =
        let scriptsAndWeaponMods =
            [
                let planes =
                    fakefield.TryGetPlanes()
                    |> Option.map (fun planes -> planes.GetPlanes())
                    |> Option.defaultValue Seq.empty
                for plane in planes do
                    let weaponModFilter =
                        plane.GetAvMods().Value.Split '/'
                        |> Seq.choose ModRange.TryFromString
                        |> List.ofSeq
                    yield plane.GetScript().Value.ToLowerInvariant().Trim(), weaponModFilter
            ]
        let planeDb =
            PlaneModelDb.planeDb
            |> List.map (fun plane -> plane.ScriptModel.Script.ToLowerInvariant().Trim(), plane)
            |> dict
        [
            for script, filter in scriptsAndWeaponMods do
                match planeDb.TryGetValue(script) with
                | true, x ->
                    let costs =
                        match filter with
                        | [] ->
                            x.WeaponModsCosts
                        | _ ->
                            let costs0 = Map x.WeaponModsCosts
                            List.init x.LastWeaponMod ((+) 1)
                            |> List.choose (fun n ->
                                let isInFilter =
                                    filter
                                    |> List.exists (function One n2 -> n = n2 | Interval(n1, n2) -> n1 <= n && n <= n2)
                                if isInFilter then
                                    costs0.TryFind(n) |> Option.map (fun v -> n, v)
                                else
                                    Some(n, 1.0f<E> * System.Single.MaxValue)
                            )
                    yield { x with WeaponModsCosts = costs; Coalition = coalition }
                | false, _ ->
                    logger.Error(sprintf "No plane named with script '%s' found in the plane db" script)
                    ()
        ]

    let extractShips(ships : T.Ship seq) =
        [
            for ship in ships do
                let country =
                    ship.GetCountry().Value
                    |> enum
                    |> CountryId.FromMcuValue
                match country with
                | Some country ->
                    let name = ship.GetName().Value
                    let desc = ship.GetDesc().Value
                    let roles = extractShipRoles desc
                    if roles.Length > 0 then
                        let model = ship.GetModel().Value
                        let script = ship.GetScript().Value
                        let scriptFilename = System.IO.Path.GetFileName(script)
                        match knownShipLogNames |> List.tryFind (fun (_, scriptName) -> scriptName = scriptFilename) with
                        | Some (logName, _) ->
                            yield
                                country, {
                                    Name = name
                                    LogName = logName
                                    Roles = roles |> List.ofArray
                                    ScriptModel = {
                                        Script = script
                                        Model = model
                                    }
                                }
                        | None ->
                            ()
                | None ->
                    ()
        ]

    let extractSeaways(waypoints : T.MCU_Waypoint seq) =
        let nodes =
            waypoints
            |> Seq.map (fun wp ->
                {
                    Id = wp.GetIndex().Value
                    Pos = Vector2.FromPos wp
                    Region = None
                    HasTerminal = wp.GetTargets().Value.Length = 0
                }
            )
            |> List.ofSeq
        let links =
            waypoints
            |> Seq.collect (fun wp ->
                wp.GetTargets().Value
                |> Seq.map (fun tgt ->
                    {
                        NodeA = wp.GetIndex().Value
                        NodeB = tgt
                        Bridges = []
                        FlowCapacity = 1e9f<M^3/H>
                    }
                )
            )
            |> List.ofSeq
        let targetted =
            waypoints
            |> Seq.collect (fun wp -> wp.GetTargets().Value)
            |> Set
        let nodes =
            nodes
            |> List.map (fun node ->
                { node with
                    HasTerminal = node.HasTerminal || not(targetted.Contains(node.Id))
                })
        {
            Nodes = nodes
            Links = links
        }

    /// Load a scenario mission file and create a world description.
    let mkWorld(scenario : string, roadsCapacity : float32<M^3/H>, railsCapacity : float32<M^3/H>) =
        let exeDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let tryGetDurability = tryGetDurability (Path.Combine(exeDir, "Config", "durability.csv"))
        let parkingSpots = loadBuildingParkingSpots (Path.Combine(exeDir, "Parking.Mission"))
        let buildingDb = loadBuildingPropertiesList (Path.Combine(exeDir, "Buildings.Mission"), tryGetDurability)
        let buildingDb =
            buildingDb
            |> List.map (fun building ->
                match parkingSpots.TryGetValue(building.Script) with
                | true, spots -> { building with ParkingSpots = List.ofSeq spots }
                | false, _ -> building
            )
        let missionData =
            try
                T.GroupData.Parse(Stream.FromFile scenario)
            with
            | :? ParseError as err ->
                printParseError err
                |> String.concat "\n"
                |> eprintfn "%s"
                failwithf "Failed to parse scenario '%s'" scenario
            | exc ->
                failwithf "Failed to parse scenario '%s': %s" scenario exc.Message
        let missionStrings =
            let allContent =
                let stringsFile = System.IO.Path.GetFileNameWithoutExtension(scenario) + ".eng"
                let stringsPath = System.IO.Path.Combine(System.IO.Path.GetDirectoryName(scenario), stringsFile)
                try
                    use stringsFile = System.IO.File.OpenRead(stringsPath)
                    let encoding = System.Text.UnicodeEncoding(false, true)
                    use reader = new StreamReader(stringsFile, encoding)
                    [
                        while not reader.EndOfStream do
                            yield reader.ReadLine()
                    ]
                with
                | exc ->
                    failwithf "Failed to read scenario strings '%s': %s" stringsPath exc.Message
            allContent
            |> List.choose (fun line ->
                match line.Split(':', 2) with
                | [| AsInt num; text |] ->
                    Some(num, text)
                | _ ->
                    None
            )
            |> dict
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
            Seq.append (missionData.GetGroup("Static").ListOfBlock) (missionData.GetGroup("Airfields").ListOfBlock)
            |> Seq.filter (fun block ->
                let pos = Vector2.FromPos block
                regionBoundaries
                |> Seq.exists (fun boundary -> pos.IsInConvexPolygon boundary))
            |> List.ofSeq
        // Building instances
        let buildings = extractBuildingInstances(buildingDb, blocks)
        let buildingsMap =
            buildings
            |> Seq.map (fun b -> b.Id, b)
            |> Map
        // Map name
        let options = Seq.head missionData.ListOfOptions
        let mapName = options.GetGuiMap().Value
        let mapLocationName =
            match mapName with
            | Contains "rheinland" -> "rheinland"
            | Contains "moscow" -> "moscow"
            | Contains "kuban" -> "kuban"
            | Contains "stalingrad" -> "stalingrad"
            | _ -> mapName
        // Mapping from countries to coalitions
        let countries =
            options.GetCountries().Value
            |> Seq.choose (fun x ->
                let country, coalition = x.Value
                match CountryId.FromMcuValue(enum country.Value), CoalitionId.FromMcuValue(enum coalition.Value) with
                | Some country, Some coalition -> Some(country, coalition)
                | _ -> None)
            |> List.ofSeq
        // Regions
        let regions =
            let countries = Map countries
            extractRegions(regionAreas, buildings, fun x -> countries.[x])
        // Airfields
        let airfieldAreas = missionData.GetGroup("Airfields").ListOfMCU_TR_InfluenceArea |> List.ofSeq
        let airfieldSpawns = missionData.GetGroup("Airfields").ListOfAirfield |> List.ofSeq
        let airfields = extractAirfields(airfieldSpawns, airfieldAreas, regions, fun bid -> buildingsMap.[bid])
        let regions = cleanRegionBuildings(regions, airfields)
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
            let roads1 = roads0.SetBridges(bridges, buildingDb)
            let roads2 = roads1.SetTerminals terminals
            roads2, bridges
        // Roads
        let roads, roadBridges = loadRoads "BridgesHW" (Path.Combine(exeDir, "Config", sprintf "Roads-%s.json" mapLocationName)) roadsCapacity
        // Railroads
        let rails, railBridges = loadRoads "BridgesRW" (Path.Combine(exeDir, "Config", sprintf "Rails-%s.json" mapLocationName)) railsCapacity
        let bridges = roadBridges @ railBridges
        // Seaways
        let seaways =
            missionData.GetGroup("Seaways").ListOfMCU_Waypoint
            |> extractSeaways
        let seaways = seaways.SetRegions regions
        // Rivers
        let rivers =
            missionData.GetGroup("Waterways").ListOfMCU_Waypoint
            |> extractSeaways
        let rivers = rivers.SetRegions regions
        // Ground units
        let groundUnits =
            loadGroundUnitsDb (Path.Combine(exeDir, "Config", "GroundUnitDb.json"))
            |> List.ofArray
        // Planesets
        let planeModels =
            [
                let countries = Map.ofList countries
                for airfield in missionData.ListOfAirfield do
                    let coalition =
                        CountryId.FromMcuValue(enum (airfield.GetCountry().Value))
                        |> Option.bind countries.TryFind
                    match coalition with
                    | Some coalition ->
                        yield! extractPlaneSet(airfield, coalition)
                    | None ->
                        ()
            ]
        // Ships
        let ships =
            extractShips (missionData.GetGroup("Ships").ListOfShip)
            |> List.groupBy fst
            |> List.map (fun (k, vs) -> k, vs |> List.map snd)

        // Misc data
        let scenario = System.IO.Path.GetFileNameWithoutExtension(scenario)
        let startDate = options.GetDate()
        let briefing =
            match missionStrings.TryGetValue(1 (* options.GetLCDesc().Value *)) with
            | true, text -> Some text
            | _ -> None
        // Bridge and transport misc factors
        let repairSpeed = 1.0f<E/H>
        let bridgeCapacity =
            railsCapacity
        // Time to fully repair one large bridge
        let bridgeRepairTime = 10.0f<H>
        let transportRepairCostRatio =  repairSpeed * bridgeRepairTime / bridgeCapacity
        let hour, minute, second = options.GetTime().Value
        {
            Scenario = scenario
            Map = mapName
            StartDate = System.DateTime(startDate.Year, startDate.Month, startDate.Day, hour.Value, minute.Value, second.Value)
            Briefing = briefing
            WeatherDaysOffset = 0.0
            RepairSpeed = repairSpeed
            RepairCostRatio = 2.0f<E/M^3>
            TransportRepairCostRatio = transportRepairCostRatio
            GroundForcesCost = 10.0f<E/MGF/H>
            GroundForcesTransportCost = 5.0f<M^3/H/MGF>
            ResourceVolume = 1.0f<M^3/E>
            ResourceProductionRate = 1.0f<E/H/M^3>
            RegionsList = regions
            BridgeCapacity = railsCapacity
            Roads = roads
            Rails = rails
            AirfieldsList = airfields
            BuildingPropertiesList = buildingDb
            BuildingsList = buildings
            BridgesList = bridges
            PlaneModelsList = planeModels
            PlaneAltsList = []
            CountriesList = countries
            Names = NameDatabase.Default
            Ranks = RanksDatabase.Default
            Awards = AwardDatabase.Default
            GroundUnitsList = groundUnits
            GroundUnitsOfCountryList = []
            ShipsList = ships
            Seaways = seaways
            Rivers = rivers
        }

module IO =
    open System.IO
    open FSharp.Json
    open Util.Json

    type World with
        member this.SaveToFile(path : string) =
            let json =
                try
                    Json.serializeEx JsonConfig.IL2Default this
                with exc ->
                    logger.Error("Failed to serialize world to json")
                    logger.Debug(exc)
                    failwith "Failed to serialize world to json"
            use outStream = File.CreateText(path)
            outStream.Write(json)

        static member LoadFromFile(path : string) : World =
            use inStream = File.OpenText(path)
            let json = inStream.ReadToEnd()
            let data =
                try
                    Json.deserializeEx JsonConfig.IL2Default json
                with exc ->
                    logger.Error("Failed to deserialize world from json")
                    logger.Debug(exc)
                    failwith "Failed to deserialize world from json"
            data