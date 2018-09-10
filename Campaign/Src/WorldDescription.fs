﻿// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
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

module Campaign.WorldDescription

open System.Numerics
open VectorExtension

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks
open SturmovikMission.Blocks.StaticDefenses.Types
open SturmovikMission.Blocks.BlocksMissionData.CommonMethods
open SturmovikMission.DataProvider.Parsing

open Util
open Campaign.BasicTypes
open Campaign.PlaneSet
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.VirtualConvoy.Factory


let private logger = NLog.LogManager.GetCurrentClassLogger()

type RegionId =
    RegionId of string
with
    override this.ToString() =
        let (RegionId s) = this
        s

/// All items in a region are under the control of the owner of the region.
/// Regions are built from influence areas in the group "Regions" in the strategy mission file.
type Region = {
    RegionId : RegionId
    Position : Vector2
    Boundary : Vector2 list
    Neighbours : RegionId list
    /// Storage controls the amount of static anti-air and anti-tank units deployed in a region.
    Storage : StaticGroup list
    /// Regions with factories get new planes, vehicles and ammo (stored in region storage and in airfield storage).
    Production : StaticGroup list
    /// Area where tanks are parked
    Parking : Vector2 list
    /// Side who owns the region initially
    InitialOwner : CoalitionId option
    /// A strong region initially has full numbers of tanks, planes and supplies
    IsStrong : bool
}
with
    static member ExtractRegions(regions : T.MCU_TR_InfluenceArea list) =
        let extractOne (region : T.MCU_TR_InfluenceArea) =
            let coalition = CoalitionId.FromCountry (enum(region.GetCountry().Value))
            { RegionId = RegionId(region.GetName().Value)
              Position = Vector2.FromPos(region)
              Boundary = region.GetBoundary().Value |> List.map(fun coord -> Vector2.FromPair(coord))
              Neighbours = []
              Storage = []
              Production = []
              Parking = []
              InitialOwner = coalition
              IsStrong = region.GetDesc().Value.Contains("***")
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

    member private this.GetStaticBlocks(blocks : T.Block list) : StaticGroup list =
        blocks
        |> List.filter (fun block -> Vector2.FromPos(block).IsInConvexPolygon(this.Boundary))
        |> List.map StaticGroup.FromBlock

    member this.AddStorage(blocks : T.Block list, subBlockSpecs) =
        let storage =
            this.GetStaticBlocks(blocks)
            |> List.filter (fun block -> block.Storage subBlockSpecs > 0.0f<E> && not (block.IsAirfieldStorage subBlockSpecs) && block.SubBlocks(subBlockSpecs).Length > 0)
        { this with Storage = this.Storage @ storage
        }

    member this.AddProduction(blocks : T.Block list, subBlockSpecs) =
        let factories =
            this.GetStaticBlocks(blocks)
            |> List.filter (fun block -> block.Production(subBlockSpecs, 1.0f) > 0.0f<E/H> && block.SubBlocks(subBlockSpecs).Length > 0)
        { this with Production = this.Production @ factories
        }

    member this.SetParking(areas : T.MCU_TR_InfluenceArea list) =
        let parking =
            areas
            |> List.tryFind (fun area -> Vector2.FromPos(area).IsInConvexPolygon this.Boundary)
            |> Option.map (fun area -> area.GetBoundary().Value |> List.map(fun coord -> Vector2.FromPair(coord)))
            |> Option.defaultVal []
        { this with Parking = parking }

    member this.GetProductionCapacity(subBlockSpecs, factor) =
        this.Production
        |> List.sumBy (fun prod -> prod.Production(subBlockSpecs, factor))

    member this.GetStorageCapacity(subBlockSpecs) =
        this.Storage
        |> List.sumBy (fun sto -> sto.Storage(subBlockSpecs))

/// Paths link regions to their neighbours. Road and rail convoys travel along those. Those are extracted from waypoints in groups Roads and Trains respectively in the strategy mission.
type Path = {
    StartId : RegionId
    EndId : RegionId
    /// Ordered list of path vertices, composed of a location and an orientation. The orientation is important for properly orienting trains and vehicle columns.
    Locations : PathVertex list
}
with
    static member ExtractPaths(waypoints : T.MCU_Waypoint list, regions : Region list) =
        let waypointsById =
            waypoints
            |> List.map (fun wp -> wp.GetIndex().Value, wp)
            |> dict
        let spawnSide (wp : T.MCU_Waypoint) =
            match wp.GetName().Value with
            | "EndR"
            | "StartR" -> Right
            | "EndL"
            | "StartL" -> Left
            | _ -> Center
        let role (wp : T.MCU_Waypoint) =
            match wp.GetName().Value with
            | "BeforeBridge" -> NarrowZoneEntrance
            | "AfterBridge" -> NarrowZoneExit
            | _ -> Intermediate
        let mkPathVertex(wp : T.MCU_Waypoint) =
            { Pos = Vector2.FromPos wp
              Ori = float32 (wp.GetYOri().Value)
              Radius = wp.GetArea().Value
              Speed = wp.GetSpeed().Value
              Priority = wp.GetPriority().Value
              SpawnSide = spawnSide wp
              Role = role wp
            }
        let buildPath(start : T.MCU_Waypoint) =
            let rec work (current : T.MCU_Waypoint) path =
                match current.GetName().Value with
                | "End"
                | "EndR"
                | "EndL" -> mkPathVertex current :: path
                | _ ->
                    match current.GetTargets().Value with
                    | [next] ->
                        match waypointsById.TryGetValue(next) with
                        | true, next -> work next (mkPathVertex current :: path)
                        | false, _ -> failwithf "Failed building path because there is no waypoints with id '%d'" next
                    | [] ->
                        failwithf "Failed to build path because node '%d' at (%4.0f, %4.0f) has no successor" (current.GetIndex().Value) (current.GetXPos().Value) (current.GetZPos().Value)
                    | _ :: _ ->
                        failwithf "Failed to build path because node '%d' at (%4.0f, %4.0f) has too many successors" (current.GetIndex().Value) (current.GetXPos().Value) (current.GetZPos().Value)
            let path = work start []
            let startRegion =
                match regions |> List.tryFind (fun area -> Vector2.FromPos(start).IsInConvexPolygon(area.Boundary)) with
                | None -> failwithf "Failed to build path because start node '%d' at (%4.0f, %4.0f) is not in a region" (start.GetIndex().Value) (start.GetXPos().Value) (start.GetZPos().Value)
                | Some x -> x
            let endRegion =
                match path with
                | finish :: reversed ->
                    match regions |> List.tryFind (fun area -> finish.Pos.IsInConvexPolygon(area.Boundary)) with
                    | None -> failwithf "Failed to build path because end node '%d' at (%4.0f, %4.0f) is not in a region" (start.GetIndex().Value) (start.GetXPos().Value) (start.GetZPos().Value)
                    | Some x -> x
                | _ ->
                    failwith "Failed to build path because it has no end node"
            let locations =
                path
                |> List.rev
            { StartId = startRegion.RegionId
              EndId = endRegion.RegionId
              Locations = locations
            }
        [
            for wp in waypoints do
                match wp.GetName().Value with
                | "Start" | "StartL" | "StartR" ->
                    yield buildPath wp
                | _ ->
                    ()
        ]

    member this.MatchesEndpoints(start, finish) =
        if this.StartId = start && this.EndId = finish then
            Some(lazy this.Locations)
        elif this.StartId = finish && this.EndId = start then
            lazy (
                this.Locations
                |> List.rev
                |> List.map (fun v ->
                    { v with
                        Ori = if v.Ori < 180.0f then v.Ori + 180.0f else v.Ori - 180.0f
                        SpawnSide = v.SpawnSide.Mirrored
                        Role = v.Role.Opposite
                    }))
            |> Some
        else
            None


type DefenseAreaId = DefenseAreaId of int

/// Defense areas contain static anti-air and anti-tank defenses. Defense areas are extracted from influence areas in group Defenses in the strategy mission.
/// Influence areas for anti-air are named "AAA" and "AT" for anti-tank.
type DefenseArea = {
    DefenseAreaId : DefenseAreaId
    Home : RegionId
    /// The orientation is relevant for AT tank canons, so that they can aim at incoming tanks.
    Position : OrientedPosition
    Boundary : Vector2 list
    MaxNumGuns : int
    Role : DefenseSpecialty
}
with
    static member ExtractCentralDefenseAreas(areas : T.MCU_TR_InfluenceArea list, regions : Region list) =
        [
            for area in areas do
                let pos = Vector2.FromPos(area)
                let areaName = area.GetName().Value
                let numGuns =
                    match (Stream.FromString areaName) with
                    | ReLit "AAA-" (ReInt (n, EOF _)) -> n
                    | ReLit "AA-" (ReInt (n, EOF _)) -> n
                    | _ -> 4
                let role =
                    if areaName.StartsWith "AAA" then
                        AntiAirCanon
                    else if areaName.StartsWith "AA" then
                        AntiAirMg
                    else
                        failwithf "Invalid name '%s' for defense area" areaName
                match regions |> List.tryFind(fun region -> pos.IsInConvexPolygon(region.Boundary)) with
                | Some region ->
                    yield {
                        DefenseAreaId = DefenseAreaId(area.GetIndex().Value)
                        Home = region.RegionId
                        Position = { Pos = pos; Rotation = float32(area.GetYOri().Value); Altitude = 0.0f }
                        Boundary = area.GetBoundary().Value |> List.map(Vector2.FromPair)
                        MaxNumGuns = numGuns
                        Role = role
                    }
                | None ->
                    failwithf "Defense area '%s' is not located in any region" areaName
        ]

    static member ExtractFrontLineDefenseAreas(areas : T.MCU_TR_InfluenceArea list, regions : Region list, paths : Path list) =
        [
            for area in areas do
                let pos = Vector2.FromPos(area)
                match regions |> List.tryFind(fun region -> pos.IsInConvexPolygon(region.Boundary)) with
                | Some region ->
                    yield {
                        DefenseAreaId = DefenseAreaId(area.GetIndex().Value)
                        Home = region.RegionId
                        Position = { Pos = pos; Rotation = float32(area.GetYOri().Value); Altitude = 0.0f } 
                        Boundary = area.GetBoundary().Value |> List.map(Vector2.FromPair)
                        MaxNumGuns = 12
                        Role = AntiTank
                    }
                | None ->
                    failwithf "Defense area '%s' is not located in any region" (area.GetName().Value)
        ]

    member private this.LongPositions =
        let dir = Vector2.FromYOri(float this.Position.Rotation)
        this.Boundary
        |> Seq.map (fun v -> Vector2.Dot(v - this.Position.Pos, dir))

    member this.DefensePos =
        let dir = Vector2.FromYOri(float this.Position.Rotation)
        let c = this.LongPositions |> Seq.min
        this.Position.Pos + c * dir

    member this.AttackPos =
        let dir = Vector2.FromYOri(float this.Position.Rotation)
        let c = this.LongPositions |> Seq.max
        this.Position.Pos + c * dir

/// Airfield identifier, uses the name of the fakefield.
type AirfieldId = AirfieldId of string
with
    member this.AirfieldName =
        match this with
        | AirfieldId name -> name

/// Identifies the kind of plane that can be parked at some location.
module ParkedPlaneTypes =
    let (|Fighter|Attacker|Bomber|Other|) (s : string) =
        if s.Contains("bf109") then
            Fighter
        elif s.Contains("bf110") then
            Attacker
        elif s.Contains("ju88") then
            Bomber
        else
            Other


/// Description of an airfield: Its position, the planes that can be parked there, the ammo storage facilities.
type Airfield = {
    AirfieldId : AirfieldId
    Region : RegionId
    Pos : Vector2
    Rotation : float32
    ParkedFighters : OrientedPosition list
    ParkedAttackers : OrientedPosition list
    ParkedBombers : OrientedPosition list
    Storage : StaticGroup list
    Spawn : T.Airfield list // Multiple spawn points. Only one is used in each mission, depending on wind direction.
}
with
    static member AddParkedFighter(airfields : Airfield list, airfield : AirfieldId, pos : OrientedPosition) =
        airfields
        |> List.map (fun af ->
            if af.AirfieldId = airfield then
                { af with ParkedFighters = pos :: af.ParkedFighters
                }
            else
                af
        )

    static member AddParkedAttacker(airfields : Airfield list, airfield : AirfieldId, pos : OrientedPosition) =
        airfields
        |> List.map (fun af ->
            if af.AirfieldId = airfield then
                { af with ParkedAttackers = pos :: af.ParkedAttackers
                }
            else
                af
        )

    static member AddParkedBomber(airfields : Airfield list, airfield : AirfieldId, pos : OrientedPosition) =
        airfields
        |> List.map (fun af ->
            if af.AirfieldId = airfield then
                { af with ParkedBombers = pos :: af.ParkedBombers
                }
            else
                af
        )

    static member AddCaponiers(airfields : Airfield list, airfield : AirfieldId, caponiers : StaticGroup) =
        airfields
        |> List.map (fun af ->
            if af.AirfieldId = airfield && (caponiers.Pos.Pos - af.Pos).Length() < 3000.0f then
                match caponiers.PlaneParkingPositions with
                | None -> af
                | Some positions ->
                    let transform(pos : Vector3) =
                        let angle =
                            (caponiers.Pos.Rotation + pos.Z) % 360.0f
                        let x = (Vector2(pos.X, pos.Y) - positions.RefPos).Rotate(caponiers.Pos.Rotation)
                        { Pos = caponiers.Pos.Pos + x
                          Rotation = angle
                          Altitude = caponiers.Pos.Altitude }
                    let fighters, attackers, bombers =
                        positions.Positions
                        |> List.fold (fun (fighters, attackers, bombers) (pos, sz) ->
                            match sz with
                            | SizeFighter -> transform pos :: fighters, attackers, bombers
                            | SizeAttacker -> fighters, transform pos :: attackers, bombers
                            | SizeBomber -> fighters, attackers, transform pos :: bombers
                        ) (af.ParkedFighters, af.ParkedAttackers, af.ParkedBombers)
                    { af with
                        ParkedFighters = fighters
                        ParkedAttackers = attackers
                        ParkedBombers = bombers
                    }
            else
                af
        )

    static member AddStorage(airfields : Airfield list, airfield : AirfieldId, storage : StaticGroup) =
        airfields
        |> List.map (fun af ->
            if af.AirfieldId = airfield then
                { af with Storage = storage :: af.Storage
                }
            else
                af
        )

    static member inline ExtractAirfields(spawns : T.Airfield list, parkedPlanes : ^Plane list, caponiers : StaticGroup list, storage : T.Block list, regions : Region list, subBlocksSpecs) =
        let airfields =
            spawns
            |> List.groupBy (fun spawn -> spawn.GetName().Value)
            |> List.map (fun (name, spawns) -> spawns)
            |> List.choose(fun spawns ->
                let spawn = spawns.Head
                let pos = Vector2.FromPos(spawn)
                let region =
                    regions
                    |> List.tryFind(fun region -> pos.IsInConvexPolygon(region.Boundary))
                match region with
                | Some region ->
                    Some {
                        AirfieldId = AirfieldId(spawn.GetName().Value)
                        Region = region.RegionId
                        Pos = pos
                        Rotation = float32(spawn.GetYOri().Value)
                        ParkedFighters = []
                        ParkedAttackers = []
                        ParkedBombers = []
                        Storage = []
                        Spawn = spawns
                    }
                | None ->
                    None
            )
        let airfields =
            parkedPlanes
            |> List.fold (fun (airfields : Airfield list) plane ->
                let pos = Vector2.FromPos plane
                let yori = plane |> getYOri |> valueOf |> float32
                let home =
                    airfields
                    |> List.minBy (fun af -> (af.Pos - pos).LengthSquared())
                match (plane |> getModel |> valueOf) with
                | ParkedPlaneTypes.Fighter -> Airfield.AddParkedFighter(airfields, home.AirfieldId, { Pos = pos; Rotation = yori; Altitude = 0.0f })
                | ParkedPlaneTypes.Attacker -> Airfield.AddParkedAttacker(airfields, home.AirfieldId, { Pos = pos; Rotation = yori; Altitude = 0.0f })
                | ParkedPlaneTypes.Bomber -> Airfield.AddParkedBomber(airfields, home.AirfieldId, { Pos = pos; Rotation = yori; Altitude = 0.0f })
                | ParkedPlaneTypes.Other -> airfields
            ) airfields
        let airfields =
            caponiers
            |> List.fold (fun airfields caponiers ->
                let pos = caponiers.Pos.Pos
                let home =
                    airfields
                    |> List.minBy (fun af -> (af.Pos - pos).LengthSquared())
                Airfield.AddCaponiers(airfields, home.AirfieldId, caponiers)
                ) airfields
        let airfields =
            storage
            |> List.choose (fun storage ->
                let blck = StaticGroup.FromBlock storage
                if blck.IsAirfieldStorage subBlocksSpecs then
                    Some blck
                else
                    None)
            |> List.fold (fun (airfields : Airfield list) blck ->
                let pos = blck.Pos.Pos
                let home =
                    airfields
                    |> List.minBy (fun af -> (af.Pos - pos).LengthSquared())
                if (home.Pos - pos).Length() <= 3000.0f then
                    Airfield.AddStorage(airfields, home.AirfieldId, blck)
                else
                    airfields
            ) airfields
        airfields

type SafeZone = {
    Boundary : Vector2 list
    Owner : CoalitionId
}
with
    static member Create(area : T.MCU_TR_InfluenceArea) =
        let owner =
            match CoalitionId.FromCountry (enum(area.GetCountry().Value)) with
            | Some coalition ->
                coalition
            | None ->
                failwith "Safe zone must not be neutral"
        { Boundary = area.GetBoundary().Value |> List.map Vector2.FromPair
          Owner = owner
        }

open FSharp.Configuration

[<Literal>]
let sampleSubBlocksFile = __SOURCE_DIRECTORY__ + @"\..\Config\SubBlocks.yaml"
type SubBlockFile = YamlConfig<sampleSubBlocksFile>

/// Try to find the airfield that is furthest away from any enemy region.
let private tryFindRearAirfield (regions : Region list) (airfields : Airfield list) (coalition : CoalitionId) =
    let regionById =
        regions
        |> List.map (fun region -> region.RegionId, region)
        |> Map.ofList
    let furthest =
        try
            airfields
            |> Seq.filter (fun af -> regionById.[af.Region].InitialOwner = Some coalition)
            |> Seq.maxBy(fun af ->
                let distance =
                    try
                        regions
                        |> Seq.filter(fun region -> region.InitialOwner = Some (coalition.Other))
                        |> Seq.map (fun region -> (region.Position - af.Pos).LengthSquared())
                        |> Seq.min
                    with
                    | _ -> 0.0f
                distance)
            |> fun af -> af.AirfieldId
            |> Some
        with
        | _ -> None
    furthest

/// Packages all description data.
type World = {
    Scenario : string
    Map : string
    PlaneSet : PlaneSet
    Regions : Region list
    Roads : Path list
    Rails : Path list
    SeaWays : Path list
    RiverWays : Path list
    AntiAirDefenses : DefenseArea list
    AntiTankDefenses : DefenseArea list
    Airfields : Airfield list
    RearAirfields : Map<CoalitionId, AirfieldId>
    SafeZones : Map<CoalitionId, SafeZone>
    PlaneProduction : float32<E/H>
    ProductionFactor : float32
    /// Date of the first mission.
    StartDate : System.DateTime
    /// Weather offset: affects how late or early the weather pattern is.
    WeatherDaysOffset : float
    SubBlockSpecs : SubBlockSpec list
    /// Fraction of cargo reserved for bombs in supply flights
    CargoReservedForBombs : float32
    /// Number of planes of each kind the AI will attempt to put at each airfield when planning transfers.
    TransferNumPlaneTarget : int
    /// Target for production: Number of tanks in each frontline region
    TankTargetNumber : int
    /// Max amount of repairs per hour
    RepairSpeed : float32<E/H>
}
with
    static member Create(scenario, planeSet, strategyFile, planeProduction, subBlocksFile : string) =
        let subBlocks = SubBlockFile()
        subBlocks.Load(subBlocksFile)
        let subBlockSpecs =
            subBlocks.Blocks
            |> Seq.map(fun spec -> SubBlockSpec.Create(spec.pattern, spec.sub_blocks, spec.production, spec.storage, spec.is_airfield, spec.durability))
            |> List.ofSeq
        let s = Stream.FromFile strategyFile
        let data = T.GroupData(s)
        let map = data.ListOfOptions.Head.GetGuiMap().Value
        let regions =
            let regions = Region.ExtractRegions(data.GetGroup("Regions").ListOfMCU_TR_InfluenceArea)
            let ammoStorages = List.concat [ data.GetGroup("Ammo").ListOfBlock; data.GetGroup("Storage").ListOfBlock ]
            let factories =
                [ data.GetGroup("Moscow_Big_Cities_Targets").ListOfBlock; data.GetGroup("Factories").ListOfBlock; data.GetGroup("Static").ListOfBlock ]
                |> List.concat
            let parkings =
                data.GetGroup("Tank parks").ListOfMCU_TR_InfluenceArea
            regions
            |> List.map (fun area -> area.AddStorage(ammoStorages, subBlockSpecs))
            |> List.map (fun area -> area.AddProduction(factories, subBlockSpecs))
            |> List.map (fun area -> area.SetParking parkings)
        let roads = Path.ExtractPaths(data.GetGroup("Roads").ListOfMCU_Waypoint, regions)
        let rails = Path.ExtractPaths(data.GetGroup("Trains").ListOfMCU_Waypoint, regions)
        let seaWays = Path.ExtractPaths(data.GetGroup("Sea").ListOfMCU_Waypoint, regions)
        let riverWays = Path.ExtractPaths(data.GetGroup("River").ListOfMCU_Waypoint, regions)
        let defenses = data.GetGroup("Defenses")
        let aaas = defenses.ListOfMCU_TR_InfluenceArea |> List.filter(fun spawn -> spawn.GetName().Value.StartsWith("AA"))
        let antiAirDefenses = DefenseArea.ExtractCentralDefenseAreas(aaas, regions)
        let battlefieldZones = data.GetGroup("Battles")
        let battlefields = battlefieldZones.ListOfMCU_TR_InfluenceArea
        let antiTankDefenses = DefenseArea.ExtractFrontLineDefenseAreas(battlefields, regions, roads)
        let afs = data.GetGroup("Airfield spawns").ListOfAirfield
        let planes = data.GetGroup("Parked planes").ListOfPlane
        let afStorages = data.GetGroup("Airfield storage").ListOfBlock @ data.GetGroup("Static").ListOfBlock
        let airfields =
            let caponiers =
                data.GetGroup("Static").ListOfBlock
                |> List.map StaticGroup.FromBlock
                |> List.filter (fun group -> group.PlaneParkingPositions.IsSome)
            match planes with
            | [] ->
                let staticPlanes = data.GetGroup("Parked planes").ListOfBlock
                Airfield.ExtractAirfields(afs, staticPlanes, caponiers, afStorages, regions, subBlockSpecs)
            | _ :: _->
                Airfield.ExtractAirfields(afs, planes, caponiers, afStorages, regions, subBlockSpecs)
        let rearAirfields =
            [Axis; Allies]
            |> List.choose (fun coalition ->
                match tryFindRearAirfield regions airfields coalition with
                | Some x -> Some(coalition, x)
                | None -> None)
            |> Map.ofList
        let safeZones =
            data.GetGroup("SafeZones").ListOfMCU_TR_InfluenceArea
            |> List.map SafeZone.Create
            |> List.map (fun zone -> zone.Owner, zone)
            |> Map.ofList
        let date =
            let options = List.head data.ListOfOptions
            let h, m, s = options.GetTime().Value
            System.DateTime(options.GetDate().Year, options.GetDate().Month, options.GetDate().Day, h.Value, m.Value, s.Value)
        { Map = map
          Scenario = scenario
          PlaneSet = planeSet
          Regions = regions
          AntiAirDefenses = antiAirDefenses
          AntiTankDefenses = antiTankDefenses
          Airfields = airfields
          RearAirfields = rearAirfields
          SafeZones = safeZones
          PlaneProduction = planeProduction
          ProductionFactor = 1.0f
          StartDate = date
          Roads = roads
          Rails = rails
          SeaWays = seaWays
          RiverWays = riverWays
          WeatherDaysOffset = 0.0
          SubBlockSpecs = subBlockSpecs
          CargoReservedForBombs = 0.2f
          TransferNumPlaneTarget = 8
          TankTargetNumber = 30
          RepairSpeed = 10.0f<E/H>
        }

    member this.GetClosestAirfield(pos : Vector2) =
        this.Airfields
        |> List.minBy(fun af -> (af.Pos - pos).LengthSquared())

    member this.TryGetRegionWhere(pos : Vector2) =
        this.Regions
        |> List.tryFind (fun region ->
            pos.IsInConvexPolygon(region.Boundary))

    member this.GetBattlefield(attacker : RegionId option, defender : RegionId) =
        let attackerPos =
            match attacker with
            | Some attacker ->
                this.Regions
                |> List.find (fun reg -> reg.RegionId = attacker)
                |> fun x -> x.Position
            | None ->
                Vector2.Zero
        let defenderPos =
            this.Regions
            |> List.find (fun reg -> reg.RegionId = defender)
            |> fun x -> x.Position
        // Find battlefield whose orientation best matches the respective location of regions
        let dir = attackerPos - defenderPos
        this.AntiTankDefenses
        |> Seq.filter (fun area -> area.Home = defender)
        |> Seq.maxBy (fun area -> Vector2.Dot(Vector2.FromYOri(float area.Position.Rotation), dir))

    member this.RegionHasAirfields(region : RegionId) =
        this.Airfields
        |> Seq.exists (fun af -> af.Region = region)

open Util

/// Provides fast access to world description data by index.
type WorldFastAccess = {
    GetRegion : RegionId -> Region
    GetAntiAirDefenses : DefenseAreaId -> DefenseArea
    GetAntiTankDefenses : DefenseAreaId -> DefenseArea
    GetAirfield : AirfieldId -> Airfield
    RearRegions : Map<CoalitionId, RegionId>
    GetRegionStorageSubBlocks : RegionId -> int -> int[]
    GetRegionProductionSubBlocks : RegionId -> int -> int[]
    GetAirfieldStorageSubBlocks : AirfieldId -> int -> int[]
    World : World
}
with
    static member Create(world : World) =
        let subBlocks location (buildings : StaticGroup list) idx =
            try
                buildings.[idx].SubBlocks world.SubBlockSpecs
            with
            | _ ->
                logger.Warn(sprintf "Bad damage index %d at %s" idx location)
                [||]

        let getRegion = mkGetStuffFast world.Regions (fun r -> r.RegionId)
        let getAirfield = mkGetStuffFast world.Airfields (fun af -> af.AirfieldId)
        { GetRegion = getRegion
          GetAntiAirDefenses = mkGetStuffFast world.AntiAirDefenses (fun r -> r.DefenseAreaId)
          GetAntiTankDefenses = mkGetStuffFast world.AntiTankDefenses (fun r -> r.DefenseAreaId)
          GetAirfield = getAirfield
          RearRegions = world.RearAirfields |> Map.map (fun _ af -> (getAirfield af).Region)
          GetRegionStorageSubBlocks =
            fun region idx ->
                let location = string region + " (storage)"
                let region = getRegion region
                subBlocks location region.Storage idx
          GetRegionProductionSubBlocks =
            fun region idx ->
                let location = string region + " (production)"
                let region = getRegion region
                subBlocks location region.Production idx
          GetAirfieldStorageSubBlocks =
            fun afId idx ->
                let location = afId.AirfieldName
                let af = getAirfield afId
                subBlocks location af.Storage idx
          World = world
        }

type World
with
    member this.FastAccess = WorldFastAccess.Create(this)

let cannonCost = 50.0f<E>
let heavyMachineGunCost = cannonCost / 4.0f
let lightMachineGunCost = heavyMachineGunCost / (float32 SturmovikMission.Blocks.StaticDefenses.Factory.numLightMachineGunsPerHeavyMachineGun)

type DefenseArea with
    member this.AmmoCost =
        match this.Role with
        | AntiTank | AntiAirCanon -> float32 this.MaxNumGuns * cannonCost
        | AntiAirMg ->
            let numFlak = 0.25f * float32 this.MaxNumGuns
            let numMg = 0.75f * float32 this.MaxNumGuns
            numFlak * cannonCost + numMg * heavyMachineGunCost // OK even for light machine guns, because there are actually four times as meany as MaxNumGuns (each light machine gun counts as 25% of a machine gun)

let bombCost = 100.0f<E> / 1000.0f<K>

type World
with
    member this.RegionAmmoCost(regionId : RegionId) =
        this.AntiAirDefenses @ this.AntiTankDefenses
        |> Seq.sumBy(fun area -> if area.Home = regionId then area.AmmoCost else 0.0f<E>)