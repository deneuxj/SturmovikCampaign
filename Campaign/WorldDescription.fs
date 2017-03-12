module Campaign.WorldDescription

open System.Numerics

open Vector
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider

[<Measure>]
/// Production output (energy)
type E

[<Measure>]
/// Mass
type M

[<Measure>]
/// Time
type H

type CoalitionId = Axis | Allies
with
    /// <summary>
    /// Convert to a numerical country value suitable for use in mission files.
    /// </summary>
    member this.ToCountry =
        match this with
        | Axis -> Mcu.CountryValue.Germany
        | Allies -> Mcu.CountryValue.Russia

    /// <summary>
    /// Convert to a numerical coalition value suitable for use in mission files.
    /// </summary>
    member this.ToCoalition =
        match this with
        | Axis -> Mcu.CoalitionValue.Axis
        | Allies -> Mcu.CoalitionValue.Allies

    /// <summary>
    /// Return the opposite coalition.
    /// </summary>
    member this.Other =
        match this with
        | Axis -> Allies
        | Allies -> Axis

/// A position on the map and a rotation around the vertical axis.
type OrientedPosition = {
    Pos : Vector2
    Rotation : float32
}

/// A group of buildings or some other static objects.
type StaticGroup = {
    Model : string
    Script : string
    Pos : OrientedPosition
}

type RegionId = RegionId of string

/// All items in a region are under the control of the owner of the region.
/// Regions are built from influence areas in the group "Regions" in the strategy mission file.
type Region = {
    RegionId : RegionId
    Boundary : Vector2 list
    Neighbours : RegionId list
    /// Storage controls the amount of static anti-air and anti-tank units deployed in a region.
    Storage : StaticGroup list
    /// Regions with factories get new planes, vehicles and ammo (stored in region storage and in airfield storage).
    Production : StaticGroup list
}
with
    static member ExtractRegions(regions : T.MCU_TR_InfluenceArea list) =
        let extractOne (region : T.MCU_TR_InfluenceArea) =
            { RegionId = RegionId(region.GetName().Value)
              Boundary = region.GetBoundary().Value |> List.map(fun coord -> Vector2.FromPair(coord))
              Neighbours = []
              Storage = []
              Production = []
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

    member private this.GetStaticBlocks(blocks : T.Block list) =
        blocks
        |> List.filter (fun block -> Vector2.FromPos(block).IsInConvexPolygon(this.Boundary))
        |> List.map (fun block ->
            { Model = block.GetModel().Value
              Script = block.GetScript().Value
              Pos = { Pos = Vector2.FromPos block
                      Rotation = float32(block.GetYOri().Value) }
            }
        )

    member this.AddStorage(blocks : T.Block list) =
        let storage = this.GetStaticBlocks(blocks)
        { this with Storage = this.Storage @ storage
        }

    member this.AddProduction(blocks : T.Block list) =
        let factories = this.GetStaticBlocks(blocks)
        { this with Production = this.Production @ factories
        }

/// Paths link regions to their neighbours. Road and rail convoys travel along those. Those are extracted from waypoints in groups Roads and Trains respectively in the strategy mission.
type Path = {
    StartId : RegionId
    EndId : RegionId
    /// Ordered list of path vertices, composed of a location and an orientation. The orientation is important for properly orienting trains and vehicle columns.
    Locations : (Vector2 * float32) list
}
with
    static member ExtractPaths(waypoints : T.MCU_Waypoint list, regions : Region list) =
        let waypointsById =
            waypoints
            |> List.map (fun wp -> wp.GetIndex().Value, wp)
            |> dict
        let buildPath(start : T.MCU_Waypoint) =
            let rec work (current : T.MCU_Waypoint) path =
                match current.GetName().Value with
                | "End" -> current :: path
                | _ ->
                    match current.GetTargets().Value with
                    | [next] ->
                        match waypointsById.TryGetValue(next) with
                        | true, next -> work next (current :: path)
                        | false, _ -> failwithf "Failed building path because there is no waypoints with id '%d'" next
                    | [] ->
                        failwithf "Failed to build path because node '%d' has no successor" (current.GetIndex().Value)
                    | _ :: _ ->
                        failwithf "Failed to build path because node '%d' has too many successors" (current.GetIndex().Value)
            let path = work start []
            let startRegion =
                match regions |> List.tryFind (fun area -> Vector2.FromPos(start).IsInConvexPolygon(area.Boundary)) with
                | None -> failwithf "Failed to build path because start node '%d' is not in a region" (start.GetIndex().Value)
                | Some x -> x
            let endRegion =
                match path with
                | finish :: reversed ->
                    match regions |> List.tryFind (fun area -> Vector2.FromPos(finish).IsInConvexPolygon(area.Boundary)) with
                    | None -> failwithf "Failed to build path because end node '%d' is not in a region" (start.GetIndex().Value)
                    | Some x -> x
                | _ ->
                    failwith "Failed to build path because it has no end node"
            let locations =
                path
                |> List.rev
                |> List.map (fun wp -> Vector2.FromPos wp, float32(wp.GetYOri().Value))
            { StartId = startRegion.RegionId
              EndId = endRegion.RegionId
              Locations = locations
            }
        [
            for wp in waypoints do
                if wp.GetName().Value = "Start" then
                    yield buildPath wp
        ]

    member this.MatchesEndpoints(start, finish) =
        if this.StartId = start && this.EndId = finish then
            Some this.Locations
        elif this.StartId = finish && this.EndId = start then
            this.Locations
            |> List.rev
            |> List.map (fun (pos, yori) -> pos, if yori < 180.0f then yori + 180.0f else yori - 180.0f)
            |> Some
        else
            None


type DefenseAreaId = DefenseAreaId of int

/// A defense area can be tied to the region itself, or the border with another region.
type DefenseAreaHome =
    | Central of RegionId
    | FrontLine of RegionId * RegionId
with
    member this.Home =
        match this with
        | Central home
        | FrontLine(home, _) -> home

/// Defense areas contain static anti-air and anti-tank defenses. Defense areas are extracted from influence areas in group Defenses in the strategy mission.
/// Influence areas for anti-air are named "AAA" and "AT" for anti-tank.
type DefenseArea = {
    DefenseAreaId : DefenseAreaId
    Home : DefenseAreaHome
    /// The orientation is relevant for AT tank canons, so that they can aim at incoming tanks.
    Position : OrientedPosition
    Boundary : Vector2 list
}
with
    static member ExtractCentralDefenseAreas(areas : T.MCU_TR_InfluenceArea list, regions : Region list) =
        [
            for area in areas do
                let pos = Vector2.FromPos(area)
                match regions |> List.tryFind(fun region -> pos.IsInConvexPolygon(region.Boundary)) with
                | Some region ->
                    yield {
                        DefenseAreaId = DefenseAreaId(area.GetIndex().Value)
                        Home = Central region.RegionId
                        Position = { Pos = pos; Rotation = float32(area.GetYOri().Value) }
                        Boundary = area.GetBoundary().Value |> List.map(Vector2.FromPair)
                    }
                | None ->
                    failwithf "Defense area '%s' is not located in any region" (area.GetName().Value)
        ]

    static member ExtractFrontLineDefenseAreas(areas : T.MCU_TR_InfluenceArea list, regions : Region list, paths : Path list) =
        [
            for area in areas do
                let pos = Vector2.FromPos(area)
                match regions |> List.tryFind(fun region -> pos.IsInConvexPolygon(region.Boundary)) with
                | Some region ->
                    let outgoing =
                        paths
                        |> List.filter (fun path -> path.StartId = region.RegionId)
                        |> List.map (fun path -> path, path.EndId)
                    let incoming =
                        paths
                        |> List.filter (fun path -> path.EndId = region.RegionId)
                        |> List.map (fun path -> path, path.StartId)
                    let toNeighbours = outgoing @ incoming
                    let other =
                        try
                            toNeighbours
                            |> List.minBy(fun (path, id) -> pos.DistanceFromPath(path.Locations |> List.map fst))
                        with
                        | _ -> failwithf "Failed to find closest path to defense area '%d'" (area.GetIndex().Value)
                        |> snd
                    yield {
                        DefenseAreaId = DefenseAreaId(area.GetIndex().Value)
                        Home = FrontLine(region.RegionId, other)
                        Position = { Pos = pos; Rotation = float32(area.GetYOri().Value) } 
                        Boundary = area.GetBoundary().Value |> List.map(Vector2.FromPair)
                    }
                | None ->
                    failwithf "Defense area '%s' is not located in any region" (area.GetName().Value)
        ]

/// Airfield identifier, uses the name of the fakefield.
type AirfieldId = AirfieldId of string

/// Identifies the kind of plane that can be parked at some location.
module PlaneTypes =
    let (|Fighter|Attacker|Bomber|Other|) (s : string) =
        if s.Contains("bf109") then
            Fighter
        elif s.Contains("bf110") then
            Attacker
        elif s.Contains("ju88") then
            Bomber
        else
            Other

open PlaneTypes
open SturmovikMission.Blocks

/// Various kind of planes used in the 1941/42 Moscow theater
type PlaneModel =
    | Bf109e7
    | Bf109f2
    | Mc202
    | Bf110e
    | Ju88a4
    | Ju52
    | I16
    | IL2M41
    | Mig3
    | P40
    | Pe2s35
with
    member this.ScriptModel =
        match this with
        | Bf109e7 -> Vehicles.germanFighter1
        | Bf109f2 -> Vehicles.germanFighter2
        | Mc202 -> Vehicles.germanFighter3
        | Bf110e -> Vehicles.germanAttacker1
        | Ju88a4 -> Vehicles.germanBomber1
        | Ju52 -> Vehicles.germanTransport
        | I16 -> Vehicles.russianFighter1
        | IL2M41 -> Vehicles.russianAttacker1
        | Mig3 -> Vehicles.russianFighter2
        | P40 -> Vehicles.russianFighter3
        | Pe2s35 -> Vehicles.russianBomber1
    
    member this.StaticScriptModel =
        match this with
        | Bf109e7 -> Vehicles.germanStaFighter1
        | Bf109f2 -> Vehicles.germanStaFighter2
        | Mc202 -> Vehicles.germanStaFighter3
        | Bf110e -> Vehicles.germanStaAttacker
        | Ju88a4 -> Vehicles.germanStaBomber
        | Ju52 -> Vehicles.germanStaTransport
        | I16 -> Vehicles.russianStaFighter1
        | IL2M41 -> Vehicles.russianStaAttacker
        | Mig3 -> Vehicles.russianStaFighter2
        | P40 -> Vehicles.russianStaFighter3
        | Pe2s35 -> Vehicles.russianStaBomber

    member this.Cost =
        match this with
        | Bf109e7 -> 300.0f<E>
        | Bf109f2 -> 500.0f<E>
        | Mc202 -> 400.0f<E>
        | Bf110e -> 750.0f<E>
        | Ju88a4 -> 1000.0f<E>
        | Ju52 -> 1000.0f<E>
        | I16 -> 300.0f<E>
        | IL2M41 -> 600.0f<E>
        | Mig3 -> 500.0f<E>
        | P40 -> 300.0f<E>
        | Pe2s35 -> 750.0f<E>

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

    static member AddStorage(airfields : Airfield list, airfield : AirfieldId, storage : StaticGroup) =
        airfields
        |> List.map (fun af ->
            if af.AirfieldId = airfield then
                { af with Storage = storage :: af.Storage
                }
            else
                af
        )

    static member ExtractAirfields(spawns : T.Airfield list, parkedPlanes : T.Plane list, storage : T.Block list, regions : Region list) =
        let airfields =
            spawns
            |> List.groupBy (fun spawn -> spawn.GetName().Value)
            |> List.map (fun (name, spawns) -> spawns)
            |> List.map(fun spawns ->
                let spawn = spawns.Head
                let pos = Vector2.FromPos(spawn)
                let region =
                    try
                        regions
                        |> List.find(fun region -> pos.IsInConvexPolygon(region.Boundary))
                    with
                    | _ -> failwithf "Airfield '%s' is not in any region" (spawn.GetName().Value)
                { AirfieldId = AirfieldId(spawn.GetName().Value)
                  Region = region.RegionId
                  Pos = pos
                  Rotation = float32(spawn.GetYOri().Value)
                  ParkedFighters = []
                  ParkedAttackers = []
                  ParkedBombers = []
                  Storage = []
                  Spawn = spawns
                }
            )
        let airfields =
            parkedPlanes
            |> List.fold (fun (airfields : Airfield list) plane ->
                let pos = Vector2.FromPos plane
                let home =
                    airfields
                    |> List.minBy (fun af -> (af.Pos - pos).LengthSquared())
                match plane.GetModel().Value with
                | Fighter -> Airfield.AddParkedFighter(airfields, home.AirfieldId, { Pos = pos; Rotation = float32(plane.GetYOri().Value) })
                | Attacker -> Airfield.AddParkedAttacker(airfields, home.AirfieldId, { Pos = pos; Rotation = float32(plane.GetYOri().Value) })
                | Bomber -> Airfield.AddParkedBomber(airfields, home.AirfieldId, { Pos = pos; Rotation = float32(plane.GetYOri().Value) })
                | Other -> airfields
            ) airfields
        let airfields =
            storage
            |> List.fold (fun (airfields : Airfield list) group ->
                let pos = Vector2.FromPos group
                let home =
                    airfields
                    |> List.minBy (fun af -> (af.Pos - pos).LengthSquared())
                Airfield.AddStorage(airfields, home.AirfieldId, ( { Model = group.GetModel().Value; Script = group.GetScript().Value; Pos = { Pos = pos; Rotation = float32(group.GetYOri().Value) } }))
            ) airfields
        airfields


open SturmovikMission.DataProvider.Parsing

/// Packages all description data.
type World = {
    Regions : Region list
    Roads : Path list
    Rails : Path list
    AntiAirDefenses : DefenseArea list
    AntiTankDefenses : DefenseArea list
    Airfields : Airfield list
    /// Date of the first mission.
    StartDate : System.DateTime
    /// Weather offset: affects how late or early the weather pattern is.
    WeatherDaysOffset : float
}
with
    static member Create(strategyFile) =
        let s = Stream.FromFile strategyFile
        let data = T.GroupData(s)
        let regions =
            let regions = Region.ExtractRegions(data.GetGroup("Regions").ListOfMCU_TR_InfluenceArea)
            let ammoStorages = data.GetGroup("Ammo").ListOfBlock
            let factories =
                data.GetGroup("Moscow_Big_Cities_Targets").ListOfBlock
                |> List.filter(fun block -> block.GetLinkTrId().Value >= 1)
            regions
            |> List.map (fun area -> area.AddStorage ammoStorages)
            |> List.map (fun area -> area.AddProduction factories)
        let roads = Path.ExtractPaths(data.GetGroup("Roads").ListOfMCU_Waypoint, regions)
        let rails = Path.ExtractPaths(data.GetGroup("Trains").ListOfMCU_Waypoint, regions)
        let defenses = data.GetGroup("Defenses")
        let aaas = defenses.ListOfMCU_TR_InfluenceArea |> List.filter(fun spawn -> spawn.GetName().Value = "AAA")
        let antiAirDefenses = DefenseArea.ExtractCentralDefenseAreas(aaas, regions)
        let ats = defenses.ListOfMCU_TR_InfluenceArea |> List.filter(fun spawn -> spawn.GetName().Value = "AT")
        let antiTankDefenses = DefenseArea.ExtractFrontLineDefenseAreas(ats, regions, roads)
        let afs = data.GetGroup("Airfield spawns").ListOfAirfield
        let planes = data.GetGroup("Parked planes").ListOfPlane
        let afStorages = data.GetGroup("Airfield storage").ListOfBlock
        let airfields = Airfield.ExtractAirfields(afs, planes, afStorages, regions)
        let date =
            let options = List.head data.ListOfOptions
            let h, m, s = options.GetTime().Value
            System.DateTime(options.GetDate().Year, options.GetDate().Month, options.GetDate().Day, h.Value, m.Value, s.Value)
        { Regions = regions
          AntiAirDefenses = antiAirDefenses
          AntiTankDefenses = antiTankDefenses
          Airfields = airfields
          StartDate = date
          Roads = roads
          Rails = rails
          WeatherDaysOffset = 0.0
        }, data.ListOfBlock, data.ListOfBridge, List.head data.ListOfOptions

    member this.GetClosestAirfield(pos : Vector2) =
        this.Airfields
        |> List.minBy(fun af -> (af.Pos - pos).LengthSquared())


open Campaign.Util

/// Provides fast accesst to world description data by index.
type WorldFastAccess = {
    GetRegion : RegionId -> Region
    GetAntiAirDefenses : DefenseAreaId -> DefenseArea
    GetAntiTankDefenses : DefenseAreaId -> DefenseArea
    GetAirfield : AirfieldId -> Airfield
}
with
    static member Create(world : World) =
        { GetRegion = mkGetStuffFast world.Regions (fun r -> r.RegionId)
          GetAntiAirDefenses = mkGetStuffFast world.AntiAirDefenses (fun r -> r.DefenseAreaId)
          GetAntiTankDefenses = mkGetStuffFast world.AntiTankDefenses (fun r -> r.DefenseAreaId)
          GetAirfield = mkGetStuffFast world.Airfields (fun af -> af.AirfieldId)
        }

type World
with
    member this.FastAccess = WorldFastAccess.Create(this)

let shellWeight = 5.0f<M>

let shellCost = 1.0f<E>

let shellsPerCanon = 20.0f

let rocketWeight = 20.0f<M>

let getWeightCapacityPerBuilding (model : string) = 5000.0f<M>

let getEnergyHealthPerBuilding (model : string) = getWeightCapacityPerBuilding model * 1.0f<E/M>

let getShellsPerBuilding (model : string) = (getWeightCapacityPerBuilding model) / shellWeight

let getDurabilityForBuilding (model : string) =
    match model with
    | Contains "arf_net" -> 1000
    | Contains "arf_dugout" -> 15000
    | Contains "arf_barak" -> 10000
    | Contains "arf_hangar" -> 10000
    | Contains "industrial" -> 10000
    | Contains "static_" -> 2500
    | _ -> 10000

let getProductionPerBuilding (model : string) =
    match model with
    | Contains "industrial" -> 5.0f<E/H>
    | _ -> 1.0f<E/H>
