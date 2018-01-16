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
open Campaign.PlaneModel
open SturmovikMission.Blocks.VirtualConvoy.Types

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
}
with
    static member ExtractRegions(regions : T.MCU_TR_InfluenceArea list) =
        let extractOne (region : T.MCU_TR_InfluenceArea) =
            { RegionId = RegionId(region.GetName().Value)
              Position = Vector2.FromPos(region)
              Boundary = region.GetBoundary().Value |> List.map(fun coord -> Vector2.FromPair(coord))
              Neighbours = []
              Storage = []
              Production = []
              Parking = []
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
            |> List.filter (fun block -> block.Storage subBlockSpecs > 0.0f<E> && not (block.IsAirfieldStorage subBlockSpecs) && not (block.SubBlocks(subBlockSpecs).IsEmpty))
        { this with Storage = this.Storage @ storage
        }

    member this.AddProduction(blocks : T.Block list, subBlockSpecs) =
        let factories =
            this.GetStaticBlocks(blocks)
            |> List.filter (fun block -> block.Production(subBlockSpecs, 1.0f) > 0.0f<E/H> && not (block.SubBlocks(subBlockSpecs).IsEmpty))
        { this with Production = this.Production @ factories
        }

    member this.SetParking(areas : T.MCU_TR_InfluenceArea list) =
        let parking =
            areas
            |> List.tryFind (fun area -> Vector2.FromPos(area).IsInConvexPolygon this.Boundary)
            |> Option.map (fun area -> area.GetBoundary().Value |> List.map(fun coord -> Vector2.FromPair(coord)))
            |> Option.defaultVal []
        { this with Parking = parking }

/// Paths link regions to their neighbours. Road and rail convoys travel along those. Those are extracted from waypoints in groups Roads and Trains respectively in the strategy mission.
type Path = {
    StartId : RegionId
    EndId : RegionId
    /// Ordered list of path vertices, composed of a location and an orientation. The orientation is important for properly orienting trains and vehicle columns.
    Locations : (Vector2 * float32 * SpawnSide) list
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
                | "EndL" -> (current, Left) :: path
                | "EndR" -> (current, Right) :: path
                | "End" -> (current, Center) :: path
                | _ ->
                    match current.GetTargets().Value with
                    | [next] ->
                        let spawnSide =
                            match current.GetName().Value with
                            | "StartR" -> Right
                            | "StartL" -> Left
                            | _ -> Center
                        match waypointsById.TryGetValue(next) with
                        | true, next -> work next ((current, spawnSide) :: path)
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
                    match regions |> List.tryFind (fun area -> Vector2.FromPos(fst finish).IsInConvexPolygon(area.Boundary)) with
                    | None -> failwithf "Failed to build path because end node '%d' is not in a region" (start.GetIndex().Value)
                    | Some x -> x
                | _ ->
                    failwith "Failed to build path because it has no end node"
            let locations =
                path
                |> List.rev
                |> List.map (fun (wp, side) -> Vector2.FromPos wp, float32(wp.GetYOri().Value), side)
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
                |> List.map (fun (pos, yori, side) -> pos, (if yori < 180.0f then yori + 180.0f else yori - 180.0f), side.Mirrored))
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


open FSharp.Configuration

[<Literal>]
let sampleSubBlocksFile = __SOURCE_DIRECTORY__ + @"\SubBlocks.yaml"
type SubBlockFile = YamlConfig<sampleSubBlocksFile>

/// Packages all description data.
type World = {
    PlaneSet : PlaneSet
    Regions : Region list
    Roads : Path list
    Rails : Path list
    SeaWays : Path list
    RiverWays : Path list
    AntiAirDefenses : DefenseArea list
    AntiTankDefenses : DefenseArea list
    Airfields : Airfield list
    // Deprecated
    MaxTankNeeds : float32<E>
    PlaneProduction : float32<E/H>
    ProductionFactor : float32
    /// Date of the first mission.
    StartDate : System.DateTime
    /// Weather offset: affects how late or early the weather pattern is.
    WeatherDaysOffset : float
    SubBlockSpecs : SubBlockSpec list
}
with
    static member Create(planeSet, strategyFile, planeProduction, subBlocksFile : string) =
        let subBlocks = SubBlockFile()
        subBlocks.Load(subBlocksFile)
        let subBlockSpecs =
            subBlocks.Blocks
            |> Seq.map(fun spec -> SubBlockSpec.Create(spec.pattern, spec.sub_blocks, spec.production, spec.storage, spec.is_airfield, spec.durability))
            |> List.ofSeq
        let s = Stream.FromFile strategyFile
        let data = T.GroupData(s)
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
        let date =
            let options = List.head data.ListOfOptions
            let h, m, s = options.GetTime().Value
            System.DateTime(options.GetDate().Year, options.GetDate().Month, options.GetDate().Day, h.Value, m.Value, s.Value)
        { PlaneSet = planeSet
          Regions = regions
          AntiAirDefenses = antiAirDefenses
          AntiTankDefenses = antiTankDefenses
          Airfields = airfields
          MaxTankNeeds = 0.0f<E>
          PlaneProduction = planeProduction
          ProductionFactor = 1.0f
          StartDate = date
          Roads = roads
          Rails = rails
          SeaWays = seaWays
          RiverWays = riverWays
          WeatherDaysOffset = 0.0
          SubBlockSpecs = subBlockSpecs
        }

    member this.GetClosestAirfield(pos : Vector2) =
        this.Airfields
        |> List.minBy(fun af -> (af.Pos - pos).LengthSquared())

    /// Fraction of cargo in planes reserved for bombs
    member this.CargoReservedForBombs = 0.2f

let productionFactor (world : World) = world.ProductionFactor

open Util

/// Provides fast access to world description data by index.
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

    /// Number of tanks per region on the frontline
    member this.TankTargetNumber = 30

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
let cargoCost = 0.5f<E/K>
let healLimit = 200.0f<E/H>
