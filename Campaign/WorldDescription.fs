namespace Campaign.WorldDescription

open System.Numerics

open Vector
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider

type CoalitionId = Axis | Allies
with
    member this.ToCountry =
        match this with
        | Axis -> Mcu.CountryValue.Germany
        | Allies -> Mcu.CountryValue.Russia

type OrientedPosition = {
    Pos : Vector2
    Rotation : float32
}

type StaticGroup = {
    Model : string
    Script : string
    Pos : OrientedPosition
}

type RegionId = RegionId of string

type Region = {
    RegionId : RegionId
    Boundary : Vector2 list
    Neighbours : RegionId list
    Storage : StaticGroup list
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

type Path = {
    StartId : RegionId
    EndId : RegionId
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


type DefenseAreaId = DefenseAreaId of int

type DefenseAreaHome =
    | Central of RegionId
    | FrontLine of RegionId * RegionId
with
    member this.Home =
        match this with
        | Central home
        | FrontLine(home, _) -> home

type DefenseArea = {
    DefenseAreaId : DefenseAreaId
    Home : DefenseAreaHome
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


type AirfieldId = AirfieldId of string

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
        | Ju52 -> Vehicles.germanBomber2
        | I16 -> Vehicles.russianFighter1
        | IL2M41 -> Vehicles.russianAttacker1
        | Mig3 -> Vehicles.russianFighter2
        | P40 -> Vehicles.russianFighter3
        | Pe2s35 -> Vehicles.russianBomber1

type Airfield = {
    AirfieldId : AirfieldId
    Region : RegionId
    Pos : Vector2
    Rotation : float32
    ParkedFighters : OrientedPosition list
    ParkedAttackers : OrientedPosition list
    ParkedBombers : OrientedPosition list
    Storage : StaticGroup list
    Spawn : T.Airfield
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
            |> List.map(fun spawn ->
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
                  Spawn = spawn
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

type World = {
    Regions : Region list
    Roads : Path list
    Rails : Path list
    AntiAirDefenses : DefenseArea list
    AntiTankDefenses : DefenseArea list
    Airfields : Airfield list
    StartDate : System.DateTime
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


open Campaign.Util

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
