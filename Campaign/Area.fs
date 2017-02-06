namespace Campaign

open System.Numerics

module Vectors =
    type Vector2
    with
        member this.IsInConvexPolygon(poly : Vector2 list) =
            match poly with
            | [] -> false
            | first :: _ ->
                let cycled = poly @ [first]
                cycled
                |> List.pairwise
                |> List.forall(fun (v1, v2) ->
                    let v = v2 - v1
                    let w = this - v1
                    let c = v.X * w.Y - v.Y * w.X
                    c >= 0.0f
                )

open Vectors

type T = SturmovikMissionTypes.Provider<"C:\Users\johann\Documents\SturmovikMission-git\data\Sample.mission", "C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.Mission">

type CoallitionId = Axis | Allies

type AreaId = AreaId of string

type Area = {
    AreaId : AreaId
    Boundary : Vector2 list
    Neighbours : AreaId list
}
with
    static member ExtractAreas(areas : T.MCU_TR_InfluenceArea list) =
        let extractOne (area : T.MCU_TR_InfluenceArea) =
            { AreaId = AreaId area.Name.Value
              Boundary = area.Boundary.Value |> List.map(fun coord -> Vector2(coord.Value |> fst |> float32, coord.Value |> snd |> float32))
              Neighbours = []
            }
        let excluded =
            Set [
                "AAA"
                "AT"
                "ARTY"
            ]
        let withBoundaries =
            areas
            |> List.filter (fun area -> not(excluded.Contains(area.Name.Value)))
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
            |> List.map (fun area ->
                area.Boundary
                |> List.mapi(fun i v ->
                    nearestCenters v
                    |> List.map (fun center -> center, (area.AreaId, i, v))
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
                    for area1, i1, v1 in kvp.Value do
                        for area2, i2, v2 in kvp.Value do
                            if area1 <> area2 && (v1 - v2).LengthSquared() < cellRadius2 then
                                yield (area1, i1), area2
                ]
            )
            |> List.concat
            |> Seq.groupBy fst
            |> Seq.map (fun (key, items) -> key, items |> Seq.map snd |> Set.ofSeq)
            |> dict
        let getNeighbours(areaId, i) =
            match neighbours.TryGetValue((areaId, i)) with
            | true, items -> items
            | false, _ -> Set.empty
        let setNeighbours (area : Area) =
            let indices = (area.Boundary |> List.mapi(fun i _ -> i)) @ [0]
            let ngh =
                indices
                |> Seq.pairwise
                |> Seq.map (fun (i, j) ->
                    let s1 = getNeighbours(area.AreaId, i)
                    let s2 = getNeighbours(area.AreaId, j)
                    Set.intersect s1 s2
                    |> List.ofSeq
                )
                |> List.concat
                |> Seq.distinct
                |> List.ofSeq
            { area with Neighbours = ngh }
        withBoundaries
        |> List.map setNeighbours


type Path = {
    StartId : AreaId
    EndId : AreaId
    Locations : Vector2 list
}
with
    static member ExtractPaths(waypoints : T.MCU_Waypoint list, areas : Area list) =
        let waypointsById =
            waypoints
            |> List.map (fun wp -> wp.Index.Value, wp)
            |> dict
        let buildPath(start : T.MCU_Waypoint) =
            let rec work (current : T.MCU_Waypoint) path =
                match current.Name.Value with
                | "End" -> current :: path
                | _ ->
                    match current.Targets.Value with
                    | [next] ->
                        match waypointsById.TryGetValue(next) with
                        | true, next -> work next (current :: path)
                        | false, _ -> failwithf "Failed building path because there is no waypoints with id '%d'" next
                    | [] ->
                        failwithf "Failed to build path because node '%d' has no successor" current.Index.Value
                    | _ :: _ ->
                        failwithf "Failed to build path because node '%d' has too many successors" current.Index.Value
            let path = work start []
            let startArea =
                match areas |> List.tryFind (fun area -> Vector2(float32 start.XPos.Value, float32 start.ZPos.Value).IsInConvexPolygon(area.Boundary)) with
                | None -> failwithf "Failed to build path because start node '%d' is not in an area" start.Index.Value
                | Some x -> x
            let endArea =
                match path with
                | finish :: reversed ->
                    match areas |> List.tryFind (fun area -> Vector2(float32 finish.XPos.Value, float32 finish.ZPos.Value).IsInConvexPolygon(area.Boundary)) with
                    | None -> failwithf "Failed to build path because end node '%d' is not in an area" start.Index.Value
                    | Some x -> x
                | _ ->
                    failwith "Failed to build path because it has no end node"
            let locations =
                path
                |> List.rev
                |> List.map (fun wp -> Vector2(float32 wp.XPos.Value, float32 wp.ZPos.Value))
            { StartId = startArea.AreaId
              EndId = endArea.AreaId
              Locations = locations
            }
        [
            for wp in waypoints do
                if wp.Name.Value = "Start" then
                    yield buildPath wp
        ]

type AreaCapacity = {
    HomeId : AreaId
    AntiAir : float
    Vehicles : float
    Planes : float
    Storage : float
}
with
    static member HeavyTankSize = 1.0
    static member MediumTankSize = 1.0
    static member LightVehicleSize = 1.0
    static member FighterSize = 1.0
    static member AttackSize = 2.0
    static member BomberSize = 3.0
    static member RocketSize = 1.0
    static member LightBombSize = 2.0
    static member MediumBombSize = 5.0
    static member HeavyBombSize = 10.0

type FrontDefenceCapacity = {
    HomeId : AreaId
    NeighbourId : AreaId
    AntiTank : float
    Artillery : float
    AntiAir : float
    StaticTanks : float
}

type FrontDefenceLevels = {
    HomeId : AreaId
    NeighbourId : AreaId
    AntiTank : float
    Artillery : float
    AntiAir : float
    HeavyTanks : float
    MediumTanks : float
}

type AreaLevels = {
    HomeId : AreaId
    AntiAir : float
    HeavyTanks : float
    MediumTanks : float
    LightVehicles : float
    Fighters : float
    Bombers : float
    Attackers : float
    Rockets : float
    LightBombs : float
    MediumBombs : float
    HeavyBombs : float
}

type SupplyTargets =
    | ReinforceAntiAir
    | ReinforceAntiTank
    | ReinforceArtillery
    | SupplyRockets
    | SupplyLightBombs
    | SupplyMediumBombs
    | SupplyHeavyBombs

type SupplyWay =
    | ByRail
    | ByShip
    | ByRoad
    | ByAir

type ActionType =
    | Supply of SupplyTargets * SupplyWay
    | AirPatrol
    | GroundPatrol
    | BombingRaid
    | GroundAttack

type OrderId = OrderId of int

type Order =
    { Id : OrderId
      Source : AreaId
      Destination : AreaId
      Action : ActionType
    }