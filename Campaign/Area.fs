namespace Campaign

open System.Numerics

type T = SturmovikMissionTypes.Provider<"..\data\Sample.Mission", "..\data\Blocks\StrategySmall1.Mission">

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
        failwith "TODO"

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