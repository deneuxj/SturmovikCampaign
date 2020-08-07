module Campaign.Targets

open System
open System.Numerics

open WorldDescription
open PlaneModel
open BasicTypes
open Buildings

type TargetType =
    | Truck | Train | Ship | Battleship | GunBoat | Artillery | Tank | ArmoredCar
    | Bridge of BuildingInstanceId * int
    | Building of BuildingInstanceId * int
    | ParkedPlane of AirfieldId * PlaneModelId
    | Air of PlaneModelId
with
    member this.GroundForceValue =
        match this with
        | Battleship -> 100.0f<MGF>
        | GunBoat -> 15.0f<MGF>
        | Artillery -> 10.0f<MGF>
        | Tank -> 25.0f<MGF>
        | ArmoredCar -> 5.0f<MGF>
        | _ -> 0.0f<MGF>

module ActivePatterns =
    let (|GroundForceTarget|_|) (kind : TargetType) =
        let value = kind.GroundForceValue
        if value > 0.0f<MGF> then
            Some value
        else None

    /// Match the name of an object from the log with a target type.
    // This is dependent on the MCUs in the mission and their naming.
    let (|TargetTypeByName|_|) (name : string) =
        match name with
        | "CANNON" -> Some Artillery
        | _ -> None

type Target =
    {
        Kind : TargetType
        Owner : CoalitionId option
        Pos : OrientedPosition
    }

type AmmoType =
    Rocket | Bullets | Bomb

type ReturnType =
    CrashedInEnemyTerritory | CrashedInFriendlyTerritory of Vector2 | AtAirfield of AirfieldId

/// The results of a flight by a player, used to build success rates of missions.
type FlightRecord =
    {
        Date : DateTime
        Length : TimeSpan
        Plane : PlaneModel
        Start : AirfieldId
        TargetsDamaged : (Target * AmmoType * float32) list
        Return : ReturnType
    }
