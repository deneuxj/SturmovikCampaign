module Campaign.Targets

open System
open System.Numerics

open WorldDescription
open PlaneModel
open BasicTypes
open Buildings
open Util

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
        match name.Trim().ToLowerInvariant() with
        | Contains "cannon" -> Some Artillery
        | Contains "truck" -> Some Truck
        | Contains "train" -> Some Train
        | Contains "ship" -> Some Ship
        | Contains "battleship" -> Some Battleship
        | Contains "gunboat" -> Some GunBoat
        | Contains "tank" -> Some Tank
        | Contains "car" -> Some ArmoredCar
        | _ -> None

type Target =
    {
        Kind : TargetType
        Owner : CoalitionId option
        Pos : OrientedPosition
    }

type AmmoType = AmmoName of string

type ReturnType =
    CrashedInEnemyTerritory | CrashedInFriendlyTerritory of Vector2 | AtAirfield of AirfieldId
with
    override this.ToString() =
        match this with
        | CrashedInEnemyTerritory -> "crashed in enemy territory"
        | CrashedInFriendlyTerritory -> "crashed in friendly territory"
        | AtAirfield afId -> sprintf "landed at %s" (string afId)

/// The results of a flight by a player, used to build success rates of missions.
type FlightRecord =
    {
        Date : DateTime
        Length : TimeSpan
        Plane : PlaneModelId
        PlaneHealth : float32
        AirKills : int
        Start : AirfieldId
        TargetsDamaged : (TargetType * AmmoType * float32) list
        Return : ReturnType
        PilotHealth : float32
    }
