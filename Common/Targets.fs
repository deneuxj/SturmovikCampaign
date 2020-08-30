// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
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

module Campaign.Common.Targets

open System
open System.Numerics

open Campaign.Common.PlaneModel
open Campaign.Common.BasicTypes
open Campaign.Common.Buildings
open Util.StringPatterns

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

    member this.Description =
        match this with
        | Truck -> "truck"
        | Train -> "train"
        | Ship -> "ship"
        | Battleship -> "battleship"
        | GunBoat -> "gunboat"
        | Artillery -> "artillery"
        | Tank -> "tank"
        | ArmoredCar -> "car"
        | Bridge _ -> "bridge"
        | Building _ -> "building"
        | ParkedPlane (_, plane) -> sprintf "parked %s" (string plane)
        | Air plane -> string plane

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
    | CrashedInEnemyTerritory
    | CrashedInFriendlyTerritory of AirfieldId option
    | AtAirfield of AirfieldId
with
    override this.ToString() =
        match this with
        | CrashedInEnemyTerritory -> "crashed in enemy territory"
        | CrashedInFriendlyTerritory(Some afId) -> sprintf "crashed in friendly territory near %s" afId.AirfieldName
        | CrashedInFriendlyTerritory None -> "crashed in friendly territory, far from all airfields"
        | AtAirfield afId -> sprintf "landed at %s" (string afId)

/// The results of a flight by a player, used to build success rates of missions.
type FlightRecord =
    {
        Date : DateTime
        [<Util.Json.TimeSpanJsonField>]
        Length : TimeSpan
        Plane : PlaneModelId
        PlaneHealth : float32
        AirKills : int
        Start : AirfieldId
        TargetsDamaged : (TargetType * AmmoType * float32) list
        Return : ReturnType
        PilotHealth : float32
    }
