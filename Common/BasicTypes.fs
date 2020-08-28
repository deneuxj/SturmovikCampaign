// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
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

module Campaign.Common.BasicTypes

open System.Numerics
open VectorExtension

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData.CommonMethods

open Util.StringPatterns

[<Measure>]
/// Cost (energy)
type E

[<Measure>]
/// Time
type H

[<Measure>]
/// Mass
type K

[<Measure>]
/// Distance
type M

[<Measure>]
/// Military ground force
type MGF

type CoalitionId = Axis | Allies
with
    /// Try to convert from an MCU coalition value
    static member FromMcuValue(v) =
        match v with
        | Mcu.CoalitionValue.Axis -> Some Axis
        | Mcu.CoalitionValue.Allies -> Some Allies
        | Mcu.CoalitionValue.CentralPowers -> Some Axis
        | Mcu.CoalitionValue.Entente -> Some Allies
        | _ -> None

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

    override this.ToString() =
        match this with
        | Axis -> "Axis"
        | Allies -> "Allies"

    static member FromString(s : string) =
        match s.ToLowerInvariant() with
        | "axis" -> Axis
        | "allies" -> Allies
        | _ -> failwithf "Invalid coalition '%s'" s

type CountryId =
    | Russia
    | UnitedStates
    | GreatBritain
    | Germany
    | Italy
with
    static member All = [
        Russia
        UnitedStates
        GreatBritain
        Germany
        Italy
    ]

    member this.ToMcuValue =
        match this with
        | Russia -> Mcu.CountryValue.Russia
        | UnitedStates -> Mcu.CountryValue.UnitedStates
        | GreatBritain -> Mcu.CountryValue.GreatBritain
        | Germany -> Mcu.CountryValue.Germany
        | Italy -> Mcu.CountryValue.Italy

    static member FromMcuValue (x : Mcu.CountryValue) =
        match x with
        | Mcu.CountryValue.Russia -> Some Russia
        | Mcu.CountryValue.UnitedStates -> Some UnitedStates
        | Mcu.CountryValue.GreatBritain -> Some GreatBritain
        | Mcu.CountryValue.Germany -> Some Germany
        | Mcu.CountryValue.Italy -> Some Italy
        | _ -> None

    static member FromString(s : string) =
        match (s.ToLowerInvariant()) with
        | "russia" -> Some Russia
        | "unitedstates" | "usa" | "us" -> Some UnitedStates
        | "greatbritain" | "uk" -> Some GreatBritain
        | "germany" -> Some Germany
        | "italy" -> Some Italy
        | _ -> None

    member this.CultureInfo =
        match this with
        | Russia -> "ru-RU"
        | UnitedStates -> "en-US"
        | GreatBritain -> "en-GB"
        | Germany -> "de-DE"
        | Italy -> "it-IT"
        |> fun (lc : string) -> System.Globalization.CultureInfo.GetCultureInfo(lc)

/// Airfield identifier, uses the name of the fakefield.
type AirfieldId = AirfieldId of string
with
    member this.AirfieldName =
        match this with
        | AirfieldId name -> name

    override this.ToString() =
        this.AirfieldName

/// Region identifier, from the name of the region.
type RegionId =
    RegionId of string
with
    override this.ToString() =
        let (RegionId s) = this
        s

/// A position on the map and a rotation around the vertical axis.
[<CustomComparison>]
[<CustomEquality>]
type OrientedPosition = {
    Pos : Vector2
    Rotation : float32
    Altitude : float32
}
with
    static member inline FromMission(block) =
        {
            Pos = Vector2.FromPos(block)
            Rotation = getYOri block |> valueOf |> float32
            Altitude = getAlt block |> valueOf |> float32
        }

    member private this.AsTuple =
        (this.Pos.X, this.Pos.Y, this.Rotation, this.Altitude)

    override this.ToString() =
        string this.AsTuple

    override this.Equals(other) =
        match other with
        | :? OrientedPosition as other ->
            this.AsTuple = other.AsTuple
        | _ ->
            false

    override this.GetHashCode() =
        hash this.AsTuple

    interface System.IComparable<OrientedPosition> with
        member this.CompareTo(other): int =
            compare this.AsTuple other.AsTuple

    interface System.IComparable with
        member this.CompareTo(other): int =
            match other with
            | :? OrientedPosition as other ->
                compare this.AsTuple other.AsTuple
            | _ ->
                invalidArg "other" "Must be an OrientedPosition"

type ParkingSpot = {
    Pos : OrientedPosition
    Radius : float32
}

type IRegion =
    abstract RegionId : RegionId
    abstract Boundary : Vector2 list
    abstract Position : Vector2
    abstract Neighbours : RegionId list
    abstract InitialOwner : CoalitionId option

type IRunway =
    abstract SpawnPos : OrientedPosition
    abstract PathToRunway : Vector2 list
    abstract PathOffRunway : Vector2 list
    abstract Start : Vector2
    abstract End : Vector2

type IAirfield =
    abstract AirfieldId : AirfieldId
    abstract Position : Vector2
    abstract Boundary : Vector2 list
    abstract Runways : IRunway list

[<AutoOpen>]
module Extensions =
    type IAirfield with
        /// Pick the wunway that has best alignment against the provided direction
        member this.PickAgainstWind (wind : Vector2) =
            this.Runways
            |> List.maxBy (fun runway ->
                let direction =
                    runway.End - runway.Start
                let direction = direction / direction.Length()
                -Vector2.Dot(direction, wind))