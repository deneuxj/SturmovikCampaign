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

module Campaign.Common.PlaneModel

open SturmovikMission.Blocks

open Campaign.Common.BasicTypes

type PlaneType  =
    | Fighter
    | Attacker
    | Bomber
    | Transport
with
    override this.ToString() =
        match this with
        | Fighter -> "Fighter"
        | Attacker -> "Attacker"
        | Bomber -> "Bomber"
        | Transport -> "Transport"

    static member FromString(s) =
        match s with
        | "Fighter" -> Fighter
        | "Attacker" -> Attacker
        | "Bomber" -> Bomber
        | "Transport" -> Transport
        | _ -> failwithf "Invalid plane type '%s'" s

type PlaneRole =
    | Interceptor
    | Patroller
    | GroundAttacker
    | LevelBomber
    | CargoTransporter
with
    static member FromString(s) =
        match s with
        | "Interceptor" -> Interceptor
        | "Patroller" -> Patroller
        | "GroundAttacker" -> GroundAttacker
        | "LevelBomber" -> LevelBomber
        | "CargoTransporter" -> CargoTransporter
        | _ -> failwithf "Invalid plane role '%s'" s

[<Struct>]
type PlaneModelId = PlaneModelId of string
with
    override this.ToString() =
        let (PlaneModelId name) = this
        name

type PlaneModel =
    { Kind : PlaneType
      Name : string
      LogName : string
      Roles : PlaneRole list
      Coalition : CoalitionId
      ScriptModel : Vehicles.VehicleTypeData
      StaticBasename : string
      Cost : float32<E>
      WeaponModsCosts : (int * float32<E>) list
      LastWeaponMod : int
      LastPayload : int
      BombCapacity : float32<K>
      CargoCapacity : float32<K>
      Payloads : (PlaneRole * (int64*int)) list
      EmptyPayload : int
      WingSpan : float32<M>
      MaxRange : float32<M>
      CruiseSpeed : float32<M/H>
      MinRunwayLength : float32<M>
      CpuCost : float32
    }
with
    member this.Id = PlaneModelId this.Name

    member this.PayloadOfRole(role : PlaneRole) =
        this.Payloads
        |> List.tryFind (fun (role2, _) -> role2 = role)
        |> Option.map snd

    member this.StaticScriptModel : Vehicles.VehicleTypeData =
        {
            Model = sprintf @"graphics\blocks\static_%s.mgm" this.StaticBasename
            Script = sprintf @"LuaScripts\WorldObjects\Blocks\static_%s.txt" this.StaticBasename
        }

open Util.RegexActivePatterns
open System.Text.RegularExpressions

/// <summary>
/// Range of plane modifications: Can be a single value or an interval (both bounds included)
/// </summary>
type ModRange =
    | One of int
    | Interval of int * int
with
    static member FromList(xs) =
        match xs with
        | [x] -> One x
        | [x; y] -> Interval(x, y)
        | _ -> failwith "Range must be a singleton or a pair"

    static member TryFromString(s : string) =
        match s.Trim() with
        | AsInt n -> Some(One n)
        | MatchesRegex (Regex(@"(\d+)\.\.(\d+)")) (GroupList [AsInt n1; AsInt n2]) -> Some(Interval(n1, n2))
        | _ -> None

    member this.ModFilter =
        match this with
        | One x -> sprintf "%d" x
        | Interval (x, y) -> sprintf "%d..%d" x y

    static member ModFilters(ranges : ModRange seq) =
        ranges
        |> Seq.map (fun range -> range.ModFilter)
        |> String.concat "/"
