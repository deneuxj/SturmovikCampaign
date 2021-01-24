// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2021 Johann Deneux <johann.deneux@gmail.com>
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

/// Description of a ground unit and the vehicle/artillery/static block corresponding to it.
module Campaign.Common.GroundUnit

open System

open FSharp.Json

open Util.Json
open SturmovikMission.Blocks

type GroundRole =
    | LandLight
    | SearchLight
    | Flak
    | AntiAir
    | AntiTank
    | AntiAirMachineGun
    | MachineGun
    | Artillery
    | Support
    | Command

[<Struct>]
type GroundUnitId = GroundUnitId of string
with
    override this.ToString() =
        let (GroundUnitId name) = this
        name

/// Properties of a ground unit
type GroundUnit =
    { Name : string
      LogName : string
      Roles : GroundRole list
      IsMobile : bool
      DynamicScriptModel : Vehicles.VehicleTypeData option
      StaticScriptModel : Vehicles.VehicleTypeData option
      Durability : int
    }

type GroundUnitIO =
    { name : string
      log_name : string
      roles : GroundRole list option
      is_mobile : bool option
      dyn_script : string option
      dyn_model : string option
      static_script : string option
      static_model : string option
      durability : int option
    }
with
    /// Convert to a GroundUnit
    member this.AsGroundUnit =
        {
            Name = this.name
            LogName = this.log_name
            Roles = this.roles |> Option.defaultValue []
            IsMobile = defaultArg this.is_mobile false
            DynamicScriptModel =
                (this.dyn_model, this.dyn_script)
                ||> Option.map2 (fun model script -> { Script = script; Model = model })
            StaticScriptModel =
                (this.static_model, this.static_script)
                ||> Option.map2 (fun model script -> { Script = script; Model = model })
            Durability = defaultArg this.durability 1250
        }

type GroundUnit with
    /// Convert to a Json-friendly record.
    member this.AsIO =
        {
            name = this.Name
            log_name = this.LogName
            roles = Some this.Roles
            is_mobile = Some this.IsMobile
            dyn_script = this.DynamicScriptModel |> Option.map (fun x -> x.Script)
            dyn_model = this.DynamicScriptModel |> Option.map (fun x -> x.Model)
            static_script = this.StaticScriptModel |> Option.map (fun x -> x.Script)
            static_model = this.StaticScriptModel |> Option.map (fun x -> x.Model)
            durability = Some this.Durability
        }

/// Load a db of ground units from a file in json format
let loadGroundUnitsDb (path : string) =
    let json = IO.File.ReadAllText(path)
    Json.deserializeEx JsonConfig.IL2Default json
    |> Array.map (fun (io : GroundUnitIO) -> io.AsGroundUnit)

/// Save a db of ground units to a file in json format
let saveGroundUnitsDb (path : string) (db : GroundUnit[]) =
    let json =
        db
        |> Array.map (fun x -> x.AsIO)
        |> Json.serializeEx JsonConfig.IL2Default
    IO.File.WriteAllText(path, json)