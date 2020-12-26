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

module Campaign.PlaneModelDb

open Util
open FSharp.Json

open Util.Json

open SturmovikMission.Blocks

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel

type PlaneModelDto =
    {
        Kind : PlaneType
        Name : string
        LogName : string
        Roles : PlaneRole[]
        Coalition : CoalitionId
        ScriptModel : Vehicles.VehicleTypeData
        [<JsonField("Static")>]
        StaticBasename : string
        Cost : float32
        BombCapacity : float32
        CargoCapacity : float32
        Payloads : {| Payload : {| Role : PlaneRole; ModMask : int; Id : int |} |}[]
        EmptyPayload : int
        WeaponModsCosts : {| Mod : int; Cost : float32 |}[]
        WingSpan : float32 option
        MaxRange : float32 option
        CruiseSpeed : float32 option
        MinRunwayLength : float32 option
        CpuCost : float32 option
    }

type Campaign.Common.PlaneModel.PlaneModel
with
    static member FromJson(json : PlaneModelDto) : PlaneModel =
        let payloads =
            json.Payloads
            |> Seq.map (fun payload ->
                payload.Payload.Role,
                (int64 payload.Payload.ModMask, payload.Payload.Id))
            |> List.ofSeq
        let weaponModsCosts =
            json.WeaponModsCosts
            |> Seq.map (fun spec ->spec.Mod, float32 spec.Cost * 1.0f<E>)
            |> List.ofSeq
        let wingSpan =
            json.WingSpan
            |> Option.defaultWith (fun() ->
                match json.Kind with
                | Fighter -> 7.0f
                | Attacker -> 11.0f
                | Bomber | Transport -> 17.0f
            )
        let cruise =
            json.CruiseSpeed
            |> Option.defaultWith (fun () ->
                match json.Name, json.Kind with
                | "me262", _ -> 500.0f
                | _, Fighter -> 400.0f
                | _, Attacker -> 400.0f
                | _, Bomber -> 350.0f
                | _, Transport -> 300.0f
            )
        let minRunway =
            json.MinRunwayLength
            |> Option.defaultWith (fun () ->
                match json.Name with
                | "me262" -> 2.2f
                | _ -> 0.0f)
        let maxRange =
            json.MaxRange
            |> Option.defaultValue 800.0f
        let cpuCost =
            json.CpuCost
            |> Option.defaultWith (fun () ->
                match json.Kind with
                | Fighter -> 100.0f
                | Attacker -> 200.0f
                | Bomber | Transport -> 400.0f
            )
        {
            Kind = json.Kind
            Name = json.Name
            LogName = json.LogName
            Roles = json.Roles |> List.ofArray
            Coalition = json.Coalition
            ScriptModel = json.ScriptModel
            StaticBasename = json.StaticBasename
            Cost = 1.0f<E> * float32 json.Cost
            BombCapacity = 1.0f<K> * float32 json.BombCapacity
            CargoCapacity = 1.0f<K> * float32 json.CargoCapacity
            Payloads = payloads
            EmptyPayload = json.EmptyPayload
            WeaponModsCosts = weaponModsCosts
            WingSpan = 1.0f<M> * wingSpan
            MaxRange = KM * maxRange
            CruiseSpeed = KPH * cruise
            CpuCost = cpuCost
            MinRunwayLength = KM * minRunway
        }

    member this.ToJson() : PlaneModelDto =
        let payloads =
            this.Payloads
            |> List.map (fun (role, (modmask, id)) ->
                {|
                    Payload =
                        {|
                            Role = role
                            ModMask = int modmask
                            Id = id
                        |}
                |}
            )
            |> Array.ofList

        let weaponModsCosts =
            this.WeaponModsCosts
            |> List.map (fun (id, cost) ->
                {|
                    Mod = id
                    Cost = float32 cost
                |}
            )
            |> Array.ofList

        {
            Kind = this.Kind
            Name = this.Name
            LogName = this.LogName
            Roles = this.Roles |> Array.ofList
            Coalition = this.Coalition
            ScriptModel = this.ScriptModel
            StaticBasename = this.StaticBasename
            Cost = float32 this.Cost
            BombCapacity = float32 this.BombCapacity
            CargoCapacity = float32 this.CargoCapacity
            Payloads = payloads
            EmptyPayload = this.EmptyPayload
            WeaponModsCosts = weaponModsCosts
            WingSpan = Some(float32 this.WingSpan)
            MaxRange = Some(float32 this.MaxRange)
            CruiseSpeed = Some(float32 this.CruiseSpeed)
            CpuCost = Some(this.CpuCost)
            MinRunwayLength = Some(float32 this.MinRunwayLength)
        }

    member this.HasRole(role) =
        this.Roles
        |> List.exists ((=) role)


let planeDb =
    let location = System.Reflection.Assembly.GetExecutingAssembly().Location |> System.IO.Path.GetDirectoryName
    let json = System.IO.File.ReadAllText(System.IO.Path.Combine(location, "Config", "PlaneDb.json"))
    let planes = Json.deserializeEx<{| Planes : {| Plane : PlaneModelDto |}[] |}> JsonConfig.IL2Default json
    planes.Planes
    |> Array.map (fun plane -> PlaneModel.FromJson plane.Plane)
    |> List.ofArray

let tryGetPlaneByName name =
    planeDb
    |> List.tryFind (fun plane -> plane.Name = name)

let planeTypeShares(coalition) =
    match coalition with
    | Axis -> [ 0.6f; 0.2f; 0.15f; 0.05f ]
    | Allies -> [ 0.5f; 0.3f; 0.2f; 0.0f ]
    |> List.zip [ Fighter; Attacker; Bomber; Transport ]
    |> Map.ofList
