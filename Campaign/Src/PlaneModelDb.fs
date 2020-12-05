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
open System.Numerics
open FSharp.Data

open SturmovikMission.Blocks

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel

[<Literal>]
let private sampleFile = __SOURCE_DIRECTORY__ + @"\..\Config\SamplePlaneDb.json"
type PlaneDbFile = JsonProvider<sampleFile>

let basePlaneCost = 500.0f<E>

/// xTimes [10; 20] [(1, x); (2, y)] -> [(11, x); (12, y); (21, x); (22, y)]
let private xTimes offsets xs =
    [
        for offset in offsets do
            for (n, x) in xs do
                yield (n + offset, x)
    ]

type Campaign.Common.PlaneModel.PlaneModel
with
    static member FromJson(json : PlaneDbFile.Plane2) =
        let payloads =
            json.Payloads
            |> Seq.map (fun payload ->
                PlaneRole.FromString payload.Payload.Role,
                (int64 payload.Payload.ModMask, payload.Payload.Id))
            |> List.ofSeq
        let weaponModsCosts =
            json.WeaponModsCosts
            |> Seq.map (fun spec ->
                spec.Mod, float32 spec.Cost * 1.0f<E>
            )
            |> List.ofSeq

        {
            Kind = PlaneType.FromString json.Kind
            Name = json.Name
            LogName = json.LogName
            Roles = json.Roles |> Seq.map PlaneRole.FromString |> List.ofSeq
            Coalition = CoalitionId.FromString json.Coalition
            ScriptModel = { Script = json.Script; Model = json.Model }
            StaticBasename = json.Static
            Cost = 1.0f<E> * float32 json.Cost
            BombCapacity = 1.0f<K> * float32 json.BombCapacity
            CargoCapacity = 1.0f<K> * float32 json.CargoCapacity
            Payloads = payloads
            EmptyPayload = 0
            WeaponModsCosts = weaponModsCosts
        }

    member this.ToJson() : PlaneDbFile.Plane2 =
        let payloads =
            this.Payloads
            |> Seq.map (fun (role, (mask, id)) ->
                PlaneDbFile.Payload(PlaneDbFile.Payload2(string role, int32 mask, id)))
            |> Array.ofSeq
        let weaponModsCosts =
            this.WeaponModsCosts
            |> Seq.map (fun (modId, cost) ->
                PlaneDbFile.WeaponModsCost(modId, decimal cost))
            |> Array.ofSeq
        let json =
            PlaneDbFile.Plane2(
                string this.Kind,
                this.Name,
                this.LogName,
                this.Roles |> List.map string |> Array.ofList,
                string this.Coalition,
                this.ScriptModel.Script,
                this.ScriptModel.Model,
                this.StaticBasename,
                decimal this.Cost,
                decimal this.BombCapacity,
                decimal this.CargoCapacity,
                payloads,
                this.EmptyPayload,
                weaponModsCosts
            )
        json

    member this.HasRole(role) = this.Roles |> List.exists ((=) role)


let planeDb =
    let location = System.Reflection.Assembly.GetExecutingAssembly().Location |> System.IO.Path.GetDirectoryName
    let file = PlaneDbFile.Load(System.IO.Path.Combine(location, "Config", "PlaneDb.json"))
    file.Planes
    |> Seq.map (fun plane -> PlaneModel.FromJson plane.Plane)
    |> List.ofSeq

let tryGetPlaneByName name =
    planeDb
    |> List.tryFind (fun plane -> plane.Name = name)

let planeTypeShares(coalition) =
    match coalition with
    | Axis -> [ 0.6f; 0.2f; 0.15f; 0.05f ]
    | Allies -> [ 0.5f; 0.3f; 0.2f; 0.0f ]
    |> List.zip [ Fighter; Attacker; Bomber; Transport ]
    |> Map.ofList
