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

module Campaign.PlaneModel

open SturmovikMission.Blocks

open Campaign.BasicTypes
open Util
open System.Numerics
open FSharp.Data

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
      Cost : float32<E>
      BombCapacity : float32<K>
      CargoCapacity : float32<K>
      Payloads : Map<PlaneRole, int64*int>
      BombLoads : (int * float32<K>) list
      SpecialLoadsCosts : (int * float32<E>) list
      EmptyPayload : int
    }
with
    member this.Id = PlaneModelId this.Name

    member this.MaxRange = 800000.0f<M>

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

type PlaneModel
with
    static member FromJson(json : PlaneDbFile.Plane2) =
        let payloads =
            json.Payloads
            |> Seq.map (fun payload ->
                PlaneRole.FromString payload.Payload.Role,
                (int64 payload.Payload.ModMask, payload.Payload.Id))
            |> Map.ofSeq
        let bombloads =
            json.Bombs
            |> Seq.map (fun group ->
                let offsets = group.Repeat.Offsets |> List.ofSeq
                let loads =
                    group.Repeat.Loads
                    |> Seq.map (fun x -> x.Load.Id, 1.0f<K> * float32 x.Load.Weight)
                    |> List.ofSeq
                xTimes offsets loads)
            |> List.concat
        let specials =
            json.Specials
            |> Seq.map (fun group ->
                let offsets = group.Repeat.Offsets |> List.ofSeq
                let loads =
                    group.Repeat.Loads
                    |> Seq.map (fun x -> x.Load.Id, 1.0f<E> * float32 x.Load.Cost)
                    |> List.ofSeq
                xTimes offsets loads)
            |> List.concat

        {
            Kind = PlaneType.FromString json.Kind
            Name = json.Name
            LogName = json.LogName
            Roles = json.Roles |> Seq.map PlaneRole.FromString |> List.ofSeq
            Coalition = CoalitionId.FromString json.Coalition
            ScriptModel = { Script = json.Script; Model = json.Model }
            Cost = 1.0f<E> * float32 json.Cost
            BombCapacity = 1.0f<K> * float32 json.BombCapacity
            CargoCapacity = 1.0f<K> * float32 json.CargoCapacity
            Payloads = payloads
            BombLoads = bombloads
            SpecialLoadsCosts = specials
            EmptyPayload = 0
        }

    member this.ToJson() : PlaneDbFile.Plane2 =
        let bombs =
            this.BombLoads
            |> List.map (fun (id, w) ->
                let load = new PlaneDbFile.Load(PlaneDbFile.Load2(id, decimal w))
                let repeat = PlaneDbFile.Repeat([|0|], [|load|])
                let bomb = PlaneDbFile.Bomb(repeat)
                bomb)
            |> Array.ofList
        let payloads =
            this.Payloads
            |> Map.toSeq
            |> Seq.map (fun (role, (mask, id)) ->
                PlaneDbFile.Payload(PlaneDbFile.Payload2(string role, int32 mask, id)))
            |> Array.ofSeq
        let specials =
            this.SpecialLoadsCosts
            |> Seq.map (fun (id, cost) ->
                let load = new PlaneDbFile.Load3(PlaneDbFile.Load4(id, decimal cost))
                let repeat = PlaneDbFile.Repeat2([|0|], [|load|])
                let bomb = PlaneDbFile.Special(repeat)
                bomb)
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
                decimal this.Cost,
                decimal this.BombCapacity,
                decimal this.CargoCapacity,
                bombs,
                payloads,
                specials,
                this.EmptyPayload
            )
        json

    member this.HasRole(role) = this.Roles |> List.exists ((=) role)

    member this.GetPayLoadCost(payload, bombCost) =
        let bombLoadWeight =
            this.BombLoads
            |> List.tryPick (fun (loadout, weight) -> if loadout = payload then Some weight else None)
            |> Option.defaultValue 0.0f<K>

        let loadoutCost =
            this.SpecialLoadsCosts
            |> List.tryPick (fun (loadout, cost) -> if loadout = payload then Some cost else None)
            |> Option.defaultValue 0.0f<E>

        let cost = bombLoadWeight * bombCost + loadoutCost
        cost

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
