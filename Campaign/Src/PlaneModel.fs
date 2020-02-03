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
open FSharp.Configuration

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
      Payloads : Map<PlaneRole, int*int>
      BombLoads : (int * float32<K>) list
      SpecialLoadsCosts : (int * float32<E>) list
      EmptyPayload : int
    }
with
    member this.Id = PlaneModelId this.Name

    member this.MaxRange = 100000.0f<M>

[<Literal>]
let private sampleFile = __SOURCE_DIRECTORY__ + @"\..\Config\SamplePlaneDb.yaml"
type PlaneDbFile = YamlConfig<sampleFile>

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
    static member FromYaml(yaml : PlaneDbFile.Planes_Item_Type.Plane_Type) =
        let payloads =
            yaml.Payloads
            |> Seq.map (fun payload ->
                PlaneRole.FromString payload.Payload.Role,
                (payload.Payload.ModMask, payload.Payload.Id))
            |> Map.ofSeq
        let bombloads =
            yaml.Bombs
            |> Seq.map (fun group ->
                let offsets = group.Repeat.Offsets |> List.ofSeq
                let loads =
                    group.Repeat.Loads
                    |> Seq.map (fun x -> x.Load.Id, 1.0f<K> * float32 x.Load.Weight)
                    |> List.ofSeq
                xTimes offsets loads)
            |> List.concat
        let specials =
            yaml.Specials
            |> Seq.map (fun group ->
                let offsets = group.Repeat.Offsets |> List.ofSeq
                let loads =
                    group.Repeat.Loads
                    |> Seq.map (fun x -> x.Load.Id, 1.0f<E> * float32 x.Load.Cost)
                    |> List.ofSeq
                xTimes offsets loads)
            |> List.concat

        {
            Kind = PlaneType.FromString yaml.Kind
            Name = yaml.Name
            LogName = yaml.LogName
            Roles = yaml.Roles |> Seq.map PlaneRole.FromString |> List.ofSeq
            Coalition = CoalitionId.FromString yaml.Coalition
            ScriptModel = { Script = yaml.Script; Model = yaml.Model }
            Cost = 1.0f<E> * float32 yaml.Cost
            BombCapacity = 1.0f<K> * float32 yaml.BombCapacity
            CargoCapacity = 1.0f<K> * float32 yaml.CargoCapacity
            Payloads = payloads
            BombLoads = bombloads
            SpecialLoadsCosts = specials
            EmptyPayload = 0
        }

    member this.ToYaml(yaml : PlaneDbFile.Planes_Item_Type.Plane_Type) =
        let toGenList (x : 'T seq) = System.Collections.Generic.List<_>(x)
        yaml.Kind <- this.Kind.ToString()
        yaml.Name <- this.Name
        yaml.LogName <- this.LogName
        yaml.Roles <- this.Roles |> Seq.map (fun x -> x.ToString()) |> toGenList
        yaml.Coalition <- this.Coalition.ToString()
        yaml.Script <- this.ScriptModel.Script
        yaml.Model <- this.ScriptModel.Model
        yaml.Cost <- float this.Cost
        yaml.BombCapacity <- float this.BombCapacity
        yaml.CargoCapacity <- float this.CargoCapacity
        yaml.Bombs <-
            this.BombLoads
            |> Seq.map (fun (id, w) ->
                let bomb = PlaneDbFile.Planes_Item_Type.Plane_Type.Bombs_Item_Type()
                let load = PlaneDbFile.Planes_Item_Type.Plane_Type.Bombs_Item_Type.Repeat_Type.Loads_Item_Type()
                load.Load.Id <- id
                load.Load.Weight <- float w
                bomb.Repeat.Loads <- toGenList [load]
                bomb.Repeat.Offsets <- toGenList [0]
                bomb)
            |> toGenList
        yaml.Payloads <-
            this.Payloads
            |> Map.toSeq
            |> Seq.map (fun (role, (mask, id)) ->
                let payload = PlaneDbFile.Planes_Item_Type.Plane_Type.Payloads_Item_Type()
                payload.Payload.Role <- role.ToString()
                payload.Payload.ModMask <- mask
                payload.Payload.Id <- id
                payload)
            |> toGenList
        yaml.Specials <-
            this.SpecialLoadsCosts
            |> Seq.map (fun (id, cost) ->
                let bomb = PlaneDbFile.Planes_Item_Type.Plane_Type.Specials_Item_Type()
                let load = PlaneDbFile.Planes_Item_Type.Plane_Type.Specials_Item_Type.Repeat_Type.Loads_Item_Type()
                load.Load.Id <- id
                load.Load.Cost <- float cost
                bomb.Repeat.Loads <- toGenList [load]
                bomb.Repeat.Offsets <- toGenList [0]
                bomb)
            |> toGenList
        yaml.EmptyPayload <- this.EmptyPayload

    member this.HasRole(role) = this.Roles |> List.exists ((=) role)

    member this.GetPayLoadCost(payload, bombCost) =
        let bombLoadWeight =
            this.BombLoads
            |> List.tryPick (fun (loadout, weight) -> if loadout = payload then Some weight else None)
            |> Option.defaultVal 0.0f<K>

        let loadoutCost =
            this.SpecialLoadsCosts
            |> List.tryPick (fun (loadout, cost) -> if loadout = payload then Some cost else None)
            |> Option.defaultValue 0.0f<E>

        let cost = bombLoadWeight * bombCost + loadoutCost
        cost

let planeDb =
    let file = PlaneDbFile()
    file.Planes.Clear()
    let location = System.Reflection.Assembly.GetExecutingAssembly().Location |> System.IO.Path.GetDirectoryName
    file.Load(System.IO.Path.Combine(location, "Config", "PlaneDb.yaml"))
    file.Planes
    |> Seq.map (fun plane -> PlaneModel.FromYaml plane.Plane)
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
