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

module Campaign.PlaneSet

open BasicTypes
open Campaign.PlaneModel
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks
open System
open FSharp.Configuration
open NLog
open Util

let private logger = LogManager.GetCurrentClassLogger()

[<Literal>]
let private sampleFile = __SOURCE_DIRECTORY__ + @"\PlaneSet-Moscow.yaml"
type PlaneSetFile = YamlConfig<sampleFile>

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

/// <summary>
/// Data associated to a plane. The plane model itself is not included.
/// </summary>
type PlaneData = {
    Cost : float32<E>
    AllowedMods : ModRange list
    StaticPlaneIndex : int
}
with
    static member Default =
        { Cost = 500.0f<E>
          AllowedMods = [Interval(0, 99)]
          StaticPlaneIndex = 0 }

    static member TryFromYaml(data : PlaneSetFile.PlaneSet_Type.Planes_Item_Type) =
        let plane =
            PlaneModel.AllModels
            |> List.tryFind(fun plane -> plane.PlaneName = data.Model)
        let idx = data.Static
        plane
        |> Option.map(fun plane -> plane, { PlaneData.Default with StaticPlaneIndex = idx })

/// <summary>
/// Region where a plane set can be used.
/// </summary>
type Region =
    | Stalingrad = 0
    | VelikieLuki = 1
    | Moscow = 2
    | Kuban = 3

let axisStatic =
    [|
        Vehicles.vehicles.GermanStaBf109
        Vehicles.vehicles.GermanStaBf109Net
        Vehicles.vehicles.GermanStaBf109Open
        Vehicles.vehicles.GermanStaBf109e7
        Vehicles.vehicles.GermanStaBf109e7Net
        Vehicles.vehicles.GermanStaBf109e7Open // 5
        Vehicles.vehicles.GermanStaAttacker
        Vehicles.vehicles.GermanStaJu87
        Vehicles.vehicles.GermanStaJu87Net
        Vehicles.vehicles.GermanStaBomber
        Vehicles.vehicles.GermanStaHe111h6 // 10
        Vehicles.vehicles.GermanStaTransport
    |]

let alliesStatic =
    [|
        Vehicles.vehicles.RussianStaI16
        Vehicles.vehicles.RussianStaI16Net
        Vehicles.vehicles.RussianStaLagg3
        Vehicles.vehicles.RussianStaLagg3Net
        Vehicles.vehicles.RussianStaLagg3W1
        Vehicles.vehicles.RussianStaLagg3W2 // 5
        Vehicles.vehicles.RussianStaMig3
        Vehicles.vehicles.RussianStaMig3Net
        Vehicles.vehicles.RussianStaYak1
        Vehicles.vehicles.RussianStaYak1Net
        Vehicles.vehicles.RussianStaYak1Open // 10
        Vehicles.vehicles.RussianStaAttacker
        Vehicles.vehicles.RussianStaBomber
    |]

let mkPlane (plane : PlaneModel, idx) =
    plane,
    { PlaneData.Default with
        Cost = plane.Cost
        StaticPlaneIndex = idx }


/// <summary>
/// A set of planes.
/// </summary>
type PlaneSet = {
    Name : string
    StartDate : DateTime // Date when planes in this set become widely available
    Regions : Region list
    Planes : Map<PlaneModel, PlaneData>
}
with
    static member Default =
        { Name = "default"
          StartDate = DateTime.Parse("1-Jan-1942")
          Regions = [Region.Stalingrad]
          Planes =
            [ (Bf109e7, 3)
              (Bf109f4, 0)
              (Mc202, 5)
              (Bf110e, 6)
              (Ju88a4, 9)
              (Ju87, 7)
              (Ju52, 11)
              (He111h6, 10)
              (I16, 0)
              (Yak1s69, 8)
              (Lagg3s29, 2)
              (P40, 7)
              (IL2M42, 11)
              (Pe2s87, 12)
            ]
            |> List.map mkPlane
            |> Map.ofList
        }

    static member StaticPlaneModels(coalition) =
        match coalition with
        | Axis -> axisStatic
        | Allies -> alliesStatic

    static member FromYaml(data : PlaneSetFile.PlaneSet_Type) =
        let date =
            match DateTime.TryParse data.StartDate with
            | (true, x) ->
                x
            | _ ->
                logger.Warn(sprintf "Date '%s' in planeset '%s' is invalid" data.StartDate data.Name)
                DateTime.Parse("1-Jan-1935")
        let regions : Region list =
            [
                for x in data.Regions do
                    yield
                        try
                            enum x
                        with
                        | _ ->
                            logger.Warn(sprintf "Bad region value in planeset '%s'" data.Name)
                            Region.Stalingrad
            ]
        let planes =
            data.Planes
            |> Seq.choose PlaneData.TryFromYaml
            |> Map.ofSeq
        { Name = data.Name
          StartDate = date
          Regions = regions
          Planes = planes }

    member this.AllModels =
        this.Planes
        |> Map.toSeq
        |> Seq.map fst

    /// <summary>
    /// Get the script and model to use for a plane when it's parked.
    /// </summary>
    member this.StaticPlaneModel(plane : PlaneModel) =
        let index =
            this.Planes.TryFind plane
            |> Option.map (fun data -> data.StaticPlaneIndex)
            |> Option.defaultValue 0
        PlaneSet.StaticPlaneModels(plane.Coalition).[index]

    member this.PlaneCost(plane : PlaneModel) =
        this.Planes.TryFind plane
        |> Option.map (fun data -> data.Cost)
        |> Option.defaultValue 1.0f<E>

    member this.AllPlanesOfType(typ : PlaneType, coalition : CoalitionId) =
        this.AllModels
        |> Seq.filter (fun model -> model.PlaneType = typ && model.Coalition = coalition)
        |> Array.ofSeq

    member this.RandomPlaneOfType(typ : PlaneType, coalition : CoalitionId) =
        this.AllPlanesOfType(typ, coalition)
        |> Array.shuffle (System.Random())
        |> Seq.tryHead

    member this.RandomPlaneWithRole(role : PlaneRole, coalition : CoalitionId) =
        this.AllModels
        |> Seq.filter (fun model -> model.Roles.Contains(role) && model.Coalition = coalition)
        |> Array.ofSeq
        |> Array.shuffle (System.Random())
        |> Seq.tryHead

type PlaneType
with
    member this.Random(planeSet : PlaneSet, coalition) = planeSet.RandomPlaneOfType(this, coalition)