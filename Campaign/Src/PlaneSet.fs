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

module Campaign.PlaneSet

open BasicTypes
open Campaign.PlaneModel
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks
open System
open FSharp.Configuration
open NLog
open Util
open System.IO

let private logger = LogManager.GetCurrentClassLogger()

[<Literal>]
let private sampleFile = __SOURCE_DIRECTORY__ + @"\..\Config\PlaneSet-Moscow.yaml"
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
    member this.ModFilter =
        match this with
        | One x -> sprintf "%d" x
        | Interval (x, y) -> sprintf "%d..%d" x y

/// <summary>
/// Data associated to a plane. The plane model itself is not included.
/// </summary>
type PlaneData = {
    Cost : float32<E>
    // Amount to remove from individual plane quotas when checking out from the rear base
    RearValueFactor : float32
    AllowedMods : ModRange list
    StaticPlaneIndex : int
}
with
    static member Default =
        { Cost = 500.0f<E>
          RearValueFactor = 1.0f
          AllowedMods = [Interval(0, 99)]
          StaticPlaneIndex = 0 }

    static member TryFromYaml(data : PlaneSetFile.PlaneSet_Type.Planes_Item_Type) =
        let model = data.Model.ToLowerInvariant()
        let plane =
            PlaneModel.AllModels
            |> List.tryFind(fun plane -> model.Contains(plane.PlaneName.ToLowerInvariant()))
        let idx = data.Static
        let factor = float32 data.Factor
        let mods =
            data.Mods
            |> Option.ofObj
            |> Option.map (fun x -> x :> _ seq)
            |> Option.defaultValue (Seq.empty)
            |> Seq.map List.ofSeq
            |> List.ofSeq
        let ranges =
            // If the Mods field is not specified in the file, it will get the default value, which I guess is the empty list.
            // Interpret that as no mod restrictions.
            // This means it's in theory not possible to forbid all mods. In practice, one can allow unassigned mod value, e.g. 99
            match List.map (ModRange.FromList) mods with
            | [] -> PlaneData.Default.AllowedMods
            | x -> x
        plane
        |> Option.map(fun plane -> plane, { PlaneData.Default with StaticPlaneIndex = idx; Cost = plane.Cost; RearValueFactor = factor; AllowedMods = ranges })

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
        try
            let planes =
                data.Planes
                |> Seq.map (fun plane ->
                    match PlaneData.TryFromYaml plane with
                    | Some x -> x
                    | None -> failwithf "Could not find a PlaneModel corresponding to %s" plane.Model)
                |> Map.ofSeq
            for (idx, coalition), models in planes |> Map.toSeq |> Seq.groupBy (fun (model, plane) -> plane.StaticPlaneIndex, model.Coalition) do
                if Seq.length models > 1 then
                    failwithf "Multiple planes in the same coalition '%s' share the same Static index '%d': %s"
                        (string coalition)
                        idx
                        (models |> Seq.map (fun (model, _) -> model.PlaneName) |> String.concat ", ")
            { Name = data.Name
              StartDate = date
              Regions = regions
              Planes = planes }
            |> Ok
        with
        | exc ->
            Error <| sprintf "In plane set %s: %s" data.Name exc.Message

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

/// <summary>
/// Try to find the earliest planeset matching a given region and start date.
/// </summary>
let tryPickPlaneSet (region : Region) (date : DateTime) (planeSets : PlaneSet seq) =
    planeSets
    |> Seq.filter (fun planeSet -> planeSet.Regions |> List.exists ((=) region))
    |> Seq.filter (fun planeSet -> planeSet.StartDate <= date)
    |> try
        Seq.maxBy (fun planeSet -> planeSet.StartDate) >> Some
       with _ -> fun _ -> None

/// <summary>
/// Load all plane sets from a directory. Planeset filenames must start with "PlaneSet-" and have extension ".yaml".
/// Plane sets must include at least one plane for each coalition.
/// </summary>
let loadPlaneSets (planeSetDir : string) =
    seq {
        for file in Directory.EnumerateFiles(planeSetDir, "PlaneSet-*.yaml") do
            let data = PlaneSetFile()
            data.Load(file)
            let planeSet = PlaneSet.FromYaml(data.PlaneSet)
            match planeSet with
            | Ok planeSet ->
                let hasAxis = planeSet.AllModels |> Seq.exists (fun plane -> plane.Coalition = Axis)
                let hasAllies = planeSet.AllModels |> Seq.exists (fun plane -> plane.Coalition = Allies)
                if hasAxis && hasAllies then
                    yield planeSet
                else
                    logger.Warn(sprintf "Skipped plane set %s because one of the coalitions has no planes" file)
            | Error msg ->
                logger.Warn(sprintf "Could not load plane set %s because: '%s'" file msg)
    }