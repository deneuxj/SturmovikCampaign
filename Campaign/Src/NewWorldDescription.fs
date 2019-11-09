// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2019 Johann Deneux <johann.deneux@gmail.com>
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

module Campaign.NewWorldDescription

open System.Numerics
open VectorExtension

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks
open SturmovikMission.Blocks.StaticDefenses.Types
open SturmovikMission.Blocks.BlocksMissionData.CommonMethods
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.VirtualConvoy.Factory
open Util

open Campaign.BasicTypes
open Campaign.PlaneSet
open Campaign.WorldDescription

type BuildingProperties = {
    Model : string
    Script : string
    Boundary : Vector2 list
    SubParts : int list
}
with
    /// Extract BuildingProperties from a block inside a delimiting influence area
    static member FromMission(building : T.Block, boundary : T.MCU_TR_InfluenceArea) =
        let pos = Vector2.FromPos(building)
        let rot = float32(building.GetYOri().Value)
        let vertices =
            boundary.GetBoundary().Value
            |> List.map (fun floats ->
                let x, y = floats.Value
                (Vector2(float32 x, float32 y) - pos).Rotate(-rot))
        let subparts =
            building.GetDamaged().Value
            |> Map.toSeq
            |> Seq.map fst
            |> List.ofSeq
        {
            Model = building.GetModel().Value
            Script = building.GetScript().Value
            Boundary = vertices
            SubParts = subparts
        }

    /// Extract a list of BuildingProperties from a .Mission file
    static member FromFile(path : string) =
        let data = T.GroupData(Stream.FromFile path)
        let blocks = data.ListOfBlock
        let zones = data.ListOfMCU_TR_InfluenceArea
        let zones =
            zones
            |> List.map (fun area ->
                let vertices =
                    area.GetBoundary().Value
                    |> List.map (fun floats ->
                        let x, y = floats.Value
                        Vector2(float32 x, float32 y))
                (fun (v : Vector2) -> v.IsInConvexPolygon(vertices)), area)
        [
            for block in blocks do
                let pos = Vector2.FromPos block
                match zones |> List.tryFind (fun (f, _) -> f pos) with
                | Some (_, data) ->
                    yield BuildingProperties.FromMission(block, data)
                | None ->   ()
        ]

type BuildingInstance = {
    Pos : OrientedPosition
    Properties : BuildingProperties
}

type Region = {
    RegionId : RegionId
    Boundary : Vector2 list
    Neighbours : RegionId list
    InitialOwner : CoalitionId option
    // Production in the region; Typically only the rearmost region should have production.
    // It represents the entry point into the game for supplies, and cannot be affected.
    Production : float32<E/H>
    // Supplies can pass through the region, affected by industry
    FlowCapacity : float32<E/H>
    IndustryBuildings : BuildingInstance list
}

/// A node in the logistics network
type NetworkNode = {
    Id : int
    Pos : Vector2
    Radius : Vector2
    // Bridges, train stations... Affect the flow capacity when damaged
    Facilities : BuildingInstance list
    FlowCapacity : float32<E/H>
    Region : RegionId
    Neighbours : int list 
}

type Runway = {
    SpawnPos : OrientedPosition
    PathToRunway : Vector2 list
    Start : Vector2
    End : Vector2
    Facilities : BuildingInstance list
}

type Airfield = {
    AirfieldId : AirfieldId
    Region : RegionId
    Runways : Runway list
    Facilities : StaticGroup list
}