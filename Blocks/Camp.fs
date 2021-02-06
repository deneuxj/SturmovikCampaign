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

/// A military camp with parked vehicles
module SturmovikMission.Blocks.Camp

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.WhileEnemyClose
open SturmovikMission.Blocks.Spotter

type Camp =
    {
        AntiAirLocations : Vector2 list
        VehicleLocations : Vector2 list
        Start : Mcu.McuTrigger
        All : McuUtil.IMcuGroup
    }
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, rotation : float32, coalition : Mcu.CoalitionValue, location : string, vehicles : McuUtil.IMcuGroup seq) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let lcSubst = Mcu.substLCId <| store.GetIdMapper()
        let group = blocks2Data.GetGroup("Camp")
        let mcus =
            group.CreateMcuList()
            |> List.filter (function :? Mcu.McuWaypoint -> false | _ -> true)

        for mcu in mcus do
            subst mcu
            lcSubst mcu
        // Key nodes
        let nodeEnable = McuUtil.getTriggerByName mcus "ENABLE"
        let nodeDisable = McuUtil.getTriggerByName mcus "DISABLE"
        let nodeStart = McuUtil.getTriggerByName mcus "START"

        // Position all nodes
        let refPos = Vector2.FromMcu nodeEnable.Pos
        for mcu in mcus do
            let rel = Vector2.FromMcu(mcu.Pos) - refPos
            (rel.Rotate(rotation) + pos).AssignTo mcu.Pos
            mcu.Ori.Y <- (mcu.Ori.Y + float rotation) % 360.0

        // Populate vehicle locations
        let vehPositions =
            [|
                for wp in group.ListOfMCU_Waypoint do
                    if wp.GetName().Value = "P" then
                        let v = Vector2.FromPos wp
                        yield (v - refPos).Rotate(rotation) + pos
            |]
        let vehPositions = Util.Array.shuffle (System.Random()) vehPositions

        // Get MCUs representing vehicles, set their position, and connect the enable/disable nodes to the MCUs that have entities (i.e. typically vehicles)
        let allVehMcus =
            [
                for pos, vehMcus in Seq.zip vehPositions vehicles do
                    let subst = Mcu.substId <| store.GetIdMapper()
                    let lcSubst = Mcu.substLCId <| store.GetIdMapper()
                    for mcu in McuUtil.deepContentOf vehMcus do
                        subst mcu
                        lcSubst mcu
                        match mcu with
                        | :? Mcu.HasEntity as vehicle when vehicle.NumberInFormation.IsSome ->
                            Mcu.addTargetLink nodeEnable vehicle.Index
                            Mcu.addTargetLink nodeDisable vehicle.Index
                        | _ -> ()
                        pos.AssignTo(mcu.Pos)
                    yield vehMcus
            ]

        // Create a spotter
        let spotter = Spotter.Create(store, lcStore, pos, coalition, location)
        Mcu.addTargetLink nodeStart spotter.Start.Index

        // Check zones to control trigger the enable and disable nodes, if needed
        let wec =
            match nodeEnable.Targets with
            | [] -> McuUtil.groupFromList []
            | _ ->
                let wec = WhileEnemyClose.Create(true, true, store, pos, coalition, 10000)
                Mcu.addTargetLink wec.WakeUp nodeEnable.Index
                Mcu.addTargetLink wec.Sleep nodeDisable.Index
                Mcu.addTargetLink nodeStart wec.StartMonitoring.Index
                wec.All

        // AA positions
        let antiAirPositions =
            [
                for wp in group.ListOfMCU_Waypoint do
                    if wp.GetName().Value = "AA" then
                        let v = Vector2.FromPos wp
                        yield (v - refPos).Rotate(rotation) + pos
            ]

        {
            AntiAirLocations = antiAirPositions
            VehicleLocations = List.ofArray vehPositions
            Start = nodeStart
            All =
                { new McuUtil.IMcuGroup with
                    member this.Content = mcus
                    member this.LcStrings = []
                    member this.SubGroups = [
                        yield spotter.All
                        yield! allVehMcus
                        yield wec
                    ]
                }
        }
