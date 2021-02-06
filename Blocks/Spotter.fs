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

/// Show a message when an enemy plane flies over a specific area.
module SturmovikMission.Blocks.Spotter

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData

/// Detect enemy planes and show a message
type Spotter = {
    Start : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, coalition : Mcu.CoalitionValue, location : string) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let lcSubst = Mcu.substLCId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Spotter").CreateMcuList()
        for mcu in group do
            subst mcu
            lcSubst mcu
        // Key nodes
        let probe = McuUtil.getTriggerByName group "PROBE" :?> Mcu.McuProximity
        let message =
            match coalition with
            | Mcu.CoalitionValue.Axis -> 
                McuUtil.getTriggerByName group "MESSAGE_AXIS"
            | _
            | Mcu.CoalitionValue.Allies -> 
                McuUtil.getTriggerByName group "MESSAGE_ALLIES"
        let detected = McuUtil.getTriggerByName group "DETECTED"
        let start = McuUtil.getTriggerByName group "START"

        // Position all nodes
        let refPos = Vector2.FromMcu probe.Pos
        let dv = pos - refPos
        for mcu in group do
            let rel = Vector2.FromMcu(mcu.Pos) - refPos
            (rel + pos).AssignTo mcu.Pos
        // Country
        probe.PlaneCoalitions <- [swapCoalition coalition]
        Mcu.addTargetLink detected message.Index
        // Message
        let all =
            { new McuUtil.IMcuGroup with
                member this.Content: Mcu.McuBase list =
                    group
                member this.LcStrings: (int * string) list = 
                    [message.SubtitleLC.Value.LCText, sprintf "Enemies over %s" location]
                member this.SubGroups: IMcuGroup list = 
                    []
            }
        // Return
        { Start = start
          All = all
        }
