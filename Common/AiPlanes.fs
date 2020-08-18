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

module Campaign.Common.AiPlanes

open System.Numerics
open VectorExtension

open SturmovikMission.Blocks.Patrol
open SturmovikMission.Blocks.GroundAttack
open SturmovikMission.DataProvider
open SturmovikMission.Blocks.IconDisplay

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel
open Util
open SturmovikMission.Blocks

type AiPatrol =
    { Plane : PlaneModel
      NumPlanes : int
      HomeAirfield : AirfieldId
      Country : CountryId
      Pos : Vector2
      Altitude : float32
      ProtectedRegion : RegionId option
      Role : PlaneRole
      PlaneReserve : int // Max number of planes that can be use for that patrol
    }
with
    member this.Coalition = this.Country.Coalition

    member this.ToPatrolBlock(store, lcStore) =
        let blocks =
            [
                for i in 1..this.NumPlanes do
                    let block = Patrol.Create(store, lcStore, this.Pos + (float32 i) * Vector2(500.0f, 500.0f), this.Altitude + 250.0f * (float32 i), this.Coalition.ToCoalition)
                    let modmask, payload = this.Plane.Payloads.[this.Role]
                    block.Plane.Country <- Some this.Country.ToMcuValue
                    this.Plane.ScriptModel.AssignTo(block.Plane)
                    block.Plane.PayloadId <- Some payload
                    block.Plane.WMMask <- Some modmask
                    block.Plane.Name <- sprintf "PTL-%s-%s" this.Plane.Name this.HomeAirfield.AirfieldName
                    yield block
            ]
        let conjKilled =
            [|
                for i in 1..this.NumPlanes - 1 do
                    let thisKilled = SturmovikMission.Blocks.Conjunction.Conjunction.Create(store, this.Pos + (float32 i) * Vector2(200.0f, 200.0f))
                    Mcu.addTargetLink blocks.[i].Killed thisKilled.SetA.Index
                    Mcu.addTargetLink blocks.[i].Spawned thisKilled.ClearA.Index
                    yield thisKilled
            |]
        for prev, curr in Seq.pairwise conjKilled do
            Mcu.addTargetLink prev.AllTrue curr.SetB.Index
            Mcu.addTargetLink prev.SomeFalse curr.ClearB.Index
        match conjKilled with
        | [||] -> ()
        | _ ->
            let first = conjKilled.[0]
            Mcu.addTargetLink blocks.[0].Killed first.SetB.Index
            Mcu.addTargetLink blocks.[0].Spawned first.ClearB.Index
        let allKilled, someSpawned =
            match conjKilled with
            | [||] ->
                blocks.[0].Killed, blocks.[0].Spawned
            | _ ->
                let last = conjKilled.[conjKilled.Length - 1]
                last.AllTrue, last.SomeFalse
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, this.Pos, sprintf "Patrol at %d m" (int this.Altitude), this.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
        Mcu.addTargetLink allKilled icon1.Hide.Index
        Mcu.addTargetLink allKilled icon2.Hide.Index
        Mcu.addTargetLink someSpawned icon1.Show.Index
        Mcu.addTargetLink someSpawned icon2.Show.Index
        // logic to stop spawns when all planes from the start airfield have been destroyed
        let counter = BlocksMissionData.newCounter 1
        counter.Count <- this.PlaneReserve
        counter.WrapAround <- false
        let subst = Mcu.substId <| store.GetIdMapper()
        subst counter
        for i in 0 .. this.NumPlanes - 1 do
            Mcu.addTargetLink blocks.[i].Killed counter.Index
            Mcu.addTargetLink counter blocks.[i].WhileEnemyClose.StopMonitoring.Index
        { new McuUtil.IMcuGroup with
              member x.Content = [counter]
              member x.LcStrings = []
              member x.SubGroups = (blocks |> List.map (fun blk -> blk.All)) @ (conjKilled |> Seq.map (fun conj -> conj.All) |> Seq.toList) @ [ icon1.All; icon2.All ]
        }, blocks

    /// Create logic for AI patrols, with constrains that ensure that no more than a maximum limit of planes are active at any time.
    static member ToConstrainedPatrolBlocks(limit : int, store, lcStore, logicPos) (patrols : AiPatrol list) =
        let blocks = patrols |> List.map (fun patrol -> patrol.ToPatrolBlock(store, lcStore))
        let limiter = ResourcePool.ResourcePool.Create(store, limit, logicPos)
        for _, patrols in blocks do
            for patrol in patrols do
                Mcu.addTargetLink limiter.AllGrabbed patrol.WhileEnemyClose.Deactivate.Index
                Mcu.addTargetLink limiter.AvailableAgain patrol.WhileEnemyClose.Activate.Index
                Mcu.addTargetLink patrol.Spawned limiter.Grab.Index
                Mcu.addTargetLink patrol.Completed limiter.Release.Index
        { new McuUtil.IMcuGroup with
            member x.Content = []
            member x.LcStrings = []
            member x.SubGroups =
                [
                    for block, _ in blocks do
                        yield block
                    yield limiter.All
                ]
        },
        blocks |> List.map snd |> List.concat

// A ground attack flight composed of multiple planes
type AiAttack =
    { Attacker : PlaneModel
      HomeAirfield : AirfieldId
      AttackerReserve : int
      NumPlanes : int
      Country : CountryId
      Start : Vector2
      Target : Vector2
      Altitude : float32
      Landing : (Vector2 * float32) option
      Role : PlaneRole
    }
with
    member this.Coalition = this.Country.Coalition

    member this.ToPatrolBlock(store, lcStore) =
        let numPlanes = this.NumPlanes
        let landOrder =
            match this.Landing with
            | None -> NoLanding
            | Some x -> Land x
        let blocks =
            [
                for i in 1..numPlanes do
                    let block = Attacker.Create(store, lcStore, this.Start + (float32 i) * Vector2(500.0f, 500.0f), this.Altitude + 250.0f * (float32 i), this.Target, landOrder)
                    let modmask, payload = this.Attacker.Payloads.[this.Role]
                    block.Plane.Country <- Some this.Country.ToMcuValue
                    this.Attacker.ScriptModel.AssignTo(block.Plane)
                    block.Plane.WMMask <- Some modmask
                    block.Plane.PayloadId <- Some payload
                    block.Plane.Name <- sprintf "ATT-%s-%s" this.Attacker.Name this.HomeAirfield.AirfieldName 
                    yield block
            ]
        let conjKilled =
            [|
                for i in 1..numPlanes-1 do
                    let thisKilled = SturmovikMission.Blocks.Conjunction.Conjunction.Create(store, this.Start + (float32 i) * Vector2(200.0f, 200.0f))
                    Mcu.addTargetLink blocks.[i].Killed thisKilled.SetA.Index
                    Mcu.addTargetLink blocks.[i].Spawned thisKilled.ClearA.Index
                    yield thisKilled
            |]
        for prev, curr in Seq.pairwise conjKilled do
            Mcu.addTargetLink prev.AllTrue curr.SetB.Index
            Mcu.addTargetLink prev.SomeFalse curr.ClearB.Index
        match conjKilled with
        | [||] -> ()
        | _ ->
            let first = conjKilled.[0]
            Mcu.addTargetLink blocks.[0].Killed first.SetB.Index
            Mcu.addTargetLink blocks.[0].Spawned first.ClearB.Index
        let allKilled, someSpawned =
            match conjKilled with
            | [||] ->
                blocks.[0].Killed, blocks.[0].Spawned
            | _ ->
                let last = conjKilled.[conjKilled.Length - 1]
                last.AllTrue, last.SomeFalse
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, 0.1f * (this.Start + 9.0f * this.Target), sprintf "Attackers at %d m" (int this.Altitude), this.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
        Mcu.addTargetLink allKilled icon1.Hide.Index
        Mcu.addTargetLink allKilled icon2.Hide.Index
        Mcu.addTargetLink someSpawned icon1.Show.Index
        Mcu.addTargetLink someSpawned icon2.Show.Index
        // logic to stop spawns when all planes from the start airfield have been destroyed
        let counter = BlocksMissionData.newCounter 1
        counter.Count <- this.AttackerReserve
        counter.WrapAround <- false
        let subst = Mcu.substId <| store.GetIdMapper()
        subst counter
        for i in 0 .. numPlanes - 1 do
            Mcu.addTargetLink blocks.[i].Killed counter.Index
            Mcu.addTargetLink counter blocks.[i].Stop.Index
        // Result
        { new McuUtil.IMcuGroup with
              member x.Content = [counter]
              member x.LcStrings = []
              member x.SubGroups = (blocks |> List.map (fun blk -> blk.All)) @ (conjKilled |> Seq.map (fun conj -> conj.All) |> Seq.toList) @ [ icon1.All; icon2.All ]
        }, blocks

