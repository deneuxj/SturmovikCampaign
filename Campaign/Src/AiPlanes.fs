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

module Campaign.AiPlanes

open System.Numerics
open VectorExtension

open SturmovikMission.Blocks.Patrol
open SturmovikMission.Blocks.GroundAttack
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Cached
open SturmovikMission.Blocks.IconDisplay

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription
open Campaign.WorldState
open Util
open SturmovikMission.Blocks

type AiPatrol =
    { Plane : PlaneModel
      HomeAirfield : AirfieldId
      Coalition : CoalitionId
      Pos : Vector2
      Altitude : float32
      ProtectedRegion : RegionId option
      Role : PlaneRole
      PlaneReserve : int // Max number of planes that can be use for that patrol
    }
with
    member this.ToPatrolBlock(store, lcStore) =
        let blocks =
            [
                for i in 0..1 do
                    let block = Patrol.Create(store, lcStore, this.Pos + (float32 i) * Vector2(500.0f, 500.0f), this.Altitude + 250.0f * (float32 i), this.Coalition.ToCoalition)
                    let modmask, payload = this.Plane.PayloadForRole(this.Role)
                    block.Plane.Country <- Some this.Coalition.ToCountry
                    this.Plane.ScriptModel.AssignTo(block.Plane)
                    block.Plane.PayloadId <- Some payload
                    block.Plane.WMMask <- Some modmask
                    block.Plane.Name <- sprintf "PTL-%s-%s" this.Plane.PlaneName this.HomeAirfield.AirfieldName
                    yield block
            ]
        // Logic to hide icon when the patrol is wiped out
        let bothKilled = SturmovikMission.Blocks.Conjunction.Conjunction.Create(store, this.Pos + Vector2(200.0f, 200.0f))
        Mcu.addTargetLink blocks.[0].Killed bothKilled.SetA.Index
        Mcu.addTargetLink blocks.[1].Killed bothKilled.SetB.Index
        Mcu.addTargetLink blocks.[0].Spawned bothKilled.ClearA.Index
        Mcu.addTargetLink blocks.[1].Spawned bothKilled.ClearB.Index
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, this.Pos, sprintf "Patrol at %d m" (int this.Altitude), this.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
        Mcu.addTargetLink bothKilled.AllTrue icon1.Hide.Index
        Mcu.addTargetLink bothKilled.AllTrue icon2.Hide.Index
        for i in 0..1 do
            Mcu.addTargetLink blocks.[i].Spawned icon1.Show.Index
            Mcu.addTargetLink blocks.[i].Spawned icon2.Show.Index
        // logic to stop spawns when all planes from the start airfield have been destroyed
        let counter = BlocksMissionData.newCounter 1
        counter.Count <- this.PlaneReserve
        counter.WrapAround <- false
        let subst = Mcu.substId <| store.GetIdMapper()
        subst counter
        for i in 0..1 do
            Mcu.addTargetLink blocks.[i].Killed counter.Index
            Mcu.addTargetLink counter blocks.[i].WhileEnemyClose.StopMonitoring.Index
        { new McuUtil.IMcuGroup with
              member x.Content = [counter]
              member x.LcStrings = []
              member x.SubGroups = [ blocks.[0].All; blocks.[1].All; bothKilled.All; icon1.All; icon2.All ]
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

    static member TryExtractHomeAirfield(name : string) =
        seq {
            if name.StartsWith("PTL-") then
                let s = name.Substring(4)
                for plane in PlaneModel.AllModels do
                    if s.StartsWith(plane.PlaneName) then
                        let af = AirfieldId (s.Substring(plane.PlaneName.Length + 1))
                        yield plane, af
        }
        |> Seq.tryHead

let getNumPlanesOfType planeType (numPlanes : Map<PlaneModel, float32>) =
    numPlanes
    |> Seq.sumBy (fun kvp -> if kvp.Key.PlaneType = planeType then kvp.Value else 0.0f)

let getNumPlanesWithRole planeRole (numPlanes : Map<PlaneModel, float32>) =
    numPlanes
    |> Seq.sumBy (fun kvp -> if kvp.Key.Roles.Contains(planeRole) then kvp.Value else 0.0f)

let mkAllPatrols (world : World) (state : WorldState) (reservedPlanes : Map<_, PlayerHangar.PlayerHangar>) (coalition : CoalitionId) =
    let sg = WorldStateFastAccess.Create state
    let wg = WorldFastAccess.Create world
    let threats =
        [
            for af, afState in List.zip world.Airfields state.Airfields do
                let owner = sg.GetRegion(af.Region).Owner
                if owner = Some coalition.Other && afState.TotalPlaneValue >= PlaneModel.I16.Cost * 2.0f then
                    yield af, afState
        ]
    let fighterRange = 60000.0f
    seq {
        // Defensive patrols
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && regState.ProductionCapacity(region, world.SubBlockSpecs, world.ProductionFactor) > 0.0f<E/H> then
                for af, afState in List.zip world.Airfields state.Airfields do
                    let owner = sg.GetRegion(af.Region).Owner
                    if owner = Some coalition && getNumPlanesOfType Fighter afState.NumPlanes > 2.0f then
                        for enemyAirfield, enemyAirfieldState in threats do
                            for plane, count in afState.NumPlanes |> Map.toSeq |> Seq.filter (fun (plane, _) -> plane.Coalition = coalition) do
                                let count = count - PlayerHangar.getTotalPlanesReservedAtAirfield coalition af.AirfieldId plane reservedPlanes
                                let dir =
                                    let x = enemyAirfield.Pos - region.Position
                                    x / x.Length()
                                let p1 = region.Position + dir * 15000.0f
                                if (p1 - af.Pos).Length() < fighterRange then
                                    if getNumPlanesOfType Bomber enemyAirfieldState.NumPlanes >= 1.0f && plane.Roles.Contains Interceptor && count >= 2.0f then
                                        yield af, {
                                            Plane = plane
                                            HomeAirfield = af.AirfieldId
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 4000.0f
                                            ProtectedRegion = Some af.Region
                                            Role = Interceptor
                                            PlaneReserve = int count
                                        }
                                    if getNumPlanesOfType Attacker enemyAirfieldState.NumPlanes >= 1.0f && plane.Roles.Contains Patroller && count >= 2.0f then
                                        yield af, {
                                            Plane = plane
                                            HomeAirfield = af.AirfieldId
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 3000.0f
                                            ProtectedRegion = Some af.Region
                                            Role = Patroller
                                            PlaneReserve = int count
                                        }
                                    if getNumPlanesOfType Fighter enemyAirfieldState.NumPlanes >= 1.0f && plane.Roles.Contains Patroller && count >= 2.0f then
                                        yield af, {
                                            Plane = plane
                                            HomeAirfield = af.AirfieldId
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 2500.0f
                                            ProtectedRegion = Some af.Region
                                            Role = Patroller
                                            PlaneReserve = int count
                                        }
        // Border patrol and offensive patrol
        let frontline = computeFrontLine true world state.Regions
        for region1, region2 in frontline do
            for af, afState in List.zip world.Airfields state.Airfields do
                let owner = sg.GetRegion(af.Region).Owner
                for plane, count in afState.NumPlanes |> Map.toSeq do
                    let count = count - PlayerHangar.getTotalPlanesReservedAtAirfield coalition af.AirfieldId plane reservedPlanes
                    if owner = Some coalition && plane.Coalition = coalition && plane.Roles.Contains Patroller && count > 2.0f then
                        // Order the two regions so that the friendly one is first, if any
                        let regions =
                            if sg.GetRegion(region1).Owner = Some coalition then
                                Some(region1, region2)
                            else if sg.GetRegion(region2).Owner = Some coalition then
                                Some(region2, region1)
                            else
                                None
                        match regions with
                        | Some(ourRegion, theirRegion) ->
                            let ourRegion = wg.GetRegion(ourRegion)
                            let theirRegion = wg.GetRegion(theirRegion)
                            let dir =
                                let x = theirRegion.Position - ourRegion.Position
                                x / x.Length()
                            // Defensive patrol
                            let p1 = ourRegion.Position + dir * 5000.0f
                            if (p1 - af.Pos).Length() < fighterRange then
                                yield af, {
                                    Plane = plane
                                    HomeAirfield = af.AirfieldId
                                    Coalition = coalition
                                    Pos = p1
                                    Altitude = 3000.0f
                                    ProtectedRegion = Some ourRegion.RegionId
                                    Role = Patroller
                                    PlaneReserve = int count
                                }
                            // Offensive patrol
                            let p1 = ourRegion.Position + dir * 20000.0f
                            if (p1 - af.Pos).Length() < fighterRange then
                                yield af, {
                                    Plane = plane
                                    HomeAirfield = af.AirfieldId
                                    Coalition = coalition
                                    Pos = p1
                                    Altitude = 3000.0f
                                    ProtectedRegion = None
                                    Role = Patroller
                                    PlaneReserve = int count
                                }
                        | None ->
                            ()
    }

let prioritizeAiPatrols (missionLength : float32<H>) (world : World) (state : WorldState) (patrols : (Airfield * AiPatrol) seq) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let regionSupplyLevels = state.GetAmmoFillLevelPerRegion(world, missionLength)
    let random = System.Random()
    patrols
    // So that it's not always the same type of plane that gets to do patrols when an airfield has multiple types of fighters available.
    |> Array.ofSeq
    |> Array.shuffle random
    // Prioritize by vulnerability and assets, then by distance (prioritize greater distances to help extend reach of covers).
    |> Seq.sortByDescending (fun (af, patrol) ->
        match patrol.ProtectedRegion with
        | Some region ->
            let assets = sg.GetRegion(region)
            let planeAssets =
                List.zip world.Airfields state.Airfields
                |> List.sumBy (fun (af, afState) -> if af.Region = region then afState.TotalPlaneValue + afState.Supplies else 0.0f<E>)
            let tankAssets =
                assets.TotalVehicleValue
            let productionAssets =
                assets.ProductionCapacity(wg.GetRegion(region), world.SubBlockSpecs, world.ProductionFactor) * 24.0f<H>
            let vulnerability =
                1.0f - (regionSupplyLevels.TryFind region |> Option.defaultValue 0.0f)
                |> max 0.0f
            vulnerability * (tankAssets + productionAssets + planeAssets), (af.Pos - wg.GetRegion(region).Position).Length()
        | None ->
            0.0f<E>, 0.0f)
    // Do not assign multiple patrols to the same region at the same altitude
    |> Seq.distinctBy (fun (_, patrol) -> patrol.ProtectedRegion, patrol.Altitude)
    // Do not assign the same patrols to multiple jobs
    |> Seq.distinctBy (fun (_, patrol) -> patrol.HomeAirfield, patrol.Plane)

// A ground attack flight composed of multiple planes
type AiAttack =
    { Attacker : PlaneModel
      HomeAirfield : AirfieldId
      AttackerReserve : int
      NumPlanes : int
      Coalition : CoalitionId
      Start : Vector2
      Target : Vector2
      Altitude : float32
      Landing : (Vector2 * float32) option
      Role : PlaneRole
    }
with
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
                    let modmask, payload = this.Attacker.PayloadForRole(this.Role)
                    block.Plane.Country <- Some this.Coalition.ToCountry
                    this.Attacker.ScriptModel.AssignTo(block.Plane)
                    block.Plane.WMMask <- Some modmask
                    block.Plane.PayloadId <- Some payload
                    block.Plane.Name <- sprintf "ATT-%s-%s" this.Attacker.PlaneName this.HomeAirfield.AirfieldName 
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

    static member TryExtractHomeAirfield(name : string) =
        seq {
            if name.StartsWith("ATT-") then
                let s = name.Substring(4)
                for plane in PlaneModel.AllModels do
                    if s.StartsWith(plane.PlaneName) then
                        let af = AirfieldId (s.Substring(plane.PlaneName.Length + 1))
                        yield plane, af
        }
        |> Seq.tryHead

let mkAllAttackers (world : World) (state : WorldState) (reservedPlanes : Map<_, PlayerHangar.PlayerHangar>) =
    let sg = WorldStateFastAccess.Create state
    let wg = WorldFastAccess.Create world
    let attackers = world.PlaneSet.AllModels |> Seq.filter (fun plane -> plane.Roles.Contains GroundAttacker)
    seq {
        for af, afState in List.zip world.Airfields state.Airfields do
            for af2, afState2 in List.zip world.Airfields state.Airfields do
                // Airfield raids
                // af: Axis, af2: Allies, separated by less than 75km (30min round trip at 300km/h)
                if sg.GetRegion(af.Region).Owner = Some Axis && sg.GetRegion(af2.Region).Owner = Some Allies && (af.Pos - af2.Pos).Length() < 75000.0f then
                    for coalition in [Axis; Allies] do
                        for attacker in attackers do
                            // If there are enough attackers of the same type, with enough bombs in the airfield's supplies, generate an attack order.
                            let _, payload = attacker.AttackPayload
                            let bombLoad =
                                attacker.BombLoads
                                |> List.tryFind (fun (idx, load) -> idx = payload)
                                |> Option.map snd
                                |> Option.defaultVal 1000.0f<K>
                            let minPlanes = 6.0f
                            match coalition with
                            | Axis ->
                                let numPlanes = afState.NumPlanes |> Map.tryFind attacker |> Option.defaultVal 0.0f
                                let numPlanes = numPlanes - PlayerHangar.getTotalPlanesReservedAtAirfield coalition af.AirfieldId attacker reservedPlanes
                                if numPlanes >= minPlanes && attacker.Coalition = coalition && afState.Supplies / bombCost > bombLoad * minPlanes then
                                    yield {
                                        Start = af.Pos
                                        HomeAirfield = af.AirfieldId
                                        AttackerReserve = int numPlanes
                                        Landing = Some afState.Runway
                                        Target = af2.Pos
                                        Altitude = 2000.0f
                                        Attacker = attacker
                                        NumPlanes = 3
                                        Coalition = coalition
                                        Role = GroundAttacker
                                    }
                            | Allies ->
                                let numPlanes = afState.NumPlanes |> Map.tryFind attacker |> Option.defaultVal 0.0f
                                let numPlanes = numPlanes - PlayerHangar.getTotalPlanesReservedAtAirfield coalition af2.AirfieldId attacker reservedPlanes
                                if numPlanes >= minPlanes && attacker.Coalition = coalition && afState2.Supplies / bombCost > bombLoad * minPlanes then
                                    yield {
                                        Start = af2.Pos
                                        HomeAirfield = af2.AirfieldId
                                        AttackerReserve = int numPlanes
                                        Landing = Some afState2.Runway
                                        Target = af.Pos
                                        Altitude = 2000.0f
                                        Attacker = attacker
                                        NumPlanes = 3
                                        Coalition = coalition
                                        Role = GroundAttacker
                                    }
            // Storage raids
            let storages =
                let cache = new System.Collections.Generic.Dictionary<_, _>()
                cached cache (fun (region : Region) -> region.Storage |> Util.Algo.computePartition (fun grp1 grp2 -> (grp1.Pos.Pos - grp2.Pos.Pos).Length() < 1000.0f))
            match sg.GetRegion(af.Region).Owner with
            | Some coalition ->
                for region, regState in List.zip world.Regions state.Regions do
                    if regState.Owner = Some coalition.Other && (region.Position - af.Pos).Length() < 75000.0f then
                        for attacker in attackers |> Seq.filter (fun plane -> plane.Coalition = coalition) do
                            // If there are enough attackers of the same type, with enough bombs in the airfield's supplies, generate an attack order.
                            let _, payload = attacker.AttackPayload
                            let bombLoad =
                                attacker.BombLoads
                                |> List.tryFind (fun (idx, load) -> idx = payload)
                                |> Option.map snd
                                |> Option.defaultVal 1000.0f<K>
                            let numPlanes = afState.NumPlanes |> Map.tryFind attacker |> Option.defaultVal 0.0f
                            let numPlanes = numPlanes - PlayerHangar.getTotalPlanesReservedAtAirfield coalition af.AirfieldId attacker reservedPlanes
                            let minPlanes = 2.0f
                            if numPlanes >= minPlanes then
                                if afState.Supplies / bombCost > bombLoad * minPlanes then
                                    let supplies = storages region
                                    for supplyGroup in supplies do
                                        yield {
                                            Start = af.Pos
                                            HomeAirfield = af.AirfieldId
                                            AttackerReserve = int numPlanes
                                            Landing = Some afState.Runway
                                            Target = supplyGroup.Head.Pos.Pos
                                            Altitude = 2000.0f
                                            Attacker = attacker
                                            NumPlanes = 1
                                            Coalition = coalition
                                            Role = GroundAttacker
                                        }
            | None -> ()
    }
    |> Array.ofSeq
    |> Array.shuffle (System.Random())
    // Do not assign multiple groups to the same region
    |> Seq.distinctBy (fun group -> group.Target)
    // Do not assign the same patrols to multiple jobs
    |> Seq.distinctBy (fun group -> group.HomeAirfield, group.Attacker)
