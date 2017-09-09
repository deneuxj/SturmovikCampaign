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
open Campaign.Util

type AiPatrol =
    { Plane : PlaneModel
      Coalition : CoalitionId
      Pos : Vector2
      Altitude : float32
      ProtectedRegion : RegionId option
      Role : PlaneRole
    }
with
    member this.ToPatrolBlock(store, lcStore) =
        let blocks =
            [
                for i in 0..1 do
                    let block = Patrol.Create(store, lcStore, this.Pos, this.Altitude, this.Coalition.ToCoalition)
                    let modmask, payload = this.Plane.PayloadForRole(this.Role)
                    block.Plane.Country <- this.Coalition.ToCountry
                    block.Plane.Script <- this.Plane.ScriptModel.Script
                    block.Plane.Model <- this.Plane.ScriptModel.Model
                    block.Plane.PayloadId <- Some payload
                    block.Plane.WMMask <- Some modmask
                    yield block
            ]
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
        { new McuUtil.IMcuGroup with
              member x.Content = []
              member x.LcStrings = []
              member x.SubGroups = [ blocks.[0].All; blocks.[1].All; bothKilled.All; icon1.All; icon2.All ]
        }, blocks


let getNumPlanesOfType planeType (numPlanes : Map<PlaneModel, float32>) =
    numPlanes
    |> Seq.sumBy (fun kvp -> if kvp.Key.PlaneType = planeType then kvp.Value else 0.0f)

let getNumPlanesWithRole planeRole (numPlanes : Map<PlaneModel, float32>) =
    numPlanes
    |> Seq.sumBy (fun kvp -> if kvp.Key.Roles.Contains(planeRole) then kvp.Value else 0.0f)

let mkAllPatrols (world : World) (state : WorldState) (coalition : CoalitionId) =
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
            if regState.Owner = Some coalition && regState.ProductionCapacity(region, productionFactor world) > 0.0f<E/H> then
                for af, afState in List.zip world.Airfields state.Airfields do
                    let owner = sg.GetRegion(af.Region).Owner
                    if owner = Some coalition && getNumPlanesOfType Fighter afState.NumPlanes > 2.0f then
                        for enemyAirfield, enemyAirfieldState in threats do
                            for plane, count in afState.NumPlanes |> Map.toSeq do
                                let dir =
                                    let x = enemyAirfield.Pos - region.Position
                                    x / x.Length()
                                let p1 = region.Position + dir * 15000.0f
                                if (p1 - af.Pos).Length() < fighterRange then
                                    if getNumPlanesOfType Bomber enemyAirfieldState.NumPlanes >= 1.0f && plane.Roles.Contains Interceptor && count >= 2.0f then
                                        yield af, {
                                            Plane = plane
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 4000.0f
                                            ProtectedRegion = Some af.Region
                                            Role = Interceptor
                                        }
                                    if getNumPlanesOfType Attacker enemyAirfieldState.NumPlanes >= 1.0f && plane.Roles.Contains Patroller && count >= 2.0f then
                                        yield af, {
                                            Plane = plane
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 3000.0f
                                            ProtectedRegion = Some af.Region
                                            Role = Patroller
                                        }
                                    if getNumPlanesOfType Fighter enemyAirfieldState.NumPlanes >= 1.0f && plane.Roles.Contains Patroller && count >= 2.0f then
                                        yield af, {
                                            Plane = plane
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 2500.0f
                                            ProtectedRegion = Some af.Region
                                            Role = Patroller
                                        }
        // Border patrol and offensive patrol
        let frontline = computeFrontLine true world state.Regions
        for region1, region2 in frontline do
            for af, afState in List.zip world.Airfields state.Airfields do
                let owner = sg.GetRegion(af.Region).Owner
                for plane, count in afState.NumPlanes |> Map.toSeq do
                    if owner = Some coalition && plane.Roles.Contains Patroller && count > 2.0f then
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
                                    Coalition = coalition
                                    Pos = p1
                                    Altitude = 3000.0f
                                    ProtectedRegion = Some ourRegion.RegionId
                                    Role = Patroller
                                }
                            // Offensive patrol
                            let p1 = ourRegion.Position + dir * 20000.0f
                            if (p1 - af.Pos).Length() < fighterRange then
                                yield af, {
                                    Plane = plane
                                    Coalition = coalition
                                    Pos = p1
                                    Altitude = 3000.0f
                                    ProtectedRegion = None
                                    Role = Patroller
                                }
                        | None ->
                            ()
    }

let prioritizeAiPatrols (world : World) (state : WorldState) (patrols : (Airfield * AiPatrol) seq) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let defenseNeeds =
        computeDefenseNeeds world
        |> Map.ofList
    patrols
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
                assets.ProductionCapacity(wg.GetRegion(region), productionFactor world) * 24.0f<H>
            let vulnerability =
                1.0f - assets.Supplies / (max 1.0f<E> (defenseNeeds.TryFind region |> Option.defaultVal 0.0f<E>))
                |> max 0.0f
            vulnerability * (tankAssets + productionAssets + planeAssets), (af.Pos - wg.GetRegion(region).Position).Length()
        | None ->
            0.0f<E>, 0.0f)
    // Do not assign multiple patrols to the same region at the same altitude
    |> Seq.distinctBy (fun (_, patrol) -> patrol.ProtectedRegion, patrol.Altitude)

// A ground attack flight composed of two planes
type AiAttack =
    { Plane : PlaneModel
      Coalition : CoalitionId
      Start : Vector2
      Target : Vector2
      Altitude : float32
      Landing : (Vector2 * float32) option
    }
with
    member this.Payload =
        this.Plane.AttackPayload
        |> snd

    member this.ModMask =
        this.Plane.AttackPayload
        |> fst

    member this.ToPatrolBlock(store, lcStore) =
        let landOrder =
            match this.Landing with
            | None -> NoLanding
            | Some x -> Land x
        let blocks =
            [
                for i in 0..1 do
                    let block = Attacker.Create(store, lcStore, this.Start + (float32 i) * Vector2(500.0f, 500.0f), this.Altitude + 250.0f * (float32 i), this.Target, landOrder)
                    block.Plane.Country <- this.Coalition.ToCountry
                    block.Plane.Script <- this.Plane.ScriptModel.Script
                    block.Plane.Model <- this.Plane.ScriptModel.Model
                    block.Plane.WMMask <- Some this.ModMask
                    block.Plane.PayloadId <- Some this.Payload
                    yield block
            ]
        let bothKilled = SturmovikMission.Blocks.Conjunction.Conjunction.Create(store, this.Start + Vector2(200.0f, 200.0f))
        Mcu.addTargetLink blocks.[0].Killed bothKilled.SetA.Index
        Mcu.addTargetLink blocks.[1].Killed bothKilled.SetB.Index
        Mcu.addTargetLink blocks.[0].Spawned bothKilled.ClearA.Index
        Mcu.addTargetLink blocks.[1].Spawned bothKilled.ClearB.Index
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, 0.1f * (this.Start + 9.0f * this.Target), sprintf "Attackers at %d m" (int this.Altitude), this.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
        Mcu.addTargetLink bothKilled.AllTrue icon1.Hide.Index
        Mcu.addTargetLink bothKilled.AllTrue icon2.Hide.Index
        for i in 0..1 do
            Mcu.addTargetLink blocks.[i].Spawned icon1.Show.Index
            Mcu.addTargetLink blocks.[i].Spawned icon2.Show.Index
        { new McuUtil.IMcuGroup with
              member x.Content = []
              member x.LcStrings = []
              member x.SubGroups = [ blocks.[0].All; blocks.[1].All; bothKilled.All; icon1.All; icon2.All ]
        }, blocks

let mkAllAttackers (world : World) (state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let wg = WorldFastAccess.Create world
    seq {
        for af, afState in List.zip world.Airfields state.Airfields do
            for af2, afState2 in List.zip world.Airfields state.Airfields do
                // Airfield raids
                // af: Axis, af2: Allies, separated by less than 75km (30min round trip at 300km/h)
                if sg.GetRegion(af.Region).Owner = Some Axis && sg.GetRegion(af2.Region).Owner = Some Allies && (af.Pos - af2.Pos).Length() < 75000.0f then
                    for coalition in [Axis; Allies] do
                        for attacker in PlaneModel.AllPlanesOfType(world.PlaneSet, Attacker, coalition) do
                            // If there are enough attackers of the same type, with enough bombs in the airfield's supplies, generate an attack order.
                            let _, payload = attacker.AttackPayload
                            let bombLoad =
                                attacker.BombLoads
                                |> List.tryFind (fun (idx, load) -> idx = payload)
                                |> Option.map snd
                                |> Option.defaultVal 1000.0f<K>
                            let numPlanes = afState.NumPlanes |> Map.tryFind attacker |> Option.defaultVal 0.0f
                            let minPlanes = 6.0f
                            if numPlanes >= minPlanes then
                                match coalition with
                                | Axis ->
                                    if afState.Supplies / bombCost > bombLoad * minPlanes then
                                        yield {
                                            Start = af.Pos
                                            Landing = Some afState.Runway
                                            Target = af2.Pos
                                            Altitude = 2000.0f
                                            Plane = attacker
                                            Coalition = Axis
                                        }, 3
                                | Allies ->
                                    if afState2.Supplies / bombCost > bombLoad * minPlanes then
                                        yield {
                                            Start = af2.Pos
                                            Landing = Some afState2.Runway
                                            Target = af.Pos
                                            Altitude = 2000.0f
                                            Plane = attacker
                                            Coalition = Allies
                                        }, 3
            // Storage raids
            let storages =
                let cache = new System.Collections.Generic.Dictionary<_, _>()
                cached cache (fun (region : Region) -> region.Storage |> Util.Algo.computePartition (fun grp1 grp2 -> (grp1.Pos.Pos - grp2.Pos.Pos).Length() < 1000.0f))
            match sg.GetRegion(af.Region).Owner with
            | Some coalition ->
                for region, regState in List.zip world.Regions state.Regions do
                    if regState.Owner = Some coalition.Other && (region.Position - af.Pos).Length() < 75000.0f then
                        for attacker in PlaneModel.AllPlanesOfType(world.PlaneSet, Attacker, coalition) do
                            // If there are enough attackers of the same type, with enough bombs in the airfield's supplies, generate an attack order.
                            let _, payload = attacker.AttackPayload
                            let bombLoad =
                                attacker.BombLoads
                                |> List.tryFind (fun (idx, load) -> idx = payload)
                                |> Option.map snd
                                |> Option.defaultVal 1000.0f<K>
                            let numPlanes = afState.NumPlanes |> Map.tryFind attacker |> Option.defaultVal 0.0f
                            let minPlanes = 2.0f
                            if numPlanes >= minPlanes then
                                if afState.Supplies / bombCost > bombLoad * minPlanes then
                                    let supplies = storages region
                                    for supplyGroup in supplies do
                                        yield {
                                            Start = af.Pos
                                            Landing = Some afState.Runway
                                            Target = supplyGroup.Head.Pos.Pos
                                            Altitude = 2000.0f
                                            Plane = attacker
                                            Coalition = coalition
                                        }, 1
            | None -> ()
    }
