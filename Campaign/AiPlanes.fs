module Campaign.AiPlanes

open System.Numerics
open Vector

open SturmovikMission.Blocks.Patrol
open SturmovikMission.Blocks.GroundAttack
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Cached

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
    }
with
    // Payload suitable for air-to-air engagements.
    member this.Payload =
        match this.Plane with
        | PlaneModel.Bf109e7 -> 0 // "0,1-MG17-AP-2000 + 2,3-MGFF-APHE-120"
        | PlaneModel.Bf109f2 -> 3 // "0,1-MG17-AP-1000 + 2-MG15120-APHE-200"
        | PlaneModel.Bf109f4 -> 0 // "0,1-MG17-AP-1000 + 2-MG15120-APHE-200"
        | PlaneModel.Bf109g2 -> 0 // "0,1-MG17-AP-1000 + 2-MG15120-APHE-200"
        | PlaneModel.Bf109g4 -> 0 // "0,1-MG17-AP-1000 + 2-MG15120-APHE-200"
        | PlaneModel.Fw190a3 -> 0 // "0,1-MG17-AP-1800 + 2,3-MG15120-APHE-500"
        | PlaneModel.Fw190a5 -> 0 // "0,1-MG17-AP-1800 + 2,3-MG15120-APHE-500"
        | PlaneModel.I16 -> 11 // "0,1-SHKAS-AP-1000 + 2,3-SHVAK-APHE-180-add"
        | PlaneModel.Mc202 -> 4 // 0,1-BREDA12-APHE-800 + 2,3-MG15120-APHE-270-add
        | PlaneModel.Mig3 -> 16 // "0,1-SHVAK-APHE-300"
        | PlaneModel.P40 -> 0 // "0,1,2,3,4,5-M250-AP-1410"
        | PlaneModel.Yak1s69 -> 0 // "0,1-SHKAS-AP-1500 + 2-SHVAK-APHE-120"
        | PlaneModel.Yak1s127 -> 0 // "0-UB-APHE-220 + 1-SHVAK-APHE-140"
        | PlaneModel.Lagg3s29 -> 1 // "0-UB-APHE-200 + 1-VYA23-APHE-90-add"
        | PlaneModel.La5 -> 0 // "0,1-SHVAK-APHE-340"
        | _ -> 0

    member this.ToPatrolBlock(store, lcStore) =
        let block = Patrol.Create(store, lcStore, this.Pos, this.Altitude, this.Coalition.ToCoalition)
        block.Plane.Country <- this.Coalition.ToCountry
        block.Plane.Script <- this.Plane.ScriptModel.Script
        block.Plane.Model <- this.Plane.ScriptModel.Model
        block.Plane.PayloadId <- Some this.Payload
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, this.Pos, sprintf "Patrol at %d m" (int this.Altitude), this.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
        Mcu.addTargetLink block.Killed icon1.Hide.Index
        Mcu.addTargetLink block.Killed icon2.Hide.Index
        Mcu.addTargetLink block.Spawned icon1.Show.Index
        Mcu.addTargetLink block.Spawned icon2.Show.Index
        { new McuUtil.IMcuGroup with
              member x.Content = []
              member x.LcStrings = []
              member x.SubGroups = [ block.All; icon1.All; icon2.All ]
        }, block


let getNumPlanesOfType planeType (numPlanes : Map<PlaneModel, float32>) =
    numPlanes
    |> Seq.sumBy (fun kvp -> if kvp.Key.PlaneType = planeType then kvp.Value else 0.0f)

let getNumPlanesWithRole planeRole (numPlanes : Map<PlaneModel, float32>) =
    numPlanes
    |> Seq.sumBy (fun kvp -> if kvp.Key.Roles.Contains(planeRole) then kvp.Value else 0.0f)

let mkAllPatrols (world : World) (state : WorldState) (coalition : CoalitionId) =
    let intercepter =
        PlaneModel.RandomPlaneWithRole(world.PlaneSet, PlaneRole.Interceptor, coalition)
    let fighter =
        PlaneModel.RandomPlaneWithRole(world.PlaneSet, PlaneRole.Patroller, coalition)

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
            if regState.Owner = Some coalition && regState.ProductionCapacity(region) > 0.0f<E/H> then
                for af, afState in List.zip world.Airfields state.Airfields do
                    let owner = sg.GetRegion(af.Region).Owner
                    if owner = Some coalition && getNumPlanesOfType Fighter afState.NumPlanes > 2.0f then
                        for enemyAirfield, enemyAirfieldState in threats do
                            let dir =
                                let x = enemyAirfield.Pos - region.Position
                                x / x.Length()
                            let p1 = region.Position + dir * 15000.0f
                            if (p1 - af.Pos).Length() < fighterRange then
                                if getNumPlanesOfType Bomber enemyAirfieldState.NumPlanes >= 1.0f && getNumPlanesWithRole Interceptor afState.NumPlanes > 1.0f then
                                    match intercepter with
                                    | Some intercepter ->
                                        yield af, {
                                            Plane = intercepter
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 4000.0f
                                            ProtectedRegion = Some af.Region
                                        }
                                    | None ->
                                        ()
                                if getNumPlanesOfType Attacker enemyAirfieldState.NumPlanes >= 1.0f && getNumPlanesWithRole Patroller afState.NumPlanes > 1.0f then
                                    match fighter with
                                    | Some protector ->
                                        yield af, {
                                            Plane = protector
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 3000.0f
                                            ProtectedRegion = Some af.Region
                                        }
                                    | None ->
                                        ()
                                if getNumPlanesOfType Fighter enemyAirfieldState.NumPlanes >= 1.0f && getNumPlanesWithRole Patroller afState.NumPlanes > 1.0f then
                                    match fighter with
                                    | Some fighter ->
                                        yield af, {
                                            Plane = fighter
                                            Coalition = coalition
                                            Pos = p1
                                            Altitude = 2500.0f
                                            ProtectedRegion = Some af.Region
                                        }
                                    | None ->
                                        ()
        // Border patrol and offensive patrol
        let frontline = computeFrontLine true world state.Regions
        for region1, region2 in frontline do
            for af, afState in List.zip world.Airfields state.Airfields do
                let owner = sg.GetRegion(af.Region).Owner
                if owner = Some coalition && getNumPlanesOfType Fighter afState.NumPlanes > 2.0f then
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
                        match PlaneModel.RandomPlaneOfType(world.PlaneSet, PlaneType.Fighter, coalition) with
                        | Some fighter ->
                            let p1 = ourRegion.Position + dir * 5000.0f
                            if (p1 - af.Pos).Length() < fighterRange then
                                yield af, {
                                    Plane = fighter
                                    Coalition = coalition
                                    Pos = p1
                                    Altitude = 3000.0f
                                    ProtectedRegion = Some ourRegion.RegionId
                                }
                            let p1 = ourRegion.Position + dir * 20000.0f
                            if (p1 - af.Pos).Length() < fighterRange then
                                yield af, {
                                    Plane = fighter
                                    Coalition = coalition
                                    Pos = p1
                                    Altitude = 3000.0f
                                    ProtectedRegion = None
                                }
                        | None ->
                            ()
                    | None ->
                        ()
    }

let prioritizeAiPatrols (world : World) (state : WorldState) (patrols : (_ * AiPatrol) seq) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let defenseNeeds =
        computeDefenseNeeds world
        |> Map.ofList
    patrols
    |> Seq.sortByDescending (fun (_, patrol) ->
        match patrol.ProtectedRegion with
        | Some region ->
            let assets = sg.GetRegion(region)
            let planeAssets =
                List.zip world.Airfields state.Airfields
                |> List.sumBy (fun (af, afState) -> if af.Region = region then afState.TotalPlaneValue + afState.Supplies else 0.0f<E>)
            let tankAssets =
                assets.TotalVehicleValue
            let productionAssets =
                assets.ProductionCapacity(wg.GetRegion(region)) * 24.0f<H>
            let vulnerability =
                1.0f - assets.Supplies / (max 1.0f<E> (defenseNeeds.TryFind region |> Option.defaultVal 0.0f<E>))
                |> max 0.0f
            vulnerability * (tankAssets + productionAssets + planeAssets)
        | None ->
            0.0f<E>
    )

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
        match this.Plane with
        | PlaneModel.Ju87 -> 5 // "0,1-MG17-AP-2000 + SC250-3"
        | PlaneModel.Bf110e -> 2 // "0,1,2,3-MG17-AP-4000 + 4,5-MGFF-APHE-360 + SC250-2 + SC50-4"
        | PlaneModel.Bf110g -> 2 //  "0,1,2,3-MG17-AP-4000 + 4-MG15120-APHE-400 + 5-MG15120-APHE-350 + SC250-2 + SC50-4"
        | PlaneModel.IL2M41 -> 32 // "0,1-SHKAS-AP-1500 + 2,3-SHVAK-APHE-420 + FAB100M-4 + ROS82-8"
        | PlaneModel.IL2M42 -> 44 // "0,1-SHKAS-AP-1500 + 2,3-SHVAK-APHE-500 + FAB100M-4 + ROS82-8"
        | PlaneModel.IL2M43 -> 41
        | PlaneModel.Pe2s35 -> 5 // "0-SHKAS-AP-450 + 1-UB-APHE-150 + FAB250SV-4"
        | _ -> 0

    member this.ModMask =
        match this.Plane with
        | PlaneModel.IL2M42 -> 33
        | _ -> 1

    member this.ToPatrolBlock(store, lcStore) =
        let landOrder =
            match this.Landing with
            | None -> NoLanding
            | Some x -> Land x
        let block = Attacker.Create(store, lcStore, this.Start, this.Altitude, this.Target, landOrder)
        block.Plane.Country <- this.Coalition.ToCountry
        block.Plane.Script <- this.Plane.ScriptModel.Script
        block.Plane.Model <- this.Plane.ScriptModel.Model
        block.Plane.WMMask <- Some this.ModMask
        block.Plane.PayloadId <- Some this.Payload
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, 0.1f * (this.Start + 9.0f * this.Target), sprintf "Attackers at %d m" (int this.Altitude), this.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
        Mcu.addTargetLink block.Killed icon1.Hide.Index
        Mcu.addTargetLink block.Killed icon2.Hide.Index
        Mcu.addTargetLink block.Spawned icon1.Show.Index
        Mcu.addTargetLink block.Spawned icon2.Show.Index
        { new McuUtil.IMcuGroup with
              member x.Content = []
              member x.LcStrings = []
              member x.SubGroups = [ block.All; icon1.All; icon2.All ]
        }, block

let mkAllAttackers (world : World) (state : WorldState) =
    let sg = WorldStateFastAccess.Create state
    let wg = WorldFastAccess.Create world
    seq {
        for af, afState in List.zip world.Airfields state.Airfields do
            for af2, afState2 in List.zip world.Airfields state.Airfields do
                // Airfield raids
                // af: Axis, af2: Allies, separated by less than 75km (30min round trip at 300km/h)
                if sg.GetRegion(af.Region).Owner = Some Axis && sg.GetRegion(af2.Region).Owner = Some Allies && (af.Pos - af2.Pos).Length() < 75000.0f then
                    let numAxisAttackers = getNumPlanesOfType Attacker afState.NumPlanes
                    let numAlliesAttackers = getNumPlanesOfType Attacker afState2.NumPlanes
                    if numAxisAttackers >= 7.0f then
                        yield {
                            Start = af.Pos
                            Landing = Some afState.Runway
                            Target = af2.Pos
                            Altitude = 2000.0f
                            Plane = PlaneModel.RandomPlaneOfType(world.PlaneSet, Attacker, Axis).Value
                            Coalition = Axis
                        }, 3
                    if numAlliesAttackers >= 7.0f then
                        yield {
                            Start = af2.Pos
                            Landing = Some afState2.Runway
                            Target = af.Pos
                            Altitude = 2000.0f
                            Plane = PlaneModel.RandomPlaneOfType(world.PlaneSet, Attacker, Allies).Value
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
                        let numAttackers = getNumPlanesOfType Attacker afState.NumPlanes
                        if numAttackers >= 4.0f then
                            let supplies = storages region
                            for supplyGroup in supplies do
                                yield {
                                    Start = af.Pos
                                    Landing = Some afState.Runway
                                    Target = supplyGroup.Head.Pos.Pos
                                    Altitude = 2000.0f
                                    Plane = PlaneModel.RandomPlaneOfType(world.PlaneSet, Attacker, coalition).Value
                                    Coalition = coalition
                                }, 2
            | None -> ()
    }
