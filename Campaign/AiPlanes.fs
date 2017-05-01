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


type AiPatrol =
    { Plane : PlaneModel
      Coalition : CoalitionId
      P1 : Vector2
      P2 : Vector2
      Altitude : float32
    }
with
    member this.ToPatrolBlock(store, lcStore) =
        let block = Patrol.Create(store, lcStore, this.Altitude, this.P1, this.P2, 5000.0f)
        block.Plane.Country <- this.Coalition.ToCountry
        block.Plane.Script <- this.Plane.ScriptModel.Script
        block.Plane.Model <- this.Plane.ScriptModel.Model
        let icon1, icon2 = IconDisplay.CreatePair(store, lcStore, 0.5f * (this.P1 + this.P2), sprintf "Patrol at %d m" (int this.Altitude), this.Coalition.ToCoalition, Mcu.IconIdValue.CoverBombersFlight)
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

let mkAllPatrols (world : World) (state : WorldState) (coalition : CoalitionId) =
    let intercepter =
        match coalition with
        | Axis -> PlaneModel.Bf110e
        | Allies -> PlaneModel.Mig3
    let protector =
        match coalition with
        | Axis -> PlaneModel.Bf109f2
        | Allies -> PlaneModel.P40
    let fighter =
        match coalition with
        | Axis -> PlaneModel.Mc202
        | Allies -> PlaneModel.I16
    let sg = WorldStateFastAccess.Create state
    let wg = WorldFastAccess.Create world
    let threats =
        [
            for af, afState in List.zip world.Airfields state.Airfields do
                let owner = sg.GetRegion(af.Region).Owner
                if owner = Some coalition.Other && afState.TotalPlaneValue >= PlaneModel.I16.Cost * 2.0f then
                    yield af, afState
        ]
    seq {
        // Defensive patrols
#if DISABLED
        for region, regState in List.zip world.Regions state.Regions do
            if regState.Owner = Some coalition && regState.ProductionCapacity(region) > 0.0f<E/H> then
                for af, afState in threats do
                    let dir =
                        let x = af.Pos - region.Position
                        x / x.Length()
                    let side = dir.Rotate(90.0f)
                    let p1 = region.Position + dir * 15000.0f + side * 15000.0f
                    let p2 = p1 - side * 30000.0f
                    if getNumPlanesOfType Bomber afState.NumPlanes >= 1.0f then
                        yield {
                            Plane = intercepter
                            Coalition = coalition
                            P1 = p1
                            P2 = p2
                            Altitude = 4000.0f
                        }
                    if getNumPlanesOfType Attacker afState.NumPlanes >= 1.0f then
                        yield {
                            Plane = protector
                            Coalition = coalition
                            P1 = p1
                            P2 = p2
                            Altitude = 3000.0f
                        }
                    if getNumPlanesOfType Fighter afState.NumPlanes >= 1.0f then
                        yield {
                            Plane = fighter
                            Coalition = coalition
                            P1 = p1
                            P2 = p2
                            Altitude = 2500.0f
                        }
#endif
        // Border patrol and offensive patrol
        let frontline = computeFrontLine true world state.Regions
        for region1, region2 in frontline do
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
                let side = dir.Rotate(90.0f)
                let p1 = ourRegion.Position + dir * 5000.0f + side * 15000.0f
                let p2 = p1 - side * 30000.0f
                match PlaneModel.RandomPlaneOfType(PlaneType.Fighter, coalition) with
                | Some fighter ->
                    yield {
                        Plane = fighter
                        Coalition = coalition
                        P1 = p1
                        P2 = p2
                        Altitude = 3000.0f
                    }
                    let p1 = ourRegion.Position + dir * 20000.0f + side * 15000.0f
                    let p2 = p1 - side * 30000.0f
                    yield {
                        Plane = fighter
                        Coalition = coalition
                        P1 = p1
                        P2 = p2
                        Altitude = 3000.0f
                    }
                | None ->
                    ()
            | None ->
                ()
    }


type AiAttack =
    { Plane : PlaneModel
      Coalition : CoalitionId
      Start : Vector2
      Target : Vector2
      Altitude : float32
    }
with
    member this.ToPatrolBlock(store, lcStore) =
        let block = Attacker.Create(store, lcStore, this.Start, this.Altitude, this.Target)
        block.Plane.Country <- this.Coalition.ToCountry
        block.Plane.Script <- this.Plane.ScriptModel.Script
        block.Plane.Model <- this.Plane.ScriptModel.Model
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
                            Target = af2.Pos
                            Altitude = 2000.0f
                            Plane = PlaneModel.RandomPlaneOfType(Attacker, Axis).Value
                            Coalition = Axis
                        }, 3
                    if numAlliesAttackers >= 7.0f then
                        yield {
                            Start = af.Pos
                            Target = af2.Pos
                            Altitude = 2000.0f
                            Plane = PlaneModel.RandomPlaneOfType(Attacker, Allies).Value
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
                                    Target = supplyGroup.Head.Pos.Pos
                                    Altitude = 2000.0f
                                    Plane = PlaneModel.RandomPlaneOfType(Attacker, coalition).Value
                                    Coalition = coalition
                                }, 2
            | None -> ()
    }
