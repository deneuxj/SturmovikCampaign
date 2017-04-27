module Campaign.AiPlanes

open System.Numerics
open Vector

open SturmovikMission.Blocks.Patrol

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
        block


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
        // Border patrol and offensive patrol
        let frontline = computeFrontLine true world state.Regions
        for region1, region2 in frontline do
            let regions =
                if sg.GetRegion(region1).Owner = Some coalition then
                    Some(region1, region2)
                else if sg.GetRegion(region2).Owner = Some coalition then
                    Some(region2, region2)
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
    }