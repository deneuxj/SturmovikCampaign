/// Given a an old world state and mission result data, produce an updated world state
module Campaign.NewWorldState

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.ResultExtraction

/// Compute the total production capacity of a coalition.
let computeProduction (coalition : CoalitionId) (wg : WorldFastAccess) (state : WorldState) =
    state.Regions
    |> Seq.map (fun s -> wg.GetRegion s.RegionId, s)
    |> Seq.filter (fun (_, s) -> s.Owner = Some coalition)
    |> Seq.sumBy (fun (region, regState) ->
        Seq.zip region.Production regState.ProductionHealth
        |> Seq.sumBy (fun (prod, health) -> health * getProductionPerBuilding prod.Model))

/// What to produce in each category of production, and how much does each category need
type ProductionOrder = {
    Vehicle : GroundAttackVehicle
    PriorityVehicle : float32<E>
    Plane : PlaneModel
    PriorityPlane : float32<E>
    PriorityShells : float32<E>
}

let computeProductionOrder (dt : float32<H>) (coalition : CoalitionId) (budget : float32<E>) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let production = computeProduction coalition wg state
    let shellFulfillTime = // Time needed to fill up defense storage of all regions
        state.Regions
        |> Seq.sumBy (fun state ->
            if state.Owner = Some coalition then
                let capacity =
                    let region = wg.GetRegion state.RegionId
                    Seq.zip region.Storage state.StorageHealth
                    |> Seq.sumBy (fun (sto, health) -> health * getShellsPerBuilding sto.Model)
                capacity - state.ShellCount
                |> max 0.0f
            else
                0.0f)
        |> (*) shellCost
        |> fun cost -> cost / production
    let numHeavy, numMedium, numLight  =
        let perRegion = GroundAttackVehicle.MediumTankCost * 10.0f
        state.Regions
        |> Seq.filter (fun state -> state.Owner = Some coalition) // Regions we control
        |> Seq.filter (fun state -> // Regions at the front
            let region = wg.GetRegion state.RegionId
            region.Neighbours
            |> Seq.exists (fun ngh -> sg.GetRegion(ngh).Owner <> Some coalition))
        |> Seq.map (fun state ->
            state.GetNumVehicles HeavyTank,
            state.GetNumVehicles MediumTank,
            state.GetNumVehicles LightArmor)
        |> Seq.fold (fun (t1, t2, t3) (n1, n2, n3) -> (t1 + n1, t2 + n2, t3 + n3)) (0, 0, 0)
    let planeNeed(af : Airfield, state : AirfieldState) =
        let bomberCapacity =
            af.ParkedBombers |> List.length |> float32
        let attackerCapacity =
            af.ParkedAttackers |> List.length |> float32
        let fighterCapacity =
            af.ParkedFighters |> List.length |> float32
        let numPlanesOfType models =
            models
            |> List.choose (fun model -> Map.tryFind model state.NumPlanes)
            |> List.sum
            |> float32
        let numBombers = numPlanesOfType [ PlaneModel.Ju88a4; Pe2s35 ]
        let numTransports = numPlanesOfType [ PlaneModel.Ju52 ]
        let numAttackPlanes = numPlanesOfType [ PlaneModel.Bf110e; IL2M41 ]
        let numFighters = numPlanesOfType [ PlaneModel.Bf109e7; Bf109f2; Mc202; I16; Mig3; P40 ]
        let fighterUsage =
            if fighterCapacity > 0.0f then
                numFighters / fighterCapacity
            else
                1.0f
        let planeFullfillmentTime (numNeeded : float32) (plane : PlaneModel) =
            plane, plane.Cost * numNeeded / production
        if fighterUsage < 0.5f then
            match coalition with
            | Axis -> Bf109e7
            | Allies -> P40
            |> planeFullfillmentTime (fighterCapacity - numFighters)
            |> Some
        elif fighterUsage < 0.75f then
            match coalition with
            | Axis -> Mc202
            | Allies -> I16
            |> planeFullfillmentTime (fighterCapacity - numFighters)
            |> Some
        elif numAttackPlanes < attackerCapacity then
            match coalition with
            | Axis -> Bf110e
            | Allies -> IL2M41
            |> planeFullfillmentTime (attackerCapacity - numAttackPlanes)
            |> Some
        elif numBombers < bomberCapacity then
            match coalition with
            | Axis ->
                if numBombers / bomberCapacity > 0.75f then
                    Ju52
                else
                    Ju88a4
            | Allies ->
                Pe2s35
            |> planeFullfillmentTime (bomberCapacity - numBombers - numTransports)
            |> Some
        elif fighterUsage < 1.0f then
            match coalition with
            | Axis -> Bf109f2
            | Allies -> Mig3
            |> planeFullfillmentTime (fighterCapacity - numFighters)
            |> Some
        else
            None
    let planeNeeds =
        world.Airfields
        |> Seq.map (fun af -> af, sg.GetAirfield af.AirfieldId)
        |> Seq.filter (fun (af, afs) -> sg.GetRegion(af.Region).Owner = Some coalition)
        |> Seq.choose planeNeed
        |> List.ofSeq
    ()