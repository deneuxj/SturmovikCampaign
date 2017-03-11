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
type ProductionPriorities = {
    Vehicle : GroundAttackVehicle
    PriorityVehicle : float32<E>
    Plane : PlaneModel
    PriorityPlane : float32<E>
    PriorityShells : float32<E>
}

/// Decide what vehicles and planes to produce, and how important they are.
let computeProductionPriorities (coalition : CoalitionId) (world : World) (state : WorldState) =
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state

    let shellNeed =
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
    
    let vehicleToProduce, vehicleNeed =
        let numHeavy, numMedium, numLight, need =
            let perRegion = GroundAttackVehicle.MediumTankCost * 10.0f
            state.Regions
            |> Seq.filter (fun state -> state.Owner = Some coalition) // Regions we control
            |> Seq.filter (fun state -> // Regions at the front
                let region = wg.GetRegion state.RegionId
                region.Neighbours
                |> Seq.exists (fun ngh -> sg.GetRegion(ngh).Owner <> Some coalition))
            |> Seq.map (fun state ->
                let numHeavy = state.GetNumVehicles HeavyTank
                let numMedium = state.GetNumVehicles MediumTank
                let numLight = state.GetNumVehicles LightArmor
                let desiredValue = 3.0f * GroundAttackVehicle.HeavyTankCost + 9.0f * GroundAttackVehicle.MediumTankCost + 3.0f * GroundAttackVehicle.LightArmorCost
                let availableValue = float32 numHeavy * GroundAttackVehicle.HeavyTankCost + float32 numMedium * GroundAttackVehicle.MediumTankCost + float32 numLight * GroundAttackVehicle.LightArmorCost
                numHeavy, numMedium, numLight, desiredValue - availableValue)
            |> Seq.fold (fun (t1, t2, t3, t4) (n1, n2, n3, n4) -> (t1 + n1, t2 + n2, t3 + n3, t4 + n4)) (0, 0, 0, 0.0f<E>)
        let vehicle =
            if numMedium = 0 then
                MediumTank
            elif numHeavy < numLight && 3 * numHeavy < numMedium then
                HeavyTank
            elif numLight >= numHeavy && 3 * numLight < numMedium then
                LightArmor
            else
                MediumTank
        vehicle, need

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
        let planeCost (plane : PlaneModel) =
            let numPlanesNeeded =
                match plane with
                | Bf109e7
                | Bf109f2
                | Mc202
                | I16
                | Mig3
                | P40 -> fighterCapacity - numFighters
                | IL2M41
                | Bf110e -> attackerCapacity - numAttackPlanes
                | _ -> bomberCapacity - numBombers - numTransports
            plane, plane.Cost * numPlanesNeeded
        if fighterUsage < 0.5f then
            match coalition with
            | Axis -> Bf109e7
            | Allies -> P40
        elif fighterUsage < 0.75f then
            match coalition with
            | Axis -> Mc202
            | Allies -> I16
        elif numAttackPlanes < attackerCapacity then
            match coalition with
            | Axis -> Bf110e
            | Allies -> IL2M41
        elif numBombers < bomberCapacity then
            match coalition with
            | Axis ->
                if numBombers / bomberCapacity > 0.75f then
                    Ju52
                else
                    Ju88a4
            | Allies ->
                Pe2s35
        elif fighterUsage < 1.0f then
            match coalition with
            | Axis -> Bf109f2
            | Allies -> Mig3
        else
            match coalition with
            | Axis -> Bf109e7
            | Allies -> I16
        |> planeCost
    let plane, planeNeed =
        try
            world.Airfields
            |> Seq.map (fun af -> af, sg.GetAirfield af.AirfieldId)
            |> Seq.filter (fun (af, afs) -> sg.GetRegion(af.Region).Owner = Some coalition)
            |> Seq.map planeNeed
            |> Seq.groupBy fst
            |> Seq.map (fun (plane, needs) -> plane, needs |> Seq.sumBy snd)
            |> Seq.maxBy snd
        with
        | _ ->
            match coalition with
            | Axis -> Bf109e7, 0.0f<E>
            | Allies -> I16, 0.0f<E>
    { Vehicle = vehicleToProduce
      PriorityVehicle = vehicleNeed
      Plane = plane
      PriorityPlane = planeNeed
      PriorityShells = shellNeed
    }


let computeNewState (dt : float32<H>) (world : World) (state : WorldState) (supplies : Resupplied list) (damages : Damage list) =
    let wg = WorldFastAccess.Create world
    let damages =
        let data =
            damages
            |> Seq.groupBy (fun dmg -> dmg.Object)
            |> Seq.map (fun (victim, damages) -> victim, damages |> Seq.map (fun dmg -> dmg.Data))
        Map.ofSeq data
    let supplies =
        let data =
            supplies
            |> Seq.groupBy (fun sup -> sup.Region)
            |> Seq.map (fun (reg, sups) -> reg, sups |> Seq.sumBy (fun sup -> sup.Energy))
        Map.ofSeq data
    let regionsAfterDamages =
        [
            for regState in state.Regions do
                let region = wg.GetRegion regState.RegionId
                let prodHealth =
                    regState.ProductionHealth
                    |> List.mapi (fun idx health ->
                        match Map.tryFind (Production(region.RegionId, idx)) damages with
                        | Some damages ->
                            damages
                            |> Seq.sumBy (fun data -> data.Amount)
                            |> (-) health
                        | None ->
                            health)
                let storeHealth =
                    regState.StorageHealth
                    |> List.mapi (fun idx health ->
                        match Map.tryFind (Storage(region.RegionId, idx)) damages with
                        | Some damages ->
                            damages
                            |> Seq.sumBy (fun data -> data.Amount)
                            |> (-) health
                        | None ->
                            health)
                // TODO: canon losses
                yield { regState with ProductionHealth = prodHealth; StorageHealth = storeHealth}
        ]
    ()