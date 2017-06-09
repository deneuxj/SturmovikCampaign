module Campaign.AfterActionReport

open Campaign.WorldDescription
open Campaign.PlaneModel
open Campaign.WorldState
open Campaign.BasicTypes

type ReportData = {
    MissionDate : System.DateTime
    RegionsCaptured : RegionId list
    PlanesLost : Map<PlaneModel, int>
    PlanesDamaged : Map<PlaneModel, int>
    PlanesCaptured : Map<PlaneModel, int>
    PlanesProduced : Map<PlaneModel, int>
    VehiclesLost : Map<GroundAttackVehicle, int>
    VehiclesProduced : Map<GroundAttackVehicle, int>
    FactoriesDestroyed : float32<E/H>
    StorageDestroyed : float32<E>
    SupplyProduced : float32<E>
}
with
    member this.GetText(coalition) =
        let mkPlaneReport (m : Map<PlaneModel, int>) =
            m
            |> Map.toSeq
            |> Seq.map (fun (plane, qty) -> sprintf "%s: %d" plane.PlaneName qty)
            |> String.concat ", "
        let mkTankReport (m : Map<GroundAttackVehicle, int>) =
            m
            |> Map.toSeq
            |> Seq.map (fun (tank, qty) -> sprintf "%s: %d" (tank.Description) qty)
            |> String.concat ", "
        seq {
            yield sprintf "<b>%s</b> %s<br>"
                    (match coalition with Axis -> "Axis" | Allies -> "Allies")
                    (this.MissionDate.ToString("d MMM yyyy HH:mm"))
            if not(this.RegionsCaptured.IsEmpty) then
                yield
                    this.RegionsCaptured
                    |> List.map (fun (RegionId name) -> name)
                    |> String.concat ", "
                    |> sprintf "Regions conquered: %s.<br>"
            if not(this.PlanesLost.IsEmpty) then
                yield
                    this.PlanesLost
                    |> mkPlaneReport
                    |> sprintf "Plane losses:<br>%s<br>"
            if not(this.PlanesDamaged.IsEmpty) then
                yield
                    this.PlanesLost
                    |> mkPlaneReport
                    |> sprintf "Planes that sustained damage:<br>%s<br>"
            if not(this.PlanesCaptured.IsEmpty) then
                yield
                    this.PlanesCaptured
                    |> mkPlaneReport
                    |> sprintf "Planes captured by the enemy:<br>%s<br>"
            if not(this.PlanesProduced.IsEmpty) then
                yield
                    this.PlanesProduced
                    |> mkPlaneReport
                    |> sprintf "New planes:<br>%s<br>"
            if not(this.VehiclesLost.IsEmpty) then
                yield
                    this.VehiclesLost
                    |> mkTankReport
                    |> sprintf "Tank losses:<br>%s<br>"
            if not(this.VehiclesProduced.IsEmpty) then
                yield
                    this.VehiclesProduced
                    |> mkTankReport
                    |> sprintf "New tanks:<br>%s<br>"
            if this.FactoriesDestroyed > 0.0f<E/H> then
                yield sprintf "Production capability lost: -%d units per hour<br>" (this.FactoriesDestroyed |> float32 |> ceil |> int)
            if this.StorageDestroyed > 0.0f<E> then
                yield sprintf "Storage capacity lost: -%d units<br>" (this.StorageDestroyed |> float32 |> ceil |> int)
            yield sprintf "New supplies from factories: +%d units<br>" (this.SupplyProduced |> float32 |> ceil |> int)
        }
        |> String.concat ""

open Campaign.ResultExtraction
open Campaign.Orders
open NewWorldState

let regionsOwnedBy (state : WorldState) (coalition : CoalitionId) =
    state.Regions
    |> List.filter (fun region -> region.Owner = Some coalition)
    |> List.map (fun region -> region.RegionId)
    |> Set.ofList

let buildReport (world : World) (oldState : WorldState) (newState : WorldState) (tookOff : TookOff list) (landed : Landed list) (damages : Damage list) (columns : ColumnMovement list) (newSupplies : Resupplied list) (newVehicles : NewVehiclesSummary) (coalition : CoalitionId) =
    let wg = WorldFastAccess.Create world
    let sg1 = WorldStateFastAccess.Create oldState
    let sg2 = WorldStateFastAccess.Create newState
    let afOwnedByCoalition af =
        sg1.GetRegion(wg.GetAirfield(af).Region).Owner = Some coalition
    let regionsGained =
        Set.difference (regionsOwnedBy newState coalition) (regionsOwnedBy oldState coalition)
    let numPlanesLost =
        let numPlanesTookOff =
            tookOff
            |> List.filter (fun event -> afOwnedByCoalition event.Airfield)
            |> List.map (fun event -> event.Plane)
            |> Util.compactSeq
        let numPlanesLanded =
            landed
            |> List.filter (fun event -> afOwnedByCoalition event.Airfield && event.Health > 0.0f)
            |> List.map (fun event -> event.Plane)
            |> Util.compactSeq
        Util.subMaps numPlanesTookOff numPlanesLanded
        |> Map.filter (fun _ qty -> qty > 0)
    let numParkedPlanesDestroyed =
        damages
        |> List.choose (fun event ->
            match event.Object with
            | ParkedPlane(af, model) -> Some(af, model, event.Data.Amount)
            | _ -> None)
        |> List.filter (fun (af, model, damage) -> afOwnedByCoalition(af))
        |> List.map (fun (_, model, damage) -> model, damage)
        |> List.groupBy fst
        |> List.map (fun (model, damages) -> model, damages |> List.sumBy snd)
        |> Map.ofList
        |> Map.map (fun _ damaged -> damaged |> ceil |> int)
    let numPlanesLandedDamaged =
        landed
        |> List.filter (fun event -> event.Health > 0.0f && event.Health < 0.75f && afOwnedByCoalition event.Airfield)
        |> List.map (fun event -> event.Plane)
        |> Util.compactSeq
    let regionsLost =
        Set.difference (regionsOwnedBy oldState coalition) (regionsOwnedBy newState coalition)
    let planesCaptured =
        newState.Airfields
        |> List.filter (fun afState -> regionsLost.Contains(wg.GetAirfield(afState.AirfieldId).Region))
        |> List.filter (fun afState -> sg2.GetRegion(wg.GetAirfield(afState.AirfieldId).Region).Owner <> Some coalition)
        |> List.map (fun afState -> afState.NumPlanes |> Map.map (fun _ qty -> qty |> floor |> int))
        |> List.fold Util.addMaps Map.empty
        |> Map.filter (fun _ qty -> qty > 0)
    let tanksIntercepted =
        let columns =
            columns
            |> List.filter (fun column -> column.OrderId.Coalition = coalition)
            |> List.map (fun column -> column.OrderId.Index, column.Composition)
            |> Map.ofList
        damages
        |> List.choose (fun event ->
            match event.Object with
            | Column { OrderId = { Coalition = owner; Index = orderIndex }; Rank = rank } when owner = coalition && event.Data.Amount > 0.25f ->
                Some(orderIndex, rank)
            | _ -> None)
        |> List.choose (fun (order, vehicle) ->
            columns.TryFind order
            |> Option.map (fun composition -> composition.[vehicle]))
        |> Util.compactSeq
    let numParkedTanksDestroyed =
        damages
        |> List.choose (fun event ->
            match event.Object with
            | Vehicle(region, model) -> Some(region, model, event.Data.Amount)
            | _ -> None)
        |> List.filter (fun (region, model, damage) -> sg1.GetRegion(region).Owner = Some coalition)
        |> List.map (fun (_, model, damage) -> model, damage)
        |> List.groupBy fst
        |> List.map (fun (model, damages) -> model, damages |> List.sumBy snd)
        |> Map.ofList
        |> Map.map (fun _ damaged -> damaged |> ceil |> int)
    let productionDamage =
        damages
        |> List.choose (fun event ->
            match event.Object with
            | Production(region, idx) ->
                if sg1.GetRegion(region).Owner = Some coalition then
                    Some (wg.GetRegion(region).Production.[idx], event.Data.Amount)
                else
                    None
            | _ -> None)
        |> List.sumBy (fun (building, damage) -> building.Production * damage)
    let storageDamage =
        damages
        |> List.choose (fun event ->
            match event.Object with
            | Storage(region, idx) ->
                if sg1.GetRegion(region).Owner = Some coalition then
                    Some (wg.GetRegion(region).Storage.[idx], event.Data.Amount)
                else
                    None
            | _ -> None)
        |> List.sumBy (fun (building, damage) -> building.Storage * damage)
    let newSupplies =
        newSupplies
        |> List.filter (fun sup -> sg2.GetRegion(sup.Region).Owner = Some coalition)
        |> List.sumBy (fun sup -> sup.Energy)
    // Result
    { MissionDate = oldState.Date
      RegionsCaptured = regionsGained |> List.ofSeq
      PlanesLost = Util.addMaps numPlanesLost numParkedPlanesDestroyed
      PlanesDamaged = numPlanesLandedDamaged
      PlanesCaptured = planesCaptured
      PlanesProduced = newVehicles.Planes
      VehiclesLost = Util.addMaps tanksIntercepted numParkedTanksDestroyed
      VehiclesProduced = newVehicles.Tanks
      FactoriesDestroyed = productionDamage
      SupplyProduced = newSupplies
      StorageDestroyed = storageDamage }

