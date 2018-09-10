﻿// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
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

module Campaign.AfterActionReport

open Campaign.WorldDescription
open Campaign.PlaneModel
open Campaign.WorldState
open Campaign.BasicTypes
open Campaign.NewWorldState

let mkTankReport (m : Map<GroundAttackVehicle, int>) =
    m
    |> Map.toSeq
    |> Seq.map (fun (tank, qty) -> sprintf "%s: %d" (tank.Description) qty)
    |> String.concat ", "

type Campaign.NewWorldState.BattleSummary
with
    member this.GetText() =
        seq {
            yield sprintf "<u>Battle for %s</u> owned by %s<br>" (this.Region.ToString()) (this.Participants.DefenderCoalition.ToString())
            yield sprintf "Defenders: %s (+%1.0f%% -%1.0f%%)<br>" (mkTankReport this.Participants.Defenders) (100.0f * (this.Participants.DefenderBonus - 1.0f)) (100.0f * this.DamageToDefendersFromAir)
            yield sprintf "Attackers: %s (+%1.0f%% -%1.0f%%)<br>" (mkTankReport this.Participants.Attackers) (100.0f * (this.Participants.AttackerBonus - 1.0f)) (100.0f * this.DamageToAttackersFromAir)
            yield sprintf "Victors: %s %s<br>" (this.Victors.ToString()) (mkTankReport this.Survivors)
            yield sprintf "Damaged supplies: %3.0f<br>" this.CollateralDamage
        }

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
        seq {
            yield sprintf "<u>%s</u> %s<br>"
                    (coalition.ToString())
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
                    |> sprintf "Planes damaged:<br>%s<br>"
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

let buildReport (world : World) (oldState : WorldState) (newState : WorldState) (tookOff : TookOff list) (landed : Landed list) (damages : Damage list) (columns : ColumnMovement list) (newSupplies : Resupplied list) (newVehicles : NewVehiclesSummary) (battleReports : BattleSummary list) (coalition : CoalitionId) =
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
            |> List.map (fun column -> column.OrderId, column)
            |> Map.ofList
        seq {
            for dmg in Damage.GroupByObject damages do
                match dmg.Object with
                | Column colDmg ->
                    match columns.TryFind colDmg.OrderId with
                    | Some ({ TransportType = ColByRoad } as order) ->
                        if dmg.Data.Amount > 0.25f then
                            yield order.Composition.[colDmg.Rank]
                    | Some ({ TransportType = ColByRiverShip } as order)
                    | Some ({ TransportType = ColBySeaShip } as order) ->
                        if dmg.Data.Amount >= 1.0f then
                            let subCompo =
                                if colDmg.Rank = 0 then
                                    order.Composition.[0..order.Composition.Length/2]
                                else
                                    order.Composition.[order.Composition.Length/2..]
                            for tank in subCompo do
                                yield tank
                    | Some ({ TransportType = ColByTrain } as order) ->
                        if dmg.Data.Amount >= 1.0f then
                            for t in order.Composition do
                                yield t
                    | None ->
                        ()
                | _ ->
                    ()
        }
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
    let numTanksDestroyed =
        battleReports
        |> Seq.map (fun report -> report.Losses.TryFind coalition |> Option.defaultValue Map.empty)
        |> Seq.fold Util.addMaps numParkedTanksDestroyed
    let productionDamage =
        damages
        |> List.choose (fun event ->
            match event.Object with
            | Production(region, idx, _) ->
                if sg1.GetRegion(region).Owner = Some coalition then
                    let group = wg.GetRegion(region).Production.[idx]
                    let groupSize = group.SubBlocks(world.SubBlockSpecs).Length
                    Some (group, event.Data.Amount / float32 groupSize)
                else
                    None
            | _ -> None)
        |> List.sumBy (fun (group, damage) -> group.Production(world.SubBlockSpecs, world.ProductionFactor) * damage)
    let storageDamage =
        damages
        |> List.choose (fun event ->
            match event.Object with
            | Storage(region, idx, _) ->
                if sg1.GetRegion(region).Owner = Some coalition then
                    let group = wg.GetRegion(region).Storage.[idx]
                    let groupSize = group.SubBlocks(world.SubBlockSpecs).Length
                    Some (group, event.Data.Amount / float32 groupSize)
                else
                    None
            | _ -> None)
        |> List.sumBy (fun (building, damage) -> building.Storage(world.SubBlockSpecs) * damage)
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
      VehiclesLost = Util.addMaps tanksIntercepted numTanksDestroyed
      VehiclesProduced = newVehicles.Tanks
      FactoriesDestroyed = productionDamage
      SupplyProduced = newSupplies
      StorageDestroyed = storageDamage }

let pseudoHtmlToMarkdown (s : string) =
    s.Replace("<br>", "\n").Replace("<u>", "__").Replace("</u>", "__")