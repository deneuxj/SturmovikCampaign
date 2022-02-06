// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
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

/// Types to maintain the state of a campaign and plan missions
namespace Campaign.CampaignScenario

open System.Numerics
open VectorExtension
open Util

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel

open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.Missions
open Campaign.MissionSelection

/// A step in the sequence of rounds making up a campaign.
type ScenarioStep =
    | Stalemate of string
    | Victory of CoalitionId * string
    | Ongoing of StepData

/// Content of a round
and StepData =
    {
        Briefing : string
        Missions : Mission list
        /// Can be used by implementation to store scenario-specific data
        Data : obj
    }

/// Interface of scenario-specific world setup functions, e.g. set the planeset
type IScenarioWorldSetup =
    abstract member Setup : World -> World

/// Interface of scenario controllers
type IScenarioController =
    /// Set ground forces in each region
    abstract member InitGroundForces : axisForcesNumberCoefficient: float32 * alliesForcesNumberCoefficient: float32 * IWarState -> unit
    /// Set plane numbers at the airfields of a coalition
    abstract member InitAirfields : planeNumberCoefficient: float32 * CoalitionId * IWarState -> unit
    abstract member Start : IWarStateQuery * float32<H> -> ScenarioStep
    abstract member NewDay : IWarStateQuery -> (Commands option * string) seq
    abstract member NextStep : StepData -> (IWarStateQuery * float32<H> -> ScenarioStep)
    abstract member TrySelectMissions : StepData * IWarStateQuery * seed:int * numSelected:int -> MissionSelection option
    abstract member SelectMissions : StepData * IWarStateQuery * seed:int -> Mission list
    abstract member DeserializeStepData : string -> obj

/// Resources available at an airfield.
/// Decreased whenever a flight is planned taking off from that airfield.
/// Utility class used by scenario controllers when planning missions.
type AirfieldStatus =
    {
        Resources : float32<M^3>
        Planes : Map<PlaneModelId, float32>
    }
with
    member this.TryCheckout((plane, resourcesPerPlane : float32<M^3>, numPlanes)) =
        let qty = this.Planes.TryFind(plane) |> Option.defaultValue 0.0f
        let qty = qty - numPlanes
        let rsc = this.Resources - resourcesPerPlane * numPlanes
        if qty >= 0.0f && rsc >= 0.0f<M^3> then
            Some {
                this with
                    Resources = rsc
                    Planes = this.Planes.Add(plane, qty)
            }
        else
            None

/// Resources, and planes available at airfields, ground forces available in regions.
/// Utility class used by scenario controllers when planning missions.
type ForcesAvailability =
    {
        Airfields : Map<AirfieldId, AirfieldStatus>
        Regions : Map<RegionId * CoalitionId, float32<MGF>>
    }
with
    static member Create(war : IWarStateQuery) =
        {
            Airfields =
                war.World.Airfields.Keys
                |> Seq.map (fun afid ->
                    afid,
                    { Resources = war.GetAirfieldCapacity(afid); Planes = war.GetNumPlanes(afid) } )
                |> Map.ofSeq
            Regions =
                war.World.Regions.Keys
                |> Seq.collect (fun rid ->
                    [Axis; Allies]
                    |> Seq.map (fun coalition -> (rid, coalition), war.GetGroundForces(coalition, rid)))
                |> Map.ofSeq
        }

    member this.TryCheckoutPlane(afid, data) =
        let afs =
            this.Airfields.[afid]
        let afs = afs.TryCheckout(data)
        match afs with
        | Some afs ->
            Some {
                this with
                    Airfields = this.Airfields.Add(afid, afs)
            }
        | None ->
            None

    member this.TryCheckoutGroundForce(coalition, rid, force) =
        let available =
            this.Regions.TryFind((rid, coalition))
            |> Option.defaultValue 0.0f<MGF>
        if available >= force then
            Some { this with Regions = this.Regions.Add((rid, coalition), available - force) }
        else
            None

/// Interface of adapters from ground target types to data useful to mission planners
type TargetAdapter<'Target> =
    abstract GetPos : 'Target -> Vector2
    abstract MkGroundTarget : 'Target -> GroundTargetType
    abstract GetRegion : 'Target -> RegionId

/// Adapter to plan raids on airfields
type AirfieldTargetAdapter() =
    interface TargetAdapter<Airfield> with
        member __.GetPos(af) = af.Position
        member __.MkGroundTarget(af) = AirfieldTarget af.AirfieldId
        member __.GetRegion(af) = af.Region

/// Adapter to plan raids on buildings in a region
type RegionTargetAdapter() =
    interface TargetAdapter<Region> with
        member __.GetPos(region) = region.Position
        member __.MkGroundTarget(region) = BuildingTarget
        member __.GetRegion(region) = region.RegionId

/// Adapter to plan raids on ground forces in a region
type GroundForcesTargetAdapter(side) =
    interface TargetAdapter<Region> with
        member __.GetPos(region) = region.Position
        member __.MkGroundTarget(region) = GroundForces side
        member __.GetRegion(region) = region.RegionId

/// Result of mission planning attempt
type MissionPlanningResult =
    | InsufficientResources of string
    | Unnecessary of string
    | Plan of string * Mission list * ForcesAvailability
with
    member this.LacksResources =
        match this with
        | InsufficientResources _ -> true
        | Unnecessary _ | Plan _  -> false

    member this.Description =
        match this with
        | InsufficientResources s
        | Unnecessary s
        | Plan(s, _, _) -> s

/// Utility functions for mission planning results
module MissionPlanningResult =
    let getMissions originalBudget =
        function
        | InsufficientResources _
        | Unnecessary _ -> [], originalBudget
        | Plan(_, missions, budget) -> missions, budget

    let hasMissions =
        function
        | Unnecessary _ | Plan(_, [],_) | InsufficientResources _ -> false
        | Plan(_, _ :: _, _) -> true

    let firstWithNonEmptyPlan (xs : MissionPlanningResult seq) =
        let proj =
            function
            | Plan(_, [], _) -> 0
            | InsufficientResources _ -> 1
            | Unnecessary _ -> 2
            | Plan(_, _ :: _, _) -> 3
        let pred =
            function
            | Plan(_, _ :: _, _) -> true
            | _ -> false
        xs
        |> Seq.maxByUntil proj pred

/// Combinators for mission planning functions
module Planning =
    /// Perform plans in a sequence, unconditionally
    let chain planners =
        fun budget ->
            let descriptions, ms, b =
                (([], [], budget), planners)
                ||> Seq.fold (fun (descriptions, missions, budget) planner ->
                    let plan = planner budget
                    let missions2, budget = MissionPlanningResult.getMissions budget plan
                    let descriptions =
                        match missions2 with
                        | [] -> descriptions
                        | _ :: _ -> descriptions @ [plan.Description]
                    (descriptions, missions @ missions2, budget)
                )
            let description =
                match descriptions with
                | [] -> "Idle"
                | [s] -> s
                | xs -> String.concat ", " xs
            Plan(description, ms, b)

    /// Perform the first plan, then continue with the rest.
    let always rest first = chain [first; rest]

    /// If the first planning results in no missions, stop. Otherwise, continue with the rest.
    let andThen rest first =
        fun budget ->
            match first budget with
            | Plan(description, ((_ :: _) as missions), budget) ->
                let missions2, budget =
                    rest budget
                    |> MissionPlanningResult.getMissions budget
                Plan(description, missions @ missions2, budget)
            | nogo -> nogo

    /// Pick the first planning that results in the non-empty list of missions
    let pickFirst alternatives =
        fun budget ->
            alternatives
            |> Seq.map (fun f -> f budget)
            |> MissionPlanningResult.firstWithNonEmptyPlan

module IO =
    open FSharp.Json
    open Util.Json
    open System.IO

    type ScenarioStep with
        member this.Serialize(writer : StreamWriter) =
            let json = Json.serializeEx JsonConfig.IL2Default this
            writer.Write(json)

        static member Deserialize(reader : StreamReader, deserializeData) =
            let json = reader.ReadToEnd()
            let data : ScenarioStep = Json.deserializeEx JsonConfig.IL2Default json
            let data =
                match data with
                | Ongoing stepData ->
                    let stepDataJson = Json.serializeEx JsonConfig.IL2Default stepData.Data
                    Ongoing { stepData with Data = deserializeData stepDataJson }
                | other ->
                    other
            data

        member this.SaveToFile(path : string) =
            if File.Exists(path) then
                File.Delete(path)
            use writer = new StreamWriter(path)
            this.Serialize(writer)