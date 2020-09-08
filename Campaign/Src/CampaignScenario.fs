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
    /// Set plane numbers at the airfields of a coalition
    abstract member InitAirfields : planeNumberCoefficient: float32 * CoalitionId * IWarState -> unit
    abstract member Start : IWarStateQuery * float32<H> -> ScenarioStep
    abstract member NewDay : IWarStateQuery -> (Commands option * string) seq
    abstract member NextStep : StepData -> (IWarStateQuery * float32<H> -> ScenarioStep)
    abstract member TrySelectMissions : StepData * IWarStateQuery * seed:int * numSelected:int -> MissionSelection option

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
    | TooFewTargets of string
    | Plan of string * Mission list * ForcesAvailability
with
    member this.LacksResources =
        match this with
        | TooFewTargets _ | Plan(_, _ :: _, _) -> false
        | Plan(_, [], _) -> true

/// Utility functions for mission planning results
module MissionPlanningResult =
    let getMissions originalBudget =
        function
        | TooFewTargets _ -> [], originalBudget
        | Plan(_, missions, budget) -> missions, budget

    let hasMissions =
        function
        | TooFewTargets _ | Plan(_, [],_) -> false
        | Plan(_, _ :: _, _) -> true

    let firstWithNonEmptyPlan (xs : MissionPlanningResult seq) =
        let proj =
            function
            | Plan(_, [], _) -> 0
            | TooFewTargets _ -> 1
            | Plan(_, _ :: _, _) -> 2
        let pred =
            function
            | Plan(_, _ :: _, _) -> true
            | _ -> false
        xs
        |> Seq.maxByUntil proj pred

/// Combinators for mission planning functions
module Planning =
    let chain planners =
        fun budget ->
            planners
            |> Seq.fold (fun (missions, budget) planner ->
                let plan = planner budget
                let missions2, budget = MissionPlanningResult.getMissions budget plan
                (missions @ missions2, budget)
            ) ([], budget)
            |> fun (m, b) -> Plan("Chain of missions", m, b)

    let andThen also first =
        fun budget ->
            match first budget with
            | Plan(description, ((_ :: _) as missions), budget) ->
                let missions2, budget =
                    (chain also) budget
                    |> MissionPlanningResult.getMissions budget
                Plan(description, missions @ missions2, budget)
            | nogo -> nogo

    let orElse alternatives =
        fun budget ->
            alternatives
            |> Seq.map (fun f -> f budget)
            |> MissionPlanningResult.firstWithNonEmptyPlan

module IO =
    open MBrace.FsPickler
    open System.IO

    type ScenarioStep with
        member this.Serialize(writer : StreamWriter) =
            let serializer = FsPickler.CreateXmlSerializer(indent = true)
            serializer.Serialize(writer, this)

        static member Deserialize(reader : StreamReader) =
            let serializer = FsPickler.CreateXmlSerializer(indent = true)
            serializer.Deserialize<ScenarioStep>(reader)

        member this.SaveToFile(path : string) =
            if File.Exists(path) then
                File.Delete(path)
            use writer = new StreamWriter(path)
            this.Serialize(writer)