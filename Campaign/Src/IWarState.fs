// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2021 Johann Deneux <johann.deneux@gmail.com>
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

/// Interfaces or WarState
namespace Campaign.WarState

open System
open System.Numerics
open System.Collections.Generic
open FSharp.Json
open Util
open Util.Json

open Campaign.Common.BasicTypes
open Campaign.Common.PlaneModel
open Campaign.Common.Targets
open Campaign.Common.Buildings
open Campaign.Common.Weather

open Campaign.NewWorldDescription
open SturmovikMission
open Campaign.Pilots

type IWarStateQuery =
    /// Deterministically get a seed for random generators
    abstract member Seed : int
    /// Get the immutable description of the world
    abstract member World : World
    /// Get the current date and time
    abstract member Date : DateTime
    /// Get the weather
    abstract member Weather : WeatherState
    /// Level of health of a subpart of a building or bridge
    abstract member GetBuildingPartHealthLevel : BuildingInstanceId * int -> float32
    /// Get buildings and parts that have damages
    abstract member BuildingDamages : (BuildingInstanceId * int * float32) seq
    /// Storage room in a building, taking health into account.
    abstract member GetBuildingCapacity : BuildingInstanceId -> float32<M^3>
    /// Storage room in a building, assuming full health.
    abstract member GetBuildingFullCapacity : BuildingInstanceId -> float32<M^3>
    /// Level of functionality of a subpart of a building or bridge
    abstract member GetBuildingPartFunctionalityLevel : BuildingInstanceId * int -> float32
    /// Level of functionality of a building
    abstract member GetBuildingFunctionalityLevel : BuildingInstanceId -> float32
    /// Get the relative processing capacity of a region. This depends on the regions' building capacity vs its full building capacity
    abstract member GetRegionProcessingLevel : RegionId -> float32
    /// Level of functionality of a bridge
    abstract member GetBridgeFunctionalityLevel : BuildingInstanceId -> float32
    /// Get the ground forces of a coalition in a region
    abstract member GetGroundForces : CoalitionId * RegionId -> float32<MGF>
    /// Get transport link capacity
    abstract member GetFlowCapacity : NetworkLink -> float32<M^3/H>
    /// Compute a mapping from a coalition to distances (in regions) to regions owned by this coalition
    abstract member ComputeDistancesToCoalition : CoalitionId -> IDictionary<RegionId, int>
    /// Compute a mapping from a regions to distances (in regions) to the closest region with an airfield
    abstract member ComputeDistancesToAirfields : unit -> IDictionary<RegionId, int>
    /// Compute a mapping from pairs of regions to road transport capacity between these two regions
    abstract member ComputeRoadCapacity : unit -> (RegionId * RegionId -> float32<M^3/H>)
    /// Compute a mapping from pairs of regions to rail transport capacity between these two regions
    abstract member ComputeRailCapacity : unit -> (RegionId * RegionId -> float32<M^3/H>)
    /// Compute a mapping from regions to amount of supplies that can reach it from the rear regions every hour
    abstract member ComputeSupplyAvailability : unit -> (RegionId -> float32<E/H>)
    /// Get a mapping denoting the number of each plane model at an airfield
    abstract member GetNumPlanes : AirfieldId -> Map<PlaneModelId, float32>
    /// Get the owner of a region
    abstract member GetOwner : RegionId -> CoalitionId option
    /// Try to get a player by its GUID
    abstract member TryGetPlayer : string -> Player option
    /// Get all players
    abstract member Players : Player list
    /// Get all pilots
    abstract member Pilots : Pilot list
    /// Get number of pilots
    abstract member NumPilots : int
    /// Get a pilot by its ID
    abstract member GetPilot : PilotId -> Pilot
    /// Find a path through road/rail network from one set of nodes to another, optionally restricted within the territory of a coalition
    abstract member TryFindPath : network : Network * sources: NetworkNode list * objectives: NetworkNode list * coalition: CoalitionId option -> NetworkLink list option
    /// Get the plane and weapon mod budget of a coalition
    abstract member GetCoalitionBudget : CoalitionId -> float32

[<AutoOpen>]
module IWarStateExtensions = 
    type IWarStateQuery with
        member this.GetNumPlanes(afId, model) =
            this.GetNumPlanes(afId)
            |> Map.tryFind model
            |> Option.defaultValue 0.0f

        member this.GetAirfieldCapacity(afid) =
            this.World.Airfields.[afid].Facilities
            |> List.sumBy this.GetBuildingCapacity

        member this.GetBuildingHealth(bid) =
            match this.World.TryGetBuildingInstance(bid) with
            | Some building ->
                match building.Properties.SubParts.Length with
                | n when n > 0 ->
                    let sum =
                        building.Properties.SubParts
                        |> List.sumBy (fun part -> this.GetBuildingPartHealthLevel(bid, part))
                    sum / (float32 n)
                | _ ->
                    1.0f
            | None ->
                1.0f

        /// Get the amount of resources available for anti-air defenses.
        member this.ComputeRegionAntiAirBudget(transport : RegionId * RegionId -> float32<M^3/H>, supplies : RegionId -> float32<M^3/H>, rId : RegionId, coalition : CoalitionId) =
            let owner = this.GetOwner(rId)
            let resupplyPeriod = 24.0f<H>
            let region = this.World.Regions.[rId]
            let viaGround =
                if owner = Some coalition then
                    supplies rId
                else
                    region.Neighbours
                    |> Seq.filter (fun ngh -> this.GetOwner(ngh) = Some coalition)
                    |> Seq.sumBy (fun ngh -> min (transport(ngh, rId)) (supplies ngh))
            let viaAir =
                if owner = Some coalition then
                    this.World.Airfields.Values
                    |> Seq.filter (fun af -> af.Region = rId)
                    |> Seq.sumBy (fun af -> this.GetAirfieldCapacity(af.AirfieldId))
                else
                    0.0f<M^3>
            let viaAir = viaAir / resupplyPeriod

            let troops =
                this.GetGroundForces(coalition, rId) * this.World.GroundForcesCost * this.World.AntiAirGroundForcesRatio * this.World.ResourceVolume

            min troops (viaGround + viaAir)

        member this.IsPilotHealthy(pilotId : PilotId) =
            match this.GetPilot(pilotId).Health with
            | Healthy -> true
            | Dead -> false
            | Injured(until) -> until < this.Date

        member this.AirfieldsOfCoalition(coalition : CoalitionId) =
            this.World.Airfields.Values
            |> Seq.filter(fun af -> this.GetOwner(af.Region) = Some coalition)

        member this.IsPilotAvailableFrom(pilotId : PilotId, afId : AirfieldId) =
            let pilot = this.GetPilot(pilotId)
            // Early exit if the owner of the airfield is not the pilot's coalition
            match this.GetOwner(this.World.Airfields.[afId].Region) with
            | None -> false
            | Some coalition when this.World.Countries.[pilot.Country] <> coalition -> false
            | _ ->
            match pilot.Flights |> List.tryLast with
            | None ->
                // No flights yet: pilot can start from any airfield
                true
            | Some { Return = AtAirfield afId2 } ->
                // Can start if airfield is same as landing of latest recorded mission
                afId2 = afId
            | Some { Return = CrashedInFriendlyTerritory(Some afId2) } ->
                afId2 = afId
            | Some { Return = CrashedInFriendlyTerritory None } ->
                false
            | Some { Return = CrashedInEnemyTerritory } ->
                false
            | Some { Return = KilledInAction } ->
                false

        member this.TryGetReturnAirfield(flight : FlightRecord) =
            match flight.Return with
            | CrashedInEnemyTerritory -> None
            | CrashedInFriendlyTerritory afId -> afId
            | AtAirfield afId -> Some afId
            | KilledInAction -> None

        member this.TryGetPilotHome(pilotId) =
            let pilot = this.GetPilot(pilotId)
            pilot.Flights
            |> List.tryLast
            |> Option.bind (fun flight -> this.TryGetReturnAirfield(flight))

        member this.GetNewNames(country : CountryId, seed : int, isFemale) : string * string =
            let random = System.Random(seed)
            let firstNames =
                this.World.Names.TryFindFirstNames(country, isFemale)
                |> Option.defaultValue [||]
            let lastNames =
                this.World.Names.TryFindLastNames(country, isFemale)
                |> Option.defaultValue [||]
            let firstName =
                if Array.isEmpty firstNames then
                    "J."
                else
                    Array.item (random.Next(Array.length firstNames)) firstNames
            let lastName =
                if Array.isEmpty lastNames then
                    "D."
                else
                    Array.item (random.Next(Array.length lastNames)) lastNames
            firstName, lastName

        /// Return a new pilot, without registering it. The name is deterministically selected in a pseudo-random manner.
        /// The seed is computed from the date of the war state, the scenario, the country, the player GUID and number of existing pilots.
        member this.NewPilot(guid : string, country : CountryId, isFemale) : Pilot =
            let id = PilotId(Guid.NewGuid())
            let seed = hash(this.Date, this.World.Scenario, country, guid, this.NumPilots)
            let firstName, lastName = this.GetNewNames(country, seed, isFemale)
            { Id = id
              Country = country
              IsFemaleOpt = Some isFemale
              PilotFirstName = firstName
              PilotLastName = lastName
              PlayerGuid = guid
              Health = Healthy
              Flights = []
              InitialAwards = []
              InitialAirKills = 0
              InitialNumFlights = 0
            }

        member this.GetPlayerPilots(guid : string) =
            this.Pilots
            |> Seq.filter (fun pilot -> pilot.PlayerGuid = guid)
            |> Seq.sortBy (fun pilot -> pilot.Id)

        /// Check if the start or end of the mission is outside of day-time
        member this.HasLowLight(missionLength : TimeSpan) =
            let sunrise, sunset = Util.suntimes(this.Date)
            [this.Date; this.Date + missionLength]
            |> List.exists (fun t -> t <= sunrise || t >= sunset)

        /// Compute the enemy forces in neighbouring regions
        member this.GroundThreatsToRegion(region : RegionId, friendly : CoalitionId) =
            let neighbours = this.World.Regions.[region].Neighbours
            neighbours
            |> Seq.map (fun ngh -> this.GetGroundForces(friendly.Other, ngh))
            |> Seq.sum

        /// Total capacity of industry buildings in a region, taking health into account
        member this.GetRegionBuildingCapacity(rid : RegionId) =
            this.World.Regions.[rid].IndustryBuildings
            |> Seq.sumBy this.GetBuildingCapacity

        /// Total capacity of industry buildings in a region, assuming full health
        member this.GetRegionBuildingFullCapacity(rid : RegionId) =
            this.World.Regions.[rid].IndustryBuildings
            |> Seq.sumBy this.GetBuildingFullCapacity

        /// Total capacity of airfield buldings in an airfield, taking health into account
        member this.GetAirfieldBuildingCapacity(afId : AirfieldId) =
            this.World.Airfields.[afId].Facilities
            |> Seq.sumBy this.GetBuildingCapacity

        /// Total capacity of airfield buildings in an airfield, assuming full health
        member this.GetAirfieldBuildingFullCapacity(afId : AirfieldId) =
            this.World.Airfields.[afId].Facilities
            |> Seq.sumBy this.GetBuildingFullCapacity


type IWarStateUpdate =
    /// Set the date and time
    abstract member SetDate : DateTime -> unit
    /// Set the weather
    abstract member SetWeather : WeatherState -> unit
    /// Set the level of health of a subpart of a building or a bridge
    abstract member SetBuildingPartHealthLevel : BuildingInstanceId * int * float32 -> unit
    /// Set the ground forces of a coalition in a region
    abstract member SetGroundForces : CoalitionId * RegionId * float32<MGF> -> unit
    /// Set the number of planes of a given model available at a given airfield
    abstract member SetNumPlanes : AirfieldId * PlaneModelId * float32 -> unit
    /// Set the owner of a region
    abstract member SetOwner : RegionId * CoalitionId option -> unit
    /// Add/update player
    abstract member UpdatePlayer : Player -> unit
    /// Add/update pilot
    abstract member UpdatePilot : Pilot * bool -> unit
    /// Refresh pilots healths: Pilots who have healed become healthy again
    abstract member RefreshPilotHealths : unit -> unit

type IWarState =
    inherit IWarStateQuery
    inherit IWarStateUpdate
