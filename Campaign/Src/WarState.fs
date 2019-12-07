// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2019 Johann Deneux <johann.deneux@gmail.com>
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

namespace Campaign.WarState

open System
open System.Numerics

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription
open Campaign.NewWorldDescription

type TargetType =
    | Truck | Train | Ship | Battleship | Artillery | Tank | ArmoredCar | Bridge
    | Building of BuildingProperties * int
    | ParkedPlane of PlaneType
    | Air of PlaneType

type Target =
    {
        Kind : TargetType
        Pos : Vector2
    }

type AmmoType =
    Rocket | Bullets | Bomb

type ReturnType =
    CrashedInEnemyTerritory | CrashedInFriendlyTerritory | AtAirfield of AirfieldId

/// The results of a flight by a player, used to build success rates of virtual missions.
type MissionRecord =
    {
        Date : DateTime
        Plane : PlaneModel
        Start : AirfieldId
        TargetsDestroyed : (Target * AmmoType) list
        Return : ReturnType
    }

/// The health and usage of a building.
type BuildingStatus =
    {
        Instance : BuildingInstance
        Health : Map<int, float32>
        Stores : Map<int, float32>
    }
with
    static member Create(instance : BuildingInstance, fillLevel) =
        let filledStores =
            instance.Properties.SubParts
            |> List.map (fun part -> part, fillLevel)
            |> Map.ofList
        {
            Instance = instance
            Health = Map.empty
            Stores = filledStores
        }

    /// Level of health of a subpart.
    member this.SubPartHealthLevel (subpart) =
        this.Health.TryFind subpart
        |> Option.defaultValue 1.0f

    /// Level of functionality of a subpart of a building.
    member this.SubPartFunctionalityLevel (subpart) =
        let health = this.SubPartHealthLevel subpart
        if health < 0.5f then
            0.0f
        else
            health

    /// Max amount of resources that can be stored, not taking into account current amounts.
    member this.Capacity =
        this.Instance.Properties.SubParts
        |> List.sumBy this.SubPartFunctionalityLevel
        |> (*) this.Instance.Properties.PartCapacity

    /// Relative level of space used in a subpart, relatively to full health availability
    member this.SubPartFillLevel (subpart) =
        let level =
            this.Stores.TryFind subpart
            |> Option.defaultValue 0.0f
        level

    /// Amount of resources stored
    member this.Stored =
        this.Instance.Properties.SubParts
        |> List.sumBy this.SubPartFillLevel
        |> (*) this.Instance.Properties.PartCapacity

    /// Assign resources to a building. Returns unassigned resources in addition to new building status.
    member this.AssignResources (rsc : float32<E>) =
        let partCapacity = this.Instance.Properties.PartCapacity
        let stores, rsc =
            this.Instance.Properties.SubParts
            |> List.fold (fun (stores, rsc) subpart ->
                let stored = partCapacity * this.SubPartFillLevel subpart
                let capacity = partCapacity * this.SubPartFunctionalityLevel subpart
                let transferred =
                    min rsc (capacity - stored)
                    |> max 0.0f<E>
                Map.add subpart ((stored + transferred) / partCapacity) stores, rsc - transferred
            ) (this.Stores, rsc)
        { this with
            Stores = stores }, rsc

    /// Assign resources to multiple buildings. Returns unassigned resources in addition to new building statuses
    static member DistributeResources (rsc : float32<E>) (buildings : BuildingStatus list) =
        // Group buildings by proximity
        let groups =
            let areClose (b1 : BuildingStatus) (b2 : BuildingStatus) =
                (b1.Instance.Pos.Pos - b2.Instance.Pos.Pos).Length() < 100.0f
            Util.Algo.computePartition areClose buildings
        // Order by fill level. We distribute in priority to group that already have 50% to 75% fill level,
        // then to groups that are less than 50%, and finally to groups that are more than 75%
        let groups =
            groups
            |> List.sortByDescending (fun group ->
                let stored = group |> List.sumBy (fun this -> this.Stored)
                let capacity = group |> List.sumBy (fun this -> this.Capacity)
                if capacity <= 0.0f<E> then
                    1, None
                else
                    match stored / capacity with
                    | x when x > 0.75f -> 2, Some (1.0f - x)
                    | x when x < 0.5f -> 3, Some x
                    | x -> 4, Some x)
        // Assign resources to sorted buildings
        let buildings, rsc =
            groups
            |> List.concat
            |> List.fold (fun (buildings, rsc) building ->
                let building, rsc = building.AssignResources rsc
                building :: buildings, rsc) ([], rsc)
        // Result
        buildings, rsc

/// The owner of a region, and the status of its buildings.
type RegionStatus =
    {
        Properties : Region
        Owner : CoalitionId option
        Buildings : BuildingStatus list
    }
with
    member this.Capacity =
        this.Buildings
        |> List.sumBy (fun building -> building.Capacity)

/// The status of the buildings at an airfield, and the planes available there.
type AirfieldStatus =
    {
        Properties : Airfield
        Buildings : BuildingStatus list
        Planes : Map<PlaneModel, float32>
    }
with
    member this.Capacity =
        this.Buildings
        |> List.sumBy (fun building -> building.Capacity)

/// The overall status of the war.
type WarState =
    {
        World : World
        Regions : Map<RegionId, RegionStatus>
        Airfields : Map<AirfieldId, AirfieldStatus>
        Date : DateTime
        MissionRecords : MissionRecord list
    }

[<RequireQualifiedAccess>]
module Init =
    /// Create the initial status of a region
    let mkRegion (world : World) (region : RegionId) =
        let description =
            world.Regions
            |> List.find (fun reg -> reg.RegionId = region)
        let buildings =
            description.IndustryBuildings
            |> List.map (fun instance -> BuildingStatus.Create(instance, 1.0f))
        {
            Properties = description
            Owner = description.InitialOwner
            Buildings = buildings
        }

    /// Create the initial status of an airfield
    let mkAirfield (world : World) (airfield : AirfieldId) =
        let description =
            world.Airfields
            |> List.find (fun afs -> afs.AirfieldId = airfield)
        let buildings =
            description.Facilities
            |> List.map (fun instance -> BuildingStatus.Create(instance, 1.0f))
        {
            Properties = description
            Buildings = buildings
            Planes = Map.empty
        }

    /// Create the initial status of the war
    let mkWar (world : World) =
        let regions =
            world.Regions
            |> List.map (fun desc-> desc.RegionId, mkRegion world desc.RegionId)
            |> Map.ofList
        let airfields =
            world.Airfields
            |> List.map (fun desc -> desc.AirfieldId, mkAirfield world desc.AirfieldId)
            |> Map.ofList
        {
            World = world
            Regions = regions
            Airfields = airfields
            Date = world.StartDate
            MissionRecords = []
        }