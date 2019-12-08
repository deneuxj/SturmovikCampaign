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
open System.Collections.Generic

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

/// The overall status of the war.
type WarState =
    {
        World : World
        RegionOwners : IDictionary<RegionId, CoalitionId>
        BuildingPartFillLevel : IDictionary<BuildingInstanceId * int, float32>
        BuildingPartHealthLevel : IDictionary<BuildingInstanceId * int, float32>
        AirfieldPlanes : IDictionary<AirfieldId, Dictionary<PlaneModel, float32>>
        Date : DateTime
        MissionRecords : MissionRecord list
    }
with
    /// Relative level of space used in a subpart, relatively to full health availability
    member this.GetBuildingPartFillLevel(bid, part) =
        match this.BuildingPartFillLevel.TryGetValue((bid, part)) with
        | true, x -> x
        | false, _ -> 0.0f

    member this.SetBuildingPartFillLevel(bid, part, x) =
        if x = 0.0f then
            this.BuildingPartFillLevel.Remove((bid, part)) |> ignore
        else
            this.BuildingPartFillLevel.[(bid, part)] <- x

    /// Amount of resources stored
    member this.GetBuildingStorage(bid) =
        let building = this.World.Buildings.[bid]
        building.Properties.SubParts
        |> List.sumBy (fun part -> this.GetBuildingPartFillLevel(bid, part))
        |> (*) building.Properties.PartCapacity

    /// Level of health of a subpart of a building
    member this.GetBuildingPartHealthLevel(bid, part) =
        match this.BuildingPartHealthLevel.TryGetValue((bid, part)) with
        | true, x -> x
        | false, _ -> 1.0f

    /// Max amount of resources that can be stored, depending on health but not taking into account current amounts.
    member this.GetBuildingCapacity(bid) =
        let building = this.World.Buildings.[bid]
        building.Properties.SubParts
        |> List.sumBy (fun part -> this.GetBuildingPartFunctionalityLevel(bid, part))
        |> (*) building.Properties.PartCapacity

    /// Level of functionality of a subpart of a building.
    member this.GetBuildingPartFunctionalityLevel(bid, part) =
        let health = this.GetBuildingPartHealthLevel(bid, part)
        if health < 0.5f then
            0.0f
        else
            health

    member this.SetBuildingPartHealthLevel(bid, part, x) =
        if x = 1.0f then
            this.BuildingPartHealthLevel.Remove((bid, part)) |> ignore
        else
            this.BuildingPartHealthLevel.[(bid, part)] <- x

    /// Assign resources to a building. Returns unassigned resources.
    member this.AssignResources (bid : BuildingInstanceId, rsc : float32<E>) =
        let building = this.World.Buildings.[bid]
        let partCapacity = building.Properties.PartCapacity
        let rsc =
            building.Properties.SubParts
            |> List.fold (fun rsc subpart ->
                let stored = partCapacity * this.GetBuildingPartFillLevel(bid, subpart)
                let capacity = partCapacity * this.GetBuildingPartFunctionalityLevel(bid, subpart)
                let transferred =
                    min rsc (capacity - stored)
                    |> max 0.0f<E>
                this.SetBuildingPartFillLevel(bid, subpart, (stored + transferred) / partCapacity)
                rsc - transferred
            ) rsc
        rsc

    /// Assign resources to multiple buildings. Returns unassigned resources.
    member this.DistributeResources (rsc : float32<E>) (buildings : BuildingInstanceId list) =
        // Group buildings by proximity
        let groups =
            let areClose (b1 : BuildingInstanceId) (b2 : BuildingInstanceId) =
                let b1 = this.World.Buildings.[b1]
                let b2 = this.World.Buildings.[b2]
                (b1.Pos.Pos - b2.Pos.Pos).Length() < 100.0f
            Util.Algo.computePartition areClose buildings
        // Order by fill level. We distribute in priority to group that already have 50% to 75% fill level,
        // then to groups that are less than 50%, and finally to groups that are more than 75%
        let groups =
            groups
            |> List.sortByDescending (fun group ->
                let stored = group |> List.sumBy this.GetBuildingStorage
                let capacity = group |> List.sumBy this.GetBuildingCapacity
                if capacity <= 0.0f<E> then
                    1, None
                else
                    match stored / capacity with
                    | x when x > 0.75f -> 2, Some (1.0f - x)
                    | x when x < 0.5f -> 3, Some x
                    | x -> 4, Some x)
        // Assign resources to sorted buildings
        let rsc =
            groups
            |> List.concat
            |> List.fold (fun rsc bid ->
                let rsc = this.AssignResources(bid, rsc)
                rsc) rsc
        // Result
        rsc

[<RequireQualifiedAccess>]
module Init =

    let private mutableDict (xs : ('K * 'V) seq) =
        let dict = Dictionary(HashIdentity.Structural)
        for k, v in xs do
            dict.Add(k, v)
        dict

    /// Create the initial status of the war
    let mkWar (world : World) =
        let regionOwners =
            world.Regions
            |> List.choose (fun desc-> desc.InitialOwner |> Option.map (fun owner -> desc.RegionId, owner))
            |> mutableDict
        let airfields =
            world.Airfields
            |> Seq.map (fun af -> af.AirfieldId, mutableDict [])
            |> mutableDict
        {
            World = world
            RegionOwners = regionOwners
            BuildingPartFillLevel = mutableDict []
            BuildingPartHealthLevel = mutableDict []
            AirfieldPlanes = airfields
            Date = world.StartDate
            MissionRecords = []
        }