﻿namespace Campaign.WarState

open System
open System.Numerics

open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.WorldDescription
open Campaign.NewWorldDescription

type TargetType =
    | Truck | Train | Ship | Battleship | Artillery | Tank | ArmoredCar | Building  | Bridge | ParkedPlane
    | Air of PlaneType * float32

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

type GroundDamage =
    {
        Instance : BuildingInstance
        Health : Map<int, float32>
    }
with
    static member CountResources (buildings : BuildingInstance list) (damages : Map<OrientedPosition, GroundDamage>) =
        buildings
        |> List.sumBy (fun building ->
            let damages =
                damages.TryFind building.Pos
                |> Option.map (fun dmg -> dmg.Health)
                |> Option.defaultValue Map.empty
            let health =
                building.Properties.SubParts
                |> List.sumBy (fun part ->
                    damages.TryFind part
                    |> Option.defaultValue 1.0f
                    |> function
                       | x when x <= 0.5f -> 0.0f
                       | x -> x)
            let health =
                match building.Properties.SubParts.Length with
                | 0 -> health
                | n -> health / float32 n
            building.Properties.Capacity * health)

type RegionStatus =
    {
        Properties : Region
        Owner : CoalitionId option
        Damages : Map<OrientedPosition, GroundDamage>
        Resources : float32<E>
    }
with
    member this.Capacity =
        GroundDamage.CountResources this.Properties.IndustryBuildings this.Damages

type AirfieldStatus =
    {
        Properties : Airfield
        Resources : float32<E>
        Damages : Map<OrientedPosition, GroundDamage>
        Planes : Map<PlaneModel, float32>
    }
with
    member this.Capacity =
        GroundDamage.CountResources this.Properties.Facilities this.Damages

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
        let resources =
            description.Capacity
        {
            Properties = description
            Owner = description.InitialOwner
            Damages = Map.empty
            Resources = resources
        }

    /// Create the initial status of an airfield
    let mkAirfield (world : World) (airfield : AirfieldId) =
        let description =
            world.Airfields
            |> List.find (fun afs -> afs.AirfieldId = airfield)
        let resources =
            description.Capacity
        {
            Airfield = description
            Resources = resources
            Damages = Map.empty
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