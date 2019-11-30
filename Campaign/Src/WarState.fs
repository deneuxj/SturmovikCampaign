namespace Campaign.WarState

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
        Health : float32
    }

type RegionStatus =
    {
        RegionId : RegionId
        Owner : CoalitionId option
        Damages : GroundDamage list
        Resources : float32<E>
    }

type AirfieldStatus =
    {
        AirfieldId : AirfieldId
        Resources : float32<E>
        Damages : GroundDamage list
        Planes : Map<PlaneModel, float32>
    }

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
            RegionId = region
            Owner = description.InitialOwner
            Damages = []
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
            AirfieldId = airfield
            Resources = resources
            Damages = []
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