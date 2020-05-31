// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
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

/// Datatypes and function related to extracting data from mission log reports.
/// This data is used to produce an updated WorldState.
module Campaign.ResultExtraction

open System.Numerics

open ploggy
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.Blocks.StaticDefenses.Types
open SturmovikMission.Blocks.ParaDrop
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.Orders
open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.PlaneSet
open Campaign.WatchLogs
open Util
open FSharp.Control
open SturmovikMission.Blocks.Vehicles

let private logger = NLog.LogManager.GetCurrentClassLogger()

let timeLessEntryTypes = Set.ofList [ LogEntryType.LogVersion; LogEntryType.PosChanged; LogEntryType.Join; LogEntryType.Leave ]

/// Match the object type strings in log events with plane models.
let planeObjectType (planeSet : PlaneSet) (s : string) =
    match s with
    | null -> None
    | s -> Some s
    |> Option.bind (fun s ->
        planeSet.AllModels
        |> Seq.tryFind (fun model -> s.ToLower().Contains(model.LogName)))

/// A region shipped supplies
type SuppliesShipped = {
    OrderId : OrderId // Refers to a resupply order
}

let extractSuppliesShipped (orders : ResupplyOrder list) (entries : AsyncSeq<LogEntry>) =
    let tryGetOrder(eventName) =
        orders
        |> Seq.tryFind (fun order -> order.MatchesMissionLogDepartureEventName(eventName))
    asyncSeq {
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    match tryGetOrder eventName with
                    | Some order ->
                        let energy = order.Convoy.TransportedSupplies
                        yield { OrderId = order.OrderId }
                    | None -> ()
                | None ->
                    ()
            | _ ->
                ()
    }

/// A tank column left its home region
type ColumnLeft = {
    OrderId : OrderId
    RankOffset : int
    Vehicles : GroundAttackVehicle[]
}

let extractColumnDepartures (orders : ColumnMovement list) (entries : AsyncSeq<LogEntry>) =
    let tryGetRank(eventName) =
        orders
        |> Seq.tryPick (fun order -> order.MatchesMissionLogDepartureEventName(eventName) |> Option.map (fun rankOffset -> order, rankOffset))
    asyncSeq {
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    match tryGetRank eventName with
                    | Some(order, rankOffset) ->
                        let maxLength =
                            match order.TransportType with
                            | ColByRoad ->
                                ColumnMovement.MaxColumnSize
                            | ColByRiverShip
                            | ColBySeaShip
                            | ColByTrain ->
                                System.Int32.MaxValue
                        let vehicles =
                            try
                                order.Composition
                                |> Array.skip rankOffset
                            with
                            | _ -> order.Composition
                            |> Array.truncate maxLength
                        yield { OrderId = order.OrderId; Vehicles = vehicles; RankOffset = rankOffset }
                    | None -> ()
                | None ->
                    ()
            | _ ->
                ()
    }

/// A train, truck convoy or a tank column encountered a destroyed bridge and stopped
type VehiclesBlocked = {
    OrderId : OrderId
    RankOffset : int option
}

let extractBlockedVehicles (tanks : ColumnMovement list) (convoys : ResupplyOrder list) (entries : AsyncSeq<LogEntry>) : AsyncSeq<VehiclesBlocked> =
    let tryGetTanksRank(eventName) =
        tanks
        |> Seq.tryPick (fun order -> order.MatchesMissionLogBlockedEventName(eventName) |> Option.map (fun rankOffset -> order, rankOffset))
    let tryGetConvoyRank(eventName) =
        convoys
        |> Seq.tryFind (fun order -> order.MatchesMissionLogBlockedEventName(eventName))
    asyncSeq {
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    match tryGetTanksRank eventName with
                    | Some(order, rankOffset) ->
                        yield { OrderId = order.OrderId; RankOffset = Some rankOffset }
                    | None ->
                        match tryGetConvoyRank eventName with
                        | Some(order) ->
                            yield { OrderId = order.OrderId; RankOffset = None }
                        | None ->
                            ()
                | None ->
                    ()
            | _ ->
                ()
    }


/// Easy handling of result of extractFerryPlanes
let (|PlaneFerrySpawned|PlaneFerryLanded|PlaneFerryKilled|) =
    function
    | Choice1Of3 (x : PlaneFerryOrder) -> PlaneFerrySpawned x
    | Choice2Of3 (x : PlaneFerryOrder) -> PlaneFerryLanded x
    | Choice3Of3 (x : PlaneFerryOrder) -> PlaneFerryKilled x

/// Extract events related to ferry planes.
let extractFerryPlanes (orders : PlaneFerryOrder list) (entries : AsyncSeq<LogEntry>) =
    asyncSeq {
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    let spawnedOrder =
                        orders
                        |> List.tryFind (fun order -> order.SpawnedEventName = eventName)
                    let landedOrder =
                        orders
                        |> List.tryFind (fun order -> order.LandedEventName = eventName)
                    let killedOrder =
                        orders
                        |> List.tryFind (fun order -> order.KilledEventName = eventName)
                    match spawnedOrder, landedOrder, killedOrder with
                    | Some order, _, _ ->
                        yield Choice1Of3 order
                    | None, Some order, _ ->
                        yield Choice2Of3 order
                    | None, None, Some order ->
                        yield Choice3Of3 order
                    | None, None, None ->
                        ()
                | None ->
                    ()
            | _ ->
                ()
    }

/// Precision of successful paratropper drop.
type ParaDropPrecision = Precise | Wide

/// A paratrooper landed alive inside or near a landing zone.
type ParaDropResult = {
    BattleId : RegionId
    Coalition : CoalitionId
    Precision : ParaDropPrecision
}

let extractParaDrops (world : World) (state : WorldState) (battles : (AreaId * CoalitionId) seq) (entries : AsyncSeq<LogEntry>) =
    let defenders =
        battles
        |> dict
    let battles =
        battles
        |> Seq.map fst
        |> Set.ofSeq
    let battles =
        world.Battlefields
        |> List.filter(fun area -> battles.Contains area.DefenseAreaId)
    let checkForParaDrop precision eventName =
        let f =
            match precision with
            | Precise -> ParaDrop.TryGetPreciseDropEventName
            | Wide -> ParaDrop.TryGetWideDropEventName
        match f eventName with
        | Some(side, suffix) ->
            match battles |> List.tryFind (fun battle -> string battle.Home = suffix) with
            | Some battle ->
                match side with
                | 'A' ->
                    Some {
                        BattleId = battle.Home
                        Coalition = defenders.[battle.DefenseAreaId].Other
                        Precision = precision
                    }
                | 'D' ->
                    Some {
                        BattleId = battle.Home
                        Coalition = defenders.[battle.DefenseAreaId]
                        Precision = precision
                    }
                | _ -> failwithf "Bad side %c" side
            | None ->
                None
        | None ->
            None
    asyncSeq {
        let idxToName = ref Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idxToName := Map.add spawned.ObjectId spawned.ObjectName !idxToName
            | :? KillEntry as event when event.AttackerId = -1 ->
                match Map.tryFind event.TargetId !idxToName with
                | Some eventName ->
                    match checkForParaDrop Precise eventName with
                    | Some result -> yield result
                    | None ->
                        match checkForParaDrop Wide eventName with
                        | Some result -> yield result
                        | None -> ()
                | None ->
                    ()
            | _ ->
                ()
    }

/// A plane took off, possibly took some damage and then landed/crashed near or at an airfield
type TookOff = {
    PlaneId : int
    Airfield : AirfieldId
    Plane : PlaneModel
    Cargo : float32<K>
    WeaponCost : float32<E>
    PlayerName : string option
    Coalition : CoalitionId option
}

type Landed = {
    PlaneId : int
    Airfield : AirfieldId
    Plane : PlaneModel
    Health : float32
    Cargo : float32<K>
    PlayerName : string option
    Coalition : CoalitionId option
}
with
    static member MaxDistanceFromAirfield = 5000.0f

let (|TookOff|Landed|) =
    function
    | Choice1Of2 x -> TookOff x
    | Choice2Of2 x -> Landed x

let extractTakeOffsAndLandings (world : World) (state : WorldState) (entries : AsyncSeq<LogEntry>) =
    let (|PlaneObjectType|_|) = planeObjectType world.PlaneSet
    let wg = WorldFastAccess.Create world
    let sg = WorldStateFastAccess.Create state
    let tookOff (x : TookOff) = Choice1Of2 x
    let landed (x : Landed) = Choice2Of2 x
    asyncSeq {
        let planeIds = ref Map.empty
        let damages = ref Map.empty
        let cargo = ref Map.empty
        let weaponCost = ref Map.empty
        let bombWeight = ref Map.empty
        // Number of bombs at take off
        let mutable initBombs = Map.empty
        // Map object ID to starting airfield
        let ongoingFlight = ref Map.empty
        // Map human player (NickId) to vehicle id
        let playerPilot = ref Map.empty
        // Map plane id to name of human player and coalition
        let planePilot = ref Map.empty
        // When a plane that took of with bombs lands, wait until the player ends the mission before registering the landing.
        // When the player leaves the plane, one can check the amount of bombs. If identical to the value at take off,
        // register as a cargo mission.
        let mutable delayedLanding = Map.empty 
        // Function to send back a plane to starting airfield. Used for planes still in the air when the round ends or if a player disconnects while flying.
        let sendPlaneBack vehicle af =
            match planeIds.Value.TryFind vehicle with
            | Some plane ->
                let health =
                    match Map.tryFind vehicle !damages with
                    | Some x -> max 0.0f (1.0f - x)
                    | None -> 1.0f
                let cargoAmount =
                    cargo.Value.TryFind vehicle
                    |> Option.defaultValue 0.0f<K>
                cargo := cargo.Value.Remove vehicle
                ongoingFlight := ongoingFlight.Value.Remove vehicle
                Some { PlaneId = vehicle; Airfield = af; Plane = plane; Health = health; Cargo = cargoAmount; PlayerName = None; Coalition = sg.GetRegion(wg.GetAirfield(af).Region).Owner }
            | None ->
                None
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                match spawned.ObjectType with
                | PlaneObjectType model ->
                    planeIds := Map.add spawned.ObjectId model !planeIds
                    damages := Map.add spawned.ObjectId 0.0f !damages
                | _ -> ()
            | :? DamageEntry as damage ->
                let oldDamage =
                    match Map.tryFind damage.TargetId !damages with
                    | Some x -> x
                    | None -> 0.0f
                let newDamage = oldDamage + damage.Damage
                damages := Map.add damage.TargetId newDamage !damages
            | :? PlayerPlaneEntry as playerPlane ->
                let country = CountryId.FromLogEntry playerPlane.Country
                let coalition = country |> Option.map (fun country -> country.Coalition)
                playerPilot := playerPilot.Value.Add(playerPlane.NickId, playerPlane.VehicleId)
                planePilot := planePilot.Value.Add(playerPlane.VehicleId, (playerPlane.Name, coalition))
                initBombs <- initBombs.Add(playerPlane.VehicleId, playerPlane.Bombs)
                match playerPlane.VehicleType with
                | PlaneObjectType plane when plane.HasRole CargoTransporter ->
                    let modmask, payload = plane.Payloads.[CargoTransporter]
                    if playerPlane.Payload = payload then
                        cargo := Map.add playerPlane.VehicleId plane.CargoCapacity cargo.Value
                | _ ->
                    ()
                match playerPlane.VehicleType with
                | PlaneObjectType model ->
                    let bombLoadWeight =
                        model.BombLoads
                        |> List.tryPick (fun (loadout, weight) -> if loadout = playerPlane.Payload then Some weight else None)
                        |> Option.defaultValue 0.0f<K>
                    let cost = model.GetPayLoadCost(playerPlane.Payload, bombCost)
                    weaponCost := Map.add playerPlane.VehicleId cost weaponCost.Value
                    bombWeight := bombWeight.Value.Add(playerPlane.VehicleId, bombLoadWeight)
                | _ ->
                    ()
            | :? TakeOffEntry as takeOff ->
                let pos = Vector2(takeOff.Position.X, takeOff.Position.Z)
                let af = world.GetClosestAirfield(pos)
                match Map.tryFind takeOff.VehicleId !planeIds with
                | Some plane ->
                    let cargo =
                        cargo.Value.TryFind takeOff.VehicleId
                        |> Option.defaultValue 0.0f<K>
                    let weapons =
                        weaponCost.Value.TryFind takeOff.VehicleId
                        |> Option.defaultValue 0.0f<E>
                    let pilot, coalition =
                        match planePilot.Value.TryFind takeOff.VehicleId with
                        | None -> None, None
                        | Some(pilot, coalition) -> Some pilot, coalition
                    ongoingFlight := ongoingFlight.Value.Add(takeOff.VehicleId, af.AirfieldId)
                    yield tookOff { PlaneId = takeOff.VehicleId; Airfield = af.AirfieldId; Plane = plane; Cargo = cargo; WeaponCost = weapons; PlayerName = pilot; Coalition = coalition }
                | None ->
                    logger.Warn(sprintf "TookOff: Unknwon type of plane '%d'" takeOff.VehicleId)
            | :? LandingEntry as landing ->
                let pos = Vector2(landing.Position.X, landing.Position.Z)
                let af = world.GetClosestAirfield(pos)
                let dist = (af.Pos - pos).Length()
                // Close the flight
                ongoingFlight := ongoingFlight.Value.Remove(landing.VehicleId)
                // Handle landing event (repair plane, register landing)
                if dist < Landed.MaxDistanceFromAirfield then
                    match Map.tryFind landing.VehicleId !planeIds with
                    | Some plane ->
                        let health =
                            match Map.tryFind landing.VehicleId !damages with
                            | Some x -> max 0.0f (1.0f - x)
                            | None -> 1.0f
                        let repairedHealth =
                            // Lightly damaged planes are repaired to full health.
                            // The acceptable level of damage depends on who controls the airfield...
                            let repairable =
                                match plane.Coalition, sg.GetRegion(af.Region).Owner with
                                | coalition, Some coalition2 when coalition = coalition2 -> 0.75f // Own airfield
                                | captured, Some capturing -> 0.9f // Enemy airfield, fewer repair options available
                                | _, None -> 1.0f // Neutral airfield, no repairs
                            if health > repairable then
                                1.0f
                            else
                                health
                        let cargoAmount =
                            cargo.Value.TryFind landing.VehicleId
                            |> Option.defaultValue 0.0f<K>
                        cargo := cargo.Value.Remove landing.VehicleId
                        let pilot, coalition =
                            match planePilot.Value.TryFind landing.VehicleId with
                            | None -> None, None
                            | Some(pilot, coalition) -> Some pilot, coalition
                        let ev = { PlaneId = landing.VehicleId; Airfield = af.AirfieldId; Plane = plane; Health = repairedHealth; Cargo = cargoAmount; PlayerName = pilot; Coalition = coalition }
                        match initBombs.TryFind(landing.VehicleId) with
                        | Some n when n > 0 ->
                            delayedLanding <- delayedLanding.Add(landing.VehicleId, (ev, sg.GetRegion(af.Region).Owner = coalition && health > 0.0f))
                        | _ ->
                            yield landed ev 
                    | None -> ()
            | :? PlayerMissionEndEntry as entry ->
                match delayedLanding.TryFind(entry.VehicleId) with
                | Some (ev, deliveredCargo) ->
                    match initBombs.TryFind(entry.VehicleId) with
                    | Some n when n > 0 && deliveredCargo ->
                        let load = bombWeight.Value.TryFind(entry.VehicleId) |> Option.defaultValue 0.0f<K>
                        if entry.Bombs = n then
                            yield landed { ev with Cargo = load }
                        else
                            yield landed ev
                    | _ ->
                        yield landed ev
                | _ -> ()
                delayedLanding <- delayedLanding.Remove(entry.VehicleId)
                initBombs <- initBombs.Remove(entry.VehicleId)
                weaponCost := weaponCost.Value.Remove(entry.VehicleId)
                bombWeight := bombWeight.Value.Remove(entry.VehicleId)
            | :? RoundEndEntry as roundEnd ->
                // register all ongoing flights as landed back at starting airfield
                for vehicle, af in ongoingFlight.Value |> Map.toSeq do
                    match planeIds.Value.TryFind vehicle with
                    | Some plane ->
                        match sendPlaneBack vehicle af with
                        | Some landing -> yield landed landing
                        | None -> ()
                    | None ->
                        ()
            | :? LeaveEntry as left ->
                // register an in-flight disconnection as landed back at starting airfield
                match playerPilot.Value.TryFind left.NickId with
                | Some vehicle ->
                    planePilot := planePilot.Value.Remove vehicle
                    match ongoingFlight.Value.TryFind vehicle with
                    | Some af ->
                        match sendPlaneBack vehicle af with
                        | Some landing -> yield landed landing
                        | None -> ()
                    | None ->
                        ()
                | None ->
                    ()
                playerPilot := playerPilot.Value.Remove left.NickId
            | _ -> ()
    }

/// <summary>
/// Remove landings of planes that never took off.
/// Used to avoid adding planes from AI flights that returned to base.
/// </summary>
let filterNoTakeOff (takeOffs : TookOff list) (landings : Landed list) =
    let tookOff =
        takeOffs
        |> Seq.map (fun ev -> ev.PlaneId)
        |> Set.ofSeq
    landings
    |> List.filter (fun ev -> tookOff.Contains ev.PlaneId)

type VehicleInColumn = {
    OrderId : OrderId
    Rank : int
}

/// Something got bombed or strafed
type DamagedObject =
    | Production of RegionId * group:int * building:int
    | Storage of RegionId * group:int * building:int
    | Airfield of AirfieldId * group:int * building:int
    | Cannon of RegionId
    | MachineGun of RegionId
    | Convoy of VehicleInColumn
    | Column of VehicleInColumn
    | Vehicle of RegionId * GroundAttackVehicle
    | ParkedPlane of AirfieldId * PlaneModel
    | ActivePlane of CoalitionId * PlaneModel * AiHomeAirfield:AirfieldId option
with
    member this.Coalition(wg : WorldFastAccess, sg : WorldStateFastAccess) =
        match this with
        | Storage(region, _, _)
        | Vehicle(region, _)
        | Production(region, _, _) -> sg.GetRegion(region).Owner
        | ParkedPlane(af, _)
        | Airfield(af, _, _) -> sg.GetRegion(wg.GetAirfield(af).Region).Owner
        | Cannon region
        | MachineGun region -> sg.GetRegion(region).Owner
        | Convoy v
        | Column v ->
            Some v.OrderId.Coalition
        | ActivePlane(coalition, _, _) ->
            Some coalition

    /// Full reward for destroying this
    member this.Value(wg : WorldFastAccess, sg : WorldStateFastAccess) =
        /// Compute value of storage of one building in a group
        let computeStorageValue (group : StaticGroup, building : int) =
            let buildings = group.SubBlocks wg.World.SubBlockSpecs
            /// Check if building is an important one (i.e. it's not a dog house, hay stack, tractor...)
            match Array.tryFind ((=) building) buildings with
            | Some _ ->
                let groupSize = Array.length buildings
                (group.Storage(wg.World.SubBlockSpecs) + group.RepairCost(wg.World.SubBlockSpecs)) / (float32 groupSize)
            | None ->
                0.0f<E>

        match this with
        | Storage(region, group, building) ->
            let group =
                wg.GetRegion(region).Storage.[group]
            computeStorageValue(group, building)
        | Airfield(afId, group, building) ->
            let group =
                wg.GetAirfield(afId).Storage.[group]
            computeStorageValue(group, building)
        | Production(region, group, building) ->
            let group =
                wg.GetRegion(region).Production.[group]
            let buildings = group.SubBlocks wg.World.SubBlockSpecs
            match Array.tryFind ((=) building) buildings with
            | Some _ ->
                let groupSize = Array.length buildings
                let groupSize = float32 groupSize
                let repairCost = group.RepairCost(wg.World.SubBlockSpecs) / groupSize
                let timeToRepair = repairCost / wg.World.RepairSpeed
                let productionLoss = 0.5f * group.Production(wg.World.SubBlockSpecs, wg.World.ProductionFactor) / groupSize * timeToRepair
                repairCost + productionLoss
            | None ->
                0.0f<E>
        | Cannon _ ->
            cannonCost
        | MachineGun _ ->
            machineGunCost
        | Convoy _ ->
            float32 shipVehicleCapacity * GroundAttackVehicle.MediumTankCost
        | Column _ ->
            GroundAttackVehicle.MediumTankCost
        | Vehicle(_, vehicle) ->
            vehicle.Cost
        | ParkedPlane(_, plane)
        | ActivePlane(_, plane, _) ->
            plane.Cost


type CommonDamageData = {
    Amount : float32
    ByPlayer : string option
}
with
    static member FromValue(v) =
        { Amount = v
          ByPlayer = None
        }

    member this.SetByPlayer(player) =
        { this with ByPlayer = player }


type Damage = {
    Object : DamagedObject
    Data : CommonDamageData
}
with
    static member GroupByObject(xs) =
        xs
        |> Seq.groupBy (fun damage -> damage.Object)
        |> Seq.map (fun (damageObject, damages) ->
            { Object = damageObject
              Data =
                { Amount =
                    damages
                    |> Seq.map (fun dam -> dam.Data.Amount)
                    |> Seq.sum
                  ByPlayer = None
                }
            })

    static member GroupByObjectAndPlayer(xs) =
        xs
        |> Seq.groupBy (fun damage -> damage.Object, damage.Data.ByPlayer)
        |> Seq.map (fun ((damageObject, player), damages) ->
            { Object = damageObject
              Data =
                { Amount =
                    damages
                    |> Seq.map (fun dam -> dam.Data.Amount)
                    |> Seq.sum
                  ByPlayer = player
                }
            })

    member this.Value(wg, sg) =
        this.Object.Value(wg, sg) * this.Data.Amount


let (|BuildingObjectType|_|) (s : string) =
    let low = s.ToLower()
    [ "arf_"; "industrial_"; "vl_"; "meh_"; "sklad_"; "scot_"; "port_"; "town_"; "rwstation_"; "watertower"; "static_gazaa" ]
    |> List.exists (fun prefix -> low.StartsWith(prefix))
    |> function true -> Some s | false -> None

let staticPlaneType (planeSet : PlaneSet) (s : string) =
    planeSet.AllModels
    |> Seq.tryPick(fun model ->
        if s.Contains(planeSet.StaticPlaneModel(model).ShortName) then
            Some model
        else
            None
    )

let (|StaticVehicleType|_|) (s : string) =
    let s = s.ToLowerInvariant()
    [
        ("t34", GroundAttackVehicle.HeavyTank)
        ("pz_iii", GroundAttackVehicle.HeavyTank)
        ("t70", GroundAttackVehicle.MediumTank)
        ("pz_iv", GroundAttackVehicle.MediumTank)
        ("bt-7m", GroundAttackVehicle.LightArmor)
        ("sdkfz251", GroundAttackVehicle.LightArmor)
    ]
    |> List.tryPick (fun (subs, model) ->
        if s.Contains("static_") && s.Contains(subs) then
            Some model
        else
            None)

let tryIdentifyBuilding (world : World) (pos : Vector2) (subGroup : int) =
    let tryFindContainingRegion (pos : Vector2) =
        world.Regions
        |> List.tryFind(fun r ->
            pos.IsInConvexPolygon(r.Boundary))
    match tryFindContainingRegion pos with
    | Some region ->
        let airfields =
            world.Airfields
            |> List.filter (fun af -> af.Pos.IsInConvexPolygon(region.Boundary))
        let matchingAirfieldBuildings =
            airfields
            |> List.map (fun af ->
                af.Storage
                |> List.mapi (fun i sto -> Airfield(af.AirfieldId, i, subGroup), sto))
            |> List.concat
        let matchingStorageBuildings =
            region.Storage
            |> List.mapi (fun i sto -> Storage(region.RegionId, i, subGroup), sto)
        let matchingProductionBuildings =
            region.Production
            |> List.mapi (fun i pro -> Production(region.RegionId, i, subGroup), pro)
        let closest =
            try
                matchingAirfieldBuildings @ matchingProductionBuildings @ matchingStorageBuildings
                |> Seq.map (fun (x, building) -> x, (building.Pos.Pos - pos).Length(), building)
                |> Seq.filter (fun (_, dist, _) -> dist < 100.0f)
                |> Seq.minBy (fun (_, dist, _) -> dist)
                |> Some
            with
            | _ -> None
        match closest with
        | Some(damaged, _, building) ->
            let significantSubBlocks = building.SubBlocks(world.SubBlockSpecs)
            if Array.contains subGroup significantSubBlocks then
                Some damaged
            else
                None
        | None ->
            None
    | None ->
        None

let extractStaticDamages (world : World) (entries : AsyncSeq<LogEntry>) =
    let (|PlaneObjectType|_|) = planeObjectType world.PlaneSet
    let (|StaticPlaneType|_|) = staticPlaneType world.PlaneSet
    let wg = WorldFastAccess.Create(world)
    let tryFindContainingRegion (pos : Vector2) =
        world.Regions
        |> List.tryFind(fun r ->
            pos.IsInConvexPolygon(r.Boundary))
    asyncSeq {
        let mutable idMapper = Map.empty
        let mutable pilots = Map.empty
        let mutable healths = Map.empty

        let handleDamage(attacker : int, target : int, amount : float32, damagePos : Vector2) =
            asyncSeq {
                // Keep track of healths, so that we can count kills as full damage
                match healths.TryFind target with
                | Some h ->
                    healths <- healths.Add(target, h - amount)
                | None ->
                    healths <- healths.Add(target, 1.0f - amount)

                match idMapper.TryFind target with
                | Some(BuildingObjectType buildingType, _, subGroup, _) ->
                    // Damage to buildings: storage or production
                    match tryIdentifyBuilding world damagePos subGroup with
                    | Some damaged ->
                        let player =
                            pilots.TryFind attacker
                        let data = { Amount = amount; ByPlayer = player }
                        yield { Object = damaged; Data = data }
                    | None -> () // No known building nearby
                | Some(StaticPlaneType planeModel, _, _, _) ->
                    let closestAirfield =
                        world.Airfields
                        |> List.minBy (fun af -> (af.Pos - damagePos).LengthSquared())
                    let distance = (closestAirfield.Pos - damagePos).Length()
                    if distance < 3000.0f then
                        let player = pilots.TryFind attacker
                        let data = { Amount = amount; ByPlayer = player }
                        yield { Object = ParkedPlane(closestAirfield.AirfieldId, planeModel); Data = data }
                | Some(StaticVehicleType vehicleModel, _, _, _) ->
                    match tryFindContainingRegion damagePos with
                    | Some region ->
                        let player = pilots.TryFind attacker
                        let data = { Amount = amount; ByPlayer = player }
                        yield { Object = Vehicle(region.RegionId, vehicleModel); Data = data }
                    | None ->
                        ()
                | Some(_, (CannonObjectName as gunType), _, _) | Some (_, (MachineGunAAName as gunType), _, _) ->
                    let region =
                        world.Regions
                        |> List.tryFind (fun area -> damagePos.IsInConvexPolygon(area.Boundary))
                    match region with
                    | Some region ->
                        let objType =
                            match gunType with
                            | CannonObjectName -> Some(Cannon(region.RegionId))
                            | MachineGunAAName -> Some(MachineGun(region.RegionId))
                            | _ ->
                                None
                        match objType with
                        | Some objType ->
                            let player = pilots.TryFind attacker
                            let data = { Amount = amount; ByPlayer = player }
                            yield { Object = objType; Data = data }
                        | None ->
                            ()
                    | None ->
                        ()
                | Some(PlaneObjectType plane, name, _, country)->
                    let coalition =
                        match country with
                        | Country.Germany | Country.OtherAxis -> Some Axis
                        | Country.USSR | Country.OtherAllies -> Some Allies
                        | _ -> None
                    match coalition with
                    | Some coalition ->
                        let player = pilots.TryFind attacker
                        let data = { Amount = amount; ByPlayer = player }
                        let aiHomeAirfield =
                            match AiPlanes.AiPatrol.TryExtractHomeAirfield name with
                            | Some(_, af) -> Some af
                            | None ->
                                match AiPlanes.AiAttack.TryExtractHomeAirfield name with
                                | Some(_, af) -> Some af
                                | None -> None
                            |> Option.filter (fun afId -> world.Airfields |> List.exists (fun af2 -> af2.AirfieldId = afId)) // If name of airfield got wrong (e.g. truncated), ignore it
                        yield { Object = ActivePlane(coalition, plane, aiHomeAirfield); Data = data }
                    | None ->
                        ()
                | _ -> () // Ignored object type
            }

        for entry in entries do
            match entry with
            | :? PlayerPlaneEntry as entry ->
                pilots <- pilots.Add(entry.VehicleId, entry.Name)
            | :? ObjectSpawnedEntry as spawned ->
                idMapper <- idMapper.Add(spawned.ObjectId, (spawned.ObjectType, spawned.ObjectName, spawned.SubGroup, spawned.Country))
            | :? DamageEntry as damage ->
                let damagePos = Vector2(float32 damage.Position.X, float32 damage.Position.Z)
                yield! handleDamage(damage.AttackerId, damage.TargetId, damage.Damage, damagePos)
            | :? KillEntry as kill ->
                let damagePos = Vector2(float32 kill.Position.X, float32 kill.Position.Z)
                let amount =
                    healths.TryFind kill.TargetId
                    |> Option.defaultValue 1.0f
                    |> max 0.0f
                healths <- healths.Remove(kill.TargetId)
                yield! handleDamage(kill.AttackerId, kill.TargetId, amount, damagePos)
            | _ -> () // Ignored log entry
    }

let extractVehicleDamages (world : World) (tanks : ColumnMovement list) (convoys : ResupplyOrder list) (entries : AsyncSeq<LogEntry>) =
    asyncSeq {
        let mutable idMapper = Map.empty
        for entry in entries do
            match entry with
            | :? ObjectSpawnedEntry as spawned ->
                idMapper <- idMapper.Add(spawned.ObjectId, (spawned.ObjectName, spawned.ObjectType, spawned.Country))
            | :? KillEntry as kill ->
                match idMapper.TryFind kill.TargetId with
                | Some(name, objectType, country) ->
                    let columnDamage =
                        tanks
                        |> List.tryPick (fun order ->
                            order.MatchesMissionLogVehicleKilledEventName(name)
                            |> Option.map (fun rank -> order, rank))
                    let convoyDamage =
                        lazy
                            convoys
                            |> List.tryPick (fun order ->
                                order.MatchesMissionLogVehicleKilledEventName(name)
                                |> Option.map (fun rank -> order, rank))
                    match columnDamage with
                    | Some(order, rank) -> yield { Object = Column { OrderId = order.OrderId; Rank = rank }; Data = CommonDamageData.FromValue 1.0f }
                    | None ->
                        match convoyDamage.Value with
                        | Some(order, rank) -> yield { Object = Convoy { OrderId = order.OrderId; Rank = rank }; Data = CommonDamageData.FromValue 1.0f }
                        | None -> ()
                | None ->
                    ()
            | _ -> ()
    }


type BattleParticipantKilled = {
    Coalition : CoalitionId
    BattleId : RegionId
    Vehicle : GroundAttackVehicle
    KilledByPlayer : string option
}

/// Extract damages caused to vehicles in a battle. Used to compute battle bonuses.
let extractBattleDamages (world : World) (state : WorldState) (battles : (AreaId * CoalitionId) seq) (entries : AsyncSeq<LogData<LogEntry>>) =
    let defenders =
        battles
        |> dict
    let battles =
        battles
        |> Seq.map fst
        |> Set.ofSeq
    let battles =
        world.Battlefields
        |> List.filter(fun area -> battles.Contains area.DefenseAreaId)
    asyncSeq {
        let mutable idMapper = Map.empty
        let mutable playerVehicles = Map.empty
        let mutable isMuted = true
        for entry in entries do
            
            match isMuted, entry with
            | true, Fresh _ ->
                isMuted <- false
                yield Choice1Of2()
            | false, _ | _, Old _ -> ()

            match entry.Data with
            | :? ObjectSpawnedEntry as spawned ->
                idMapper <- Map.add spawned.ObjectId spawned.ObjectName idMapper
                // If the parent object (if any) of a newly spawned object is controlled by a player,
                // record this sub-object also as controlled by that player.
                match playerVehicles.TryFind(spawned.ParentId) with
                | Some turretPlayer ->
                    playerVehicles <- Map.add spawned.ObjectId turretPlayer playerVehicles
                | None ->
                    ()
            | :? PlayerPlaneEntry as entry ->
                playerVehicles <- Map.add entry.VehicleId entry.Name playerVehicles
            | :? KillEntry as kill ->
                match Map.tryFind kill.TargetId idMapper with
                | Some name ->
                    yield!
                        asyncSeq {
                            for battle in battles do
                                for vehicle in GroundAttackVehicle.AllVehicles do
                                    for side in [ "A"; "D" ] do
                                        let vehName = sprintf "B-%s-%s-%s" (string battle.Home) side vehicle.Description
                                        if name = vehName then
                                            let coalition =
                                                match side with
                                                | "D" -> defenders.[battle.DefenseAreaId]
                                                | "A" -> defenders.[battle.DefenseAreaId].Other
                                                | _ -> failwithf "Unknown side '%s'" side
                                            yield Choice2Of2 {
                                                Coalition = coalition
                                                BattleId = battle.Home
                                                Vehicle = vehicle
                                                KilledByPlayer = playerVehicles.TryFind(kill.AttackerId)
                                            }
                        }
                | None ->
                    ()
            | _ -> ()
    }
