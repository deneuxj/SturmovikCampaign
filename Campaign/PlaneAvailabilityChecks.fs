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

module Campaign.PlaneAvailabilityChecks

open System
open System.Numerics
open NLog
open ploggy
open FSharp.Control
open SturmovikMission.Blocks.StaticDefenses.Types
open Campaign.PlayerDiscipline
open Campaign.BasicTypes
open Campaign.WorldDescription
open Campaign.PlayerHangar
open Campaign.PlaneModel
open Campaign.WorldState
open Campaign.ResultExtraction
open System.Runtime.Serialization

let private logger = LogManager.GetCurrentClassLogger()

/// Messages sent to the live commenter
type PlaneAvailabilityMessage =
    | PlayerEntered of Guid
    | Overview of UserIds * delay:int * string list
    | Warning of UserIds * delay:int * string list
    | Announce of CoalitionId * string list
    | Violation of UserIds * string
    | Status of Map<string, PlayerHangar> * Map<AirfieldId, Map<PlaneModel, float32>>
    | PlanesAtAirfield of AirfieldId * Map<PlaneModel, float32>

/// Show hangar to its player
let showHangar(hangar : PlayerHangar, delay) =
    [
        let userIds = { UserId = string hangar.Player; Name = hangar.PlayerName }
        yield Overview(userIds, delay,
            [
                sprintf "Welcome back %s" userIds.Name
                sprintf "Your cash reserve is %0.0f" hangar.Reserve
            ])
        yield Overview(userIds, delay,
            [
                for kvp in hangar.Airfields do
                    match hangar.ShowAvailablePlanes(kvp.Key) with
                    | [] -> ()
                    | planes ->
                        let planes = String.concat ", " planes
                        yield sprintf "Your reserved planes at %s: %s" kvp.Key.AirfieldName planes
            ])
    ]

/// Create an empty hangar for a player
let private emptyHangar (playerId : string, playerName : string) =
    { Player = Guid(playerId); PlayerName = playerName; Reserve = 0.0f<E>; Airfields = Map.empty }

/// Commands sent during state transitions in PlayerStateData to the main asyncSeq computation
type Command =
    | PlaneCheckOut of user:UserIds * PlaneModel * health:float32 * funds:float32<E> * AirfieldId
    | PlaneCheckIn of user:UserIds * PlaneModel * health:float32 * AirfieldId
    | DeliverSupplies of float32<E> * RegionId
    | RewardPlayer of user:UserIds * float32<E>
    | InformPlayerHangar of UserIds
    | PunishThief of user:UserIds * PlaneModel * AirfieldId
    | Message of PlaneAvailabilityMessage

// It is not possible to identify static objects before they are damaged, because their position vector
// is null in the spawn log entry. The information for static objects is stored in StaticObject
// and resolved to Production, Storage, AirfieldBuilding or nothing when damage entries are handled.

type ObjectInstance =
    | StaticObject of objectType:string * subGroup:int option
    | Production of StaticGroup * int
    | Storage of StaticGroup * int
    | AirfieldBuilding of StaticGroup * int
    | StaticPlane of PlaneModel
    | StaticTank of GroundAttackVehicle
    | DynamicPlane of PlaneModel
    | CargoShip
    | BigEscortShip
    | SmallEscortShip
    | LandingShip
    | Artillery
    | LightMachineGun
    | HeavyMachineGun
    | DynamicTank of GroundAttackVehicle
    | ConvoyTruck
    | TrainWagon
    | Locomotive

/// Keeps track of information needed for the state transitions in PlayerFlightData
type Context =
    { World : WorldFastAccess
      State : WorldStateFastAccess
      Binding : Map<int, CoalitionId * ObjectInstance>
      Hangars : Map<string, PlayerHangar>
      Airfields : Map<AirfieldId, Map<PlaneModel, float32>>
      RearAirfields : Set<AirfieldId>
      RegionNeeds : Map<RegionId, float32<E>>
      MaxCash : float32<E>
    }
with
    static member Create(world : World, state : WorldState, hangars : Map<string, PlayerHangar>, maxCash : int) =
        let rearAirfields =
            [Axis; Allies]
            |> List.choose (fun coalition -> state.RearAirfield(world, coalition))
            |> Set.ofList
        let needs = AutoOrder.computeSupplyNeeds world state
        let airfields =
            state.Airfields
            |> Seq.map (fun afs -> afs.AirfieldId, afs.NumPlanes)
            |> Map.ofSeq
        {
            World = world.FastAccess
            State = state.FastAccess
            Binding = Map.empty
            Hangars = hangars
            Airfields = airfields
            RearAirfields = rearAirfields
            RegionNeeds = needs
            MaxCash = 1.0f<E> * float32 maxCash
        }

    /// Bind a vehicle ID to a coalition and a type of object
    member this.HandleBinding(spawn : ObjectSpawnedEntry) =
        let (|PlaneObjectType|_|) = planeObjectType this.World.World.PlaneSet
        let (|StaticPlaneType|_|) = staticPlaneType this.World.World.PlaneSet
        let entity =
            match spawn.ObjectType, spawn.ObjectName with
            | null, _
            | _, null -> failwith "Null field in spawn log entry"
            | PlaneObjectType plane, _ -> DynamicPlane plane
            | StaticPlaneType plane, _ -> StaticPlane plane
            | StaticVehicleType vehicle, _ -> StaticTank vehicle
            | _, CannonObjectName -> Artillery
            | _, HeavyMachineGunAAName -> HeavyMachineGun
            | _, LightMachineGunAAName -> LightMachineGun
            | "PzKpfw III Ausf.H", _
            | "_PzKpfw III Ausf.L", _
            | "T-34-76", _
            | "_T-34-76 STZ", _ -> DynamicTank HeavyTank
            | "Destroyer Type 7", _ -> BigEscortShip
            | "Opel Blitz", _
            | "GAZ-AA", _
            | "GAZ-M", _ -> ConvoyTruck
            | "Landing Boat type A", _ -> LandingShip
            | "River Cargo Ship type Georgia AAA", _
            | "Large Cargo Ship type 1", _
            | "Large Tanker Ship type 1", _ -> CargoShip
            | "Locomotive_E", _
            | "Locomotive_G8", _ -> Locomotive
            | "T-70", _
            | "PzKpfw IV Ausf.F1", _ -> DynamicTank MediumTank
            | "Sd Kfz 10 Flak 38", _
            | "Sd Kfz 251 Wurfrahmen 40", _ -> DynamicTank LightArmor
            | "Torpedo Boat type S-38", _
            | "Torpedo Boat G-5 series 11-bis 213", _ -> SmallEscortShip
            | "wagon_tankb", _ -> TrainWagon
            | "zis-3", _ -> ConvoyTruck
            | "zis-5 72-k", _ -> DynamicTank LightArmor
            | "zis-5", _ -> ConvoyTruck
            | "zis-6 bm-13", _ -> DynamicTank LightArmor
            | _ ->
                StaticObject(spawn.ObjectType, if spawn.SubGroup >= 0 then Some spawn.SubGroup else None)
        let binding =
            match CoalitionId.FromLogEntry spawn.Country with
            | None -> this.Binding
            | Some coalition -> this.Binding.Add(spawn.ObjectId, (coalition, entity))
        { this with Binding = binding}

    /// Attempt to resolve static objects using a damage entry
    member this.ResolveBindings(damage : DamageEntry) =
        let binding =
            match this.Binding.TryFind(damage.TargetId) with
            | Some(coalition, StaticObject(objectType, Some groupIdx)) ->
                let damagePos = Vector2(damage.Position.X, damage.Position.Z)
                match tryIdentifyBuilding this.World.World damagePos groupIdx with
                | Some(Airfield(af, i, _)) ->
                    let sto = this.World.GetAirfield(af).Storage.[i]
                    this.Binding.Add(damage.TargetId, (coalition, AirfieldBuilding(sto, groupIdx)))
                | Some(DamagedObject.Storage(reg, i, _)) ->
                    let sto = this.World.GetRegion(reg).Storage.[i]
                    this.Binding.Add(damage.TargetId, (coalition, Storage(sto, groupIdx)))
                | Some(DamagedObject.Production(reg, i, _)) ->
                    let pro = this.World.GetRegion(reg).Production.[i]
                    this.Binding.Add(damage.TargetId, (coalition, Production(pro, groupIdx)))
                | _ ->
                    this.Binding
            | _ ->
                this.Binding
        { this with Binding = binding }

    /// Execute a command.
    /// Commands are typically created by PlayerFlightData instances, when they hand game log entries.
    member this.Execute(command : Command) =
        match command with
        | PlaneCheckOut(user, plane, health, cost, af) ->
            let coalition =
                this.State.GetRegion(this.World.GetAirfield(af).Region).Owner
            let hangar =
                this.Hangars.TryFind(user.UserId)
                |> Option.defaultValue(emptyHangar(user.UserId, user.Name))
            let planes =
                this.Airfields.TryFind(af)
                |> Option.defaultValue(Map.empty)
            let oldQty =
                planes.TryFind(plane)
                |> Option.defaultValue(0.0f)
            let newQty = oldQty - 1.0f
            let hangar = hangar.RemovePlane(af, plane, health, cost)
            let hangars = this.Hangars.Add(user.UserId, hangar)
            let airfields = this.Airfields.Add(af, planes.Add(plane, max 0.0f newQty))
            { this with Hangars = hangars; Airfields = airfields },
            [
                yield Status(hangars, airfields)
                if oldQty >= 1.0f && newQty < 1.0f then
                    yield PlanesAtAirfield(af, airfields.[af])
                match coalition, newQty with
                | Some coalition, x when int x <= 0 ->
                    yield Announce(coalition, [ sprintf "%s took the last %s from %s" user.Name plane.PlaneName af.AirfieldName ])
                | Some coalition, x ->
                    yield Announce(coalition, [ sprintf "%s entered a %s from %s (%d left)" user.Name plane.PlaneName af.AirfieldName (int x)])
                | None, _ ->
                    logger.Warn(sprintf "Plane checkout from a neutral region from %s" af.AirfieldName)
            ]

        | PlaneCheckIn(user, plane, health, af) ->
            let hangar =
                this.Hangars.TryFind(user.UserId)
                |> Option.defaultValue(emptyHangar(user.UserId, user.Name))
            let hangar = hangar.AddPlane(af, plane, health)
            let hangars = this.Hangars.Add(user.UserId, hangar)
            let planes =
                this.Airfields.TryFind(af)
                |> Option.defaultValue(Map.empty)
            let oldQty =
                planes.TryFind(plane)
                |> Option.defaultValue(0.0f)
            let newQty = oldQty + health
            let planes =
                planes.Add(plane, oldQty + health)
            let airfields = this.Airfields.Add(af, planes)
            { this with Hangars = hangars; Airfields = airfields },
            [
                yield Status(hangars, airfields)
                if oldQty < 1.0f && newQty >= 1.0f then
                    yield PlanesAtAirfield(af, airfields.[af])
                    match this.State.GetRegion(this.World.GetAirfield(af).Region).Owner with
                    | Some coalition ->
                        let origNumPlanes =
                            this.State.GetAirfield(af).NumPlanes
                            |> Map.map (fun _ -> int)
                        let spawnPlanes = Airfield.selectPlaneSpawns Airfield.maxPlaneSpawns coalition origNumPlanes
                        if Array.exists ((=) plane) spawnPlanes then
                            yield Announce(coalition, [sprintf "%s available at %s again" plane.PlaneName af.AirfieldName])
                        else
                            yield Announce(coalition, [sprintf "%s will be available at %s in the next mission" plane.PlaneName af.AirfieldName])
                    | None ->
                        ()
            ]

        | DeliverSupplies(supplies, region) ->
            let oldNeeds =
                this.RegionNeeds.TryFind(region)
                |> Option.defaultValue 0.0f<E>
            let newNeeds = oldNeeds - supplies
            { this with RegionNeeds = this.RegionNeeds.Add(region, newNeeds) }, []

        | RewardPlayer(user, reward) ->
            let hangar =
                this.Hangars.TryFind(user.UserId)
                |> Option.defaultValue(emptyHangar(user.UserId, user.Name))
            let reserve = min this.MaxCash (hangar.Reserve + reward)
            let hangar = { hangar with Reserve = reserve }
            let hangars = this.Hangars.Add(user.UserId, hangar)
            { this with Hangars = hangars },
            [
                Overview(user, 15, [sprintf "You have been awarded %1.0f" reward])
                Status(hangars, this.Airfields)
            ]

        | InformPlayerHangar(user) ->
            let hangar =
                this.Hangars.TryFind(user.UserId)
                |> Option.defaultValue(emptyHangar(user.UserId, user.Name))
            this,
            showHangar(hangar, 5)

        | PunishThief(user, plane, af) ->
            this,
            [
                Violation(user, "taking off in a plane you cannot afford")
            ]

        | Message m ->
            this, [m]

    member this.TryCheckoutPlane(user: UserIds, af: AirfieldId, plane: PlaneModel) =
        let hangar =
            this.Hangars.TryFind(user.UserId)
            |> Option.defaultValue (emptyHangar(user.UserId, user.Name))
        hangar.TryRemovePlane(af, plane, 1.0f)

    member this.GetClosestAirfield(v : Vector2) =
        this.State.State.Airfields
        |> Seq.minBy (fun afs -> let pos, _ = afs.Runway in (pos - v).Length())
        |> fun afs -> afs.AirfieldId

    member this.IsSpawnRestricted(af : AirfieldId, plane : PlaneModel, coalition : CoalitionId) =
        let availableAtAirfield =
            this.State.GetAirfield af
            |> fun afs -> afs.NumPlanes
            |> fun planes -> planes.TryFind plane
            |> Option.defaultValue 0.0f
        let isRestricted =
            let numOwned =
                this.State.State.Regions
                |> Seq.filter (fun region -> region.Owner = Some coalition)
                |> Seq.length
            if numOwned * 4 <= this.State.State.Regions.Length then
                false
            elif this.RearAirfields.Contains(af) then
                false
            elif this.State.GetRegion(this.World.GetAirfield(af).Region).HasInvaders then
                false
            else
                match plane.PlaneType with
                | Fighter -> availableAtAirfield < 10.0f
                | Attacker -> availableAtAirfield < 7.0f
                | Bomber -> availableAtAirfield < 5.0f
                | Transport -> false
        isRestricted

    member this.SupplyFlightFactor(start : AirfieldId, destination : AirfieldId) =
        let computeSupplyRewardFactor regStart regEnd =
            // Only give a positive reward if the transfer contributed to improve the overall picture
            if this.RegionNeeds.[regEnd] > this.RegionNeeds.[regStart] then
                // Reward long flights better than short flights
                let distance = this.World.GetRegion(regStart).Position - this.World.GetRegion(regEnd).Position
                let distance = distance.Length()
                distance / 70000.0f
                |> min 1.0f
                |> max 0.0f
                |> (*) 5.0f // Adjust because cargo rewards should be worth about the same as the damage they can inflict when used as bombs, not how much they cost to produce.
            else
                0.0f

        let regStart = this.World.GetAirfield(start).Region
        let regEnd = this.World.GetAirfield(destination).Region
        let factor =
            let coalitionStart = this.State.GetRegion(regStart).Owner
            let coalitionEnd = this.State.GetRegion(regEnd).Owner
            if coalitionStart = coalitionEnd then
                computeSupplyRewardFactor regStart regEnd
            else
                // Delivering goods to the enemy: negative reward!
                -1.0f
        factor

/// States in the PlayerFlightData state machine
type PlayerFlightState =
    | Spawned of cost:float32<E> option // Cost of taking off in the plane, None if player can't afford it, or plane is not available at airfield
    | Aborted
    | InFlight
    | Landed of AirfieldId option * TimeSpan
    | Disconnected
    | MissionEnded
    | StolePlane

/// The state of a player and their plane, and where they are in the flight sequence
type PlayerFlightData =
    { Player : UserIds
      Vehicle : int
      State : PlayerFlightState
      Health : float32
      Coalition : CoalitionId
      Plane : PlaneModel
      Cargo : float32<K>
      InitialBombs : float32<K>
      NumBombs : int
      Reward : float32<E>
      StartAirfield : AirfieldId
    }
with
    // Check if entry is a player joining a plane, show best destinations for supply flights
    static member TryCreate(context : Context, entry : PlayerPlaneEntry) =
        match context.Binding.TryFind(entry.VehicleId) with
        | Some(coalition, DynamicPlane plane) ->
            let cargo =
                if plane.Roles.Contains CargoTransporter then
                    let modmask, payload = plane.CargoPayload
                    if entry.Payload = payload then
                        plane.CargoCapacity
                    else
                        0.0f<K>
                else
                    0.0f<K>
            let weight =
                plane.BombLoads
                |> List.tryPick (fun (loadout, weight) -> if loadout = entry.Payload then Some weight else None)
                |> Option.defaultValue 0.0f<K>
            let af = context.GetClosestAirfield(Vector2(entry.Position.X, entry.Position.Z))
            let user = { UserId = string entry.UserId; Name = entry.Name }
            let supplyInfo =
                seq {
                    let bestDestinations =
                        context.World.World.Airfields
                        |> Seq.filter (fun af -> context.State.GetRegion(af.Region).Owner = Some coalition)
                        |> Seq.choose (fun afDest ->
                            match context.SupplyFlightFactor(af, afDest.AirfieldId) with
                            | x when x > 1.0f -> Some(afDest, x)
                            | _ -> None)
                        |> Seq.sortByDescending snd
                        |> Seq.truncate 3
                        |> Seq.map fst
                        |> List.ofSeq
                    match bestDestinations with
                    | [] ->
                        yield Message(Overview(user, 15, ["This airfield is not a good place to start a supply mission from"]))
                    | _ :: _ as x ->
                        yield Message(Overview(user, 15, ["The following regions would benefit from resupplies: " + (x |> List.map (fun af -> af.AirfieldId.AirfieldName) |> String.concat ", ")]))
                }
            let cost =
                if context.IsSpawnRestricted(af, plane, coalition) then
                    match context.TryCheckoutPlane(user, af, plane), context.Airfields.[af].TryFind plane with
                    | _, None
                    | None, _ -> None
                    | Some hangar, Some qty when qty >= 1.0f ->
                        let fundsBefore =
                            context.Hangars.TryFind(user.UserId)
                            |> Option.map(fun h -> h.Reserve)
                            |> Option.defaultValue 0.0f<E>
                        Some(fundsBefore - hangar.Reserve)
                    | _ -> None
                else
                    Some 0.0f<E>
            let planeInfo =
                [
                    match cost with
                    | None ->
                        yield Message(Warning(user, 0, [sprintf "You are not allowed to take off in a %s at %s" plane.PlaneName af.AirfieldName]))
                    | Some 0.0f<E> ->
                        yield Message(Overview(user, 0, [sprintf "You are cleared to take off in a %s from %s" plane.PlaneName af.AirfieldName]))
                        yield PlaneCheckOut(user, plane, 1.0f, 0.0f<E>, af)
                    | Some cost ->
                        yield Message(Overview(user, 0, [sprintf "It will cost you %0.0f to take off in a %s from %s" cost plane.PlaneName af.AirfieldName]))
                        yield PlaneCheckOut(user, plane, 1.0f, cost, af)
                ]
            let supplyCommands =
                if cargo > 0.0f<K> || weight >= 500.0f<K> then
                    List.ofSeq supplyInfo
                else
                    []
            Some {
                Player = user
                Vehicle = entry.VehicleId
                State = Spawned cost
                Health = 1.0f
                Coalition = coalition
                Plane = plane
                Cargo = cargo
                NumBombs = entry.Bombs
                InitialBombs = weight
                Reward = 0.0f<E>
                StartAirfield = af
            }, List.concat [supplyCommands; planeInfo]
        | Some _
        | None ->
            None, []

    // Handle first take off after spawn
    member this.HandleTakeOff(context : Context, takeOff : TakeOffEntry) =
        match this.State with
        | Spawned(Some cost) ->
            { this with State = InFlight },
            [
                yield Message(
                        Announce(
                            this.Coalition,
                            [sprintf "%s has taken off from %s in a %s" this.Player.Name this.StartAirfield.AirfieldName (string this.Plane.PlaneType)]))
            ]
        | Spawned None ->
            { this with State = StolePlane },
            [ PunishThief(this.Player, this.Plane, this.StartAirfield) ]
        | unexpected ->
            logger.Warn(sprintf "Unexpected state during take off %A" unexpected)
            this, []

    // Handle take off after landing
    member this.HandleTakeOffAgain() =
        { this with State = InFlight }, []

    // Diminish health by received damage amount
    member this.HandleReceivedDamage(context : Context, damage : DamageEntry) =
        { this with Health = this.Health - damage.Damage }, []

    // Increase reward depending on target and amount of damage
    member this.HandleInflictedDamage(context : Context, damage : DamageEntry) =
        let value =
            match context.Binding.TryFind(damage.TargetId) with
            | Some(_, StaticPlane _) ->
                0.0f<E>
            | Some(_, DynamicPlane plane) ->
                plane.Cost / 5.0f
            | Some(_, AirfieldBuilding(group, idx))
            | Some(_, Storage(group, idx)) ->
                let specs = context.World.World.SubBlockSpecs
                let buildings = group.SubBlocks specs
                if Array.exists ((=) idx) buildings then
                    (group.RepairCost specs + group.Storage specs) / float32 buildings.Length
                else
                    0.0f<E>
            | Some(_, Production(group, idx)) ->
                let specs = context.World.World.SubBlockSpecs
                let buildings = group.SubBlocks specs
                if Array.exists ((=) idx) buildings then
                    (group.RepairCost specs) / float32 buildings.Length
                else
                    0.0f<E>
            | Some(_, CargoShip) ->
                Orders.ResupplyOrder.ShipCapacity
            | Some(_, BigEscortShip) ->
                Orders.ResupplyOrder.ShipCapacity
            | Some(_, SmallEscortShip) ->
                Orders.ResupplyOrder.ShipCapacity / 3.0f
            | Some(_, LandingShip) ->
                float32 Orders.shipVehicleCapacity * GroundAttackVehicle.MediumTankCost
            | Some(_, Artillery) ->
                cannonCost
            | Some(_, LightMachineGun) ->
                lightMachineGunCost
            | Some(_, HeavyMachineGun) ->
                heavyMachineGunCost
            | Some(_, StaticTank tank)
            | Some(_, DynamicTank tank) ->
                tank.Cost
            | Some(_, ConvoyTruck) ->
                Orders.ResupplyOrder.TruckCapacity
            | Some(_, TrainWagon) ->
                Orders.ResupplyOrder.TrainCapacity / 8.0f
            | Some(_, Locomotive) ->
                Orders.ResupplyOrder.TrainCapacity / 8.0f
            | Some(_, StaticObject _) ->
                0.0f<E>
            | None ->
                0.0f<E>
        let factor =
            match context.Binding.TryFind(damage.TargetId) with
            | Some (coalition, _) ->
                if coalition = this.Coalition then
                    -1.0f
                else
                    1.0f
            | None ->
                0.0f
        let productionLoss =
            match context.Binding.TryFind(damage.TargetId) with
            | Some(_, Production(group, idx)) ->
                let specs = context.World.World.SubBlockSpecs
                let buildings = group.SubBlocks specs
                if Array.exists ((=) idx) buildings then
                    let timeToRepair =
                        (group.RepairCost specs) / float32 buildings.Length / context.World.World.RepairSpeed
                    0.5f * group.Production(specs, context.World.World.ProductionFactor) / float32 buildings.Length * timeToRepair
                else
                    0.0f<E>
            | _ ->
                0.0f<E>
        let reward = factor * (damage.Damage * value + productionLoss)
        { this with Reward = this.Reward + reward }, []

    // Set health to 0
    member this.HandleKilled(context : Context, killed : KillEntry) =
        { this with Health = 0.0f }, []

    // Check airfield where we landed, change state to landed
    member this.HandleLanding(context : Context, landing : LandingEntry) =
        let pos = Vector2(landing.Position.X, landing.Position.Z)
        let af = context.GetClosestAirfield(pos)
        let af = context.World.GetAirfield(af)
        let afCheckout =
            if (af.Pos - pos).Length() < 3000.0f then
                Some af.AirfieldId
            else
                None
        // Calculate reward from cargo, reward is secured later at mission end
        let delivered, reward =
            match afCheckout, this.Health with
            | Some af, x when x > 0.0f -> this.Cargo, this.Cargo * bombCost * context.SupplyFlightFactor(this.StartAirfield, af)
            | Some _, _
            | None, _ -> 0.0f<K>, 0.0f<E>
        let cmds =
            [
                match afCheckout with
                | Some af when delivered > 0.0f<K>->
                    let reg = context.World.GetAirfield(af).Region
                    yield DeliverSupplies(delivered * bombCost, reg)
                    yield Message(Announce(this.Coalition, [sprintf "%s has delivered %0.0f Kg of cargo to %s" this.Player.Name delivered af.AirfieldName]))
                | _ -> ()
            ]
        { this with State = Landed(afCheckout, landing.Timestamp); Cargo = this.Cargo - delivered; Reward = this.Reward + reward }, cmds

    // Announce landing of healthy planes, kick players delivering planes to the enemy.
    member this.AnnounceLanding(context : Context, af : AirfieldId option) =
        let cmds =
            [
                match af with
                | Some af ->
                    if this.Health > 0.0f then
                        yield Message(
                            Announce(
                                this.Coalition,
                                [sprintf "%s is back at %s" this.Player.Name af.AirfieldName]))
                    let afCoalition =
                        context.State.GetRegion(context.World.GetAirfield(af).Region).Owner
                    if afCoalition = Some this.Coalition.Other then
                        yield Message(
                            Announce(
                                this.Coalition.Other,
                                [sprintf "%s (an enemy!) has landed at %s" this.Player.Name af.AirfieldName]))
                        if this.Health > 0.5f then
                            yield Message(Violation(this.Player, "landing on an enemy airfield"))
                | None ->
                    if this.Health > 0.0f then
                        yield Message(
                            Announce(
                                this.Coalition,
                                [sprintf "%s landed in the rough" this.Player.Name]))
            ]
        this, cmds

    // Check in what's left of the plane, award reward
    member this.HandleMissionEnd(context : Context, af : AirfieldId option, bombs : int option) =
        match af, this.Health with
        | Some af, health when health > 0.0f ->
            let suppliesTransfered =
                match bombs with
                | Some bombs when bombs = this.NumBombs -> this.InitialBombs
                | _ -> 0.0f<K>
            let supplyReward = context.SupplyFlightFactor(this.StartAirfield, af) * suppliesTransfered
            { this with State = MissionEnded },
            [
                assert(this.State <> MissionEnded)
                let healthUp = ceil(health * 10.0f) / 10.0f
                yield PlaneCheckIn(this.Player, this.Plane, healthUp, af)
                yield DeliverSupplies(bombCost * (this.Cargo + suppliesTransfered), context.World.GetAirfield(af).Region)
                yield RewardPlayer(this.Player, supplyReward * bombCost + this.Reward)
                yield InformPlayerHangar(this.Player)
                // Try to show PIN
                match
                    (try
                        Message(PlayerEntered(Guid(this.Player.UserId)))
                        |> Some
                     with _ -> None)
                    with
                    | Some m -> yield m
                    | None -> ()
            ]
        | _ ->
            { this with State = MissionEnded }, []

    // Player disconnected, return plane to start airfield if undamaged
    member this.HandlePrematureMissionEnd(context : Context) =
        assert(this.State <> MissionEnded)
        { this with State = MissionEnded },
        [
            if this.Health >= 1.0f then
                yield PlaneCheckIn(this.Player, this.Plane, 1.0f, this.StartAirfield)
        ]

    // Player ended mission before taking off, cancel mission
    member this.HandleAbortionBeforeTakeOff(context, doCheckIn) =
        assert(this.State <> MissionEnded)
        { this with State = MissionEnded },
        [
            if doCheckIn then
                yield PlaneCheckIn(this.Player, this.Plane, this.Health, this.StartAirfield)
        ]

    // Player disconnected, mark as mission ended
    member this.HandleDisconnection(context) =
        { this with State = MissionEnded }, []

    member this.HandleEntry (context : Context, entry : LogEntry) =
        match this.State, entry with
        | Spawned _, (:? TakeOffEntry as takeOff) when takeOff.VehicleId = this.Vehicle ->
            this.HandleTakeOff(context, takeOff)
        | Landed _, (:? TakeOffEntry as takeOff) when takeOff.VehicleId = this.Vehicle ->
            this.HandleTakeOffAgain()
        | _, (:? TakeOffEntry as takeOff) when takeOff.VehicleId = this.Vehicle ->
            logger.Warn(sprintf "Spurious take off event for %s in %s id %d" this.Player.Name this.Plane.PlaneName this.Vehicle)
            this, []
        | _, (:? DamageEntry as damage) when damage.AttackerId = this.Vehicle ->
            this.HandleInflictedDamage(context, damage)
        | _, (:? DamageEntry as damage) when damage.TargetId = this.Vehicle ->
            this.HandleReceivedDamage(context, damage)
        | _, (:? KillEntry as killed) when killed.TargetId = this.Vehicle ->
            this.HandleKilled(context, killed)
        | InFlight, (:? LandingEntry as landing) when landing.VehicleId = this.Vehicle ->
            this.HandleLanding(context, landing)
        | _, (:? LandingEntry as landing) when landing.VehicleId = this.Vehicle ->
            logger.Warn(sprintf "Spurious landing event for %s in %s id %d" this.Player.Name this.Plane.PlaneName this.Vehicle)
            this, []
        | Landed(af, _), (:? PlayerMissionEndEntry as finish) when finish.VehicleId = this.Vehicle ->
            this.HandleMissionEnd(context, af, Some finish.Bombs)
        | Landed(af, _), (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this.HandleMissionEnd(context, af, None)
        | InFlight, (:? PlayerMissionEndEntry as finish) when finish.VehicleId = this.Vehicle ->
            this.HandlePrematureMissionEnd(context)
        | InFlight, (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this.HandlePrematureMissionEnd(context)
        | Spawned cost, (:? PlayerMissionEndEntry as finish) when finish.VehicleId = this.Vehicle ->
            this.HandleAbortionBeforeTakeOff(context, cost.IsSome)
        | Spawned cost, (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this.HandleAbortionBeforeTakeOff(context, cost.IsSome)
        | MissionEnded, (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this, []
        | _, (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this.HandleDisconnection(context)
        | _ ->
            this, []

        // Wait a second after landing to properly handle crashes when announcing landings
        |> fun (this2, cmds) ->
            match this.State with // "this": Check old value, we might have just switched to MissionEnded
            | Landed(af, ts) when (entry.Timestamp - ts).TotalSeconds > 1.0 ->
                this2.AnnounceLanding(context, af) // "this2": Use updated health value
            | _ ->
                this2, cmds


/// Monitor events and check that players don't take off in planes they are not allowed to fly according to player hangars.
/// Also send information about player hangars.
let checkPlaneAvailability maxCash (world : World) (state : WorldState) (hangars : Map<string, PlayerHangar>) (entries : AsyncSeq<LogEntry>) =

    asyncSeq {
        let mutable context = Context.Create(world, state, hangars, maxCash)
        let mutable players : Map<int, PlayerFlightData> = Map.empty

        for entry in entries do
            let mutable cmds0 = []
            // Handle special entries
            match entry with
            | :? JoinEntry as joined ->
                yield PlayerEntered(joined.UserId)
                match context.Hangars.TryFind(string joined.UserId) with
                | Some hangar ->
                    yield! AsyncSeq.ofSeq(showHangar(hangar, 15))
                | None ->
                    ()
            | :? ObjectSpawnedEntry as spawned ->
                context <- context.HandleBinding(spawned)
            | :? DamageEntry as damage ->
                context <- context.ResolveBindings(damage)
            | :? PlayerPlaneEntry as plane ->
                match PlayerFlightData.TryCreate(context, plane) with
                | Some data, cmds ->
                    players <- players.Add(data.Vehicle, data)
                    cmds0 <- cmds
                | None, cmds ->
                    cmds0 <- cmds
            | _ -> ()

            // Execute commands gathered from player data creation
            for cmd in cmds0 do
                let ctx, msgs = context.Execute(cmd)
                yield! AsyncSeq.ofSeq msgs
                context <- ctx

            // Update player data and context
            let players2 = players |> Map.map(fun vehicleId data -> data.HandleEntry(context, entry))
            players <-
                players2
                |> Map.map (fun _ (player, _) -> player)
                |> Map.filter (fun _ player ->
                    match player.State with
                    | MissionEnded -> false
                    | _ -> true)

            let context2, msgs =
                players2
                |> Map.toSeq
                |> Seq.collect (snd >> snd) // Turn into a command seq
                |> Seq.fold (fun (context : Context, msgs) cmd ->
                    let context, msgs2 = context.Execute(cmd)
                    context, msgs2 @ msgs) (context, [])
            context <- context2

            // Yield messages generated while updating player data
            yield! AsyncSeq.ofSeq msgs
    }
