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

module Campaign.PlaneChecksStatus

open System
open System.Numerics
open NLog
open ploggy
open FSharp.Control
open Campaign.PlayerDiscipline
open Campaign.BasicTypes
open Campaign.WorldDescription
open Campaign.PlaneModel
open Campaign.WorldState
open Campaign
open Campaign.PlaneChecksContext

let private logger = LogManager.GetCurrentClassLogger()

type TransactionCost =
    | Denied of reason:string
    | FreshSpawn of PlaneType * float32
    | Free
with
    member this.IsDeniedCase =
        match this with
        | Denied _ -> true
        | _ -> false

/// States in the PlayerFlightData state machine
type PlayerFlightState =
    | Spawned
    | Aborted
    | InFlight
    | Landed of AirfieldId option * TimeSpan option
    | Disconnected
    | MissionEnded
    | StolePlane

/// The state of a player and their plane, and where they are in the flight sequence
type PlayerFlightData =
    { Player : UserIds
      Vehicle : int
      State : PlayerFlightState
      CheckoutCost : TransactionCost
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
                let hangar = context.GetHangar(user, coalition)
                if hangar.HasReservedPlane(af, plane) then
                    Free
                elif context.IsSpawnRestricted(af, plane, coalition) then
                    Denied "Another pilot has reserved that plane; Bring one from the rear airfield to earn a reservation"
                elif context.RearAirfields.Contains(af) then
                    let numFreshSpawnsLeft = hangar.FreshSpawns.TryFind(plane.PlaneType) |> Option.defaultValue 0.0f
                    let rearValueFactor = context.GetRearValueFactor(plane)
                    match numFreshSpawnsLeft with
                    | x when x >= rearValueFactor ->
                        FreshSpawn(plane.PlaneType, rearValueFactor)
                    | x ->
                        let alts =
                            hangar.FreshSpawns
                            |> Map.toSeq
                            |> Seq.filter (fun (_, qty) -> qty >= 1.0f)
                            |> Seq.map fst
                            |> Set.ofSeq
                            |> fun planeTypes -> context.GetFreshSpawnAlternatives(planeTypes, af)
                            |> Seq.filter (fun plane -> x >= context.GetRearValueFactor(plane))
                            |> List.ofSeq
                        match alts with
                        | [] ->
                            Denied "You have exhausted all your fresh spawns; They refill partially every new mission"
                        | planes ->
                            let planes =
                                planes
                                |> List.map (fun plane -> plane.PlaneName)
                                |> String.concat ", "
                            Denied (sprintf "You have exhausted your fresh spawns in that plane, try one of the following instead: %s" planes)
                else
                    Free
            // Deny if loadout is not OK
            let cost =
                let afs = context.State.GetAirfield(af)
                let payload = plane.GetPayLoadCost(entry.Payload, bombCost)
                if payload > 0.0f<E> && afs.Supplies < payload then
                    Denied "Airfield supplies are no longer sufficient to provide this loadout"
                else
                    cost
            let planeInfo =
                [
                    let rank = context.GetHangar(user, coalition).RankedName
                    match cost with
                    | Denied reason ->
                        yield Message(Warning(user, 0,
                                        [sprintf "%s, you are not allowed to take off in a %s at %s" rank plane.PlaneName af.AirfieldName
                                         reason
                                         "You will be KICKED if you take off"]))
                    | Free | FreshSpawn _ ->
                        yield Message(Overview(user, 0, [sprintf "%s, you are cleared to take off in a %s from %s" rank plane.PlaneName af.AirfieldName]))
                        yield PlaneCheckOut(user, plane, af)
                        if cost = Free then
                            yield RemoveReservedPlane(user, plane, af, coalition)
                ]
            let supplyCommands =
                if cargo > 0.0f<K> || weight >= 500.0f<K> then
                    List.ofSeq supplyInfo
                else
                    []
            Some {
                Player = user
                Vehicle = entry.VehicleId
                State = Spawned
                Health = 1.0f
                CheckoutCost = cost
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
        | Spawned when this.CheckoutCost.IsDeniedCase ->
            { this with State = StolePlane },
            [ PunishThief(this.Player, this.Plane, this.StartAirfield) ]
        | Spawned ->
            { this with State = InFlight },
            [
                match this.CheckoutCost with
                | FreshSpawn(planeType, factor) ->
                    yield PlayerFreshSpawn(this.Player, this.Coalition, planeType, factor)
                | _ -> ()

                let rank = context.GetHangar(this.Player, this.Coalition).RankedName
                yield Message(
                        Announce(
                            this.Coalition,
                            [sprintf "%s has taken off from %s in a %s" rank this.StartAirfield.AirfieldName (string this.Plane.PlaneType)]))
            ]
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
        this.AccumulateDamageReward(context, damage.TargetId, damage.Damage)

    member this.HandleKill(context : Context, kill : KillEntry) =
        let targetHealth = context.GetObjectHealth(kill.TargetId)
        this.AccumulateDamageReward(context, kill.TargetId, targetHealth)

    member this.AccumulateDamageReward(context : Context, target : int, damage : float32) =
        let value =
            match context.Binding.TryFind(target) with
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
                tank.Cost / 10.0f
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
            match context.Binding.TryFind(target) with
            | Some (coalition, _) ->
                if coalition = this.Coalition then
                    -1.0f
                else
                    1.0f
            | None ->
                0.0f
        let productionLoss =
            match context.Binding.TryFind(target) with
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
        let reward = factor * (damage * value + productionLoss)
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
                    let rank = context.GetHangar(this.Player, this.Coalition).RankedName
                    yield Message(Announce(this.Coalition, [sprintf "%s has delivered %0.0f Kg of cargo to %s" rank delivered af.AirfieldName]))
                | _ -> ()
            ]
        { this with State = Landed(afCheckout, Some landing.Timestamp); Cargo = this.Cargo - delivered; Reward = this.Reward + reward }, cmds

    // Announce landing of healthy planes, kick players delivering planes to the enemy.
    member this.AnnounceLanding(context : Context, af : AirfieldId option) =
        let rank = context.GetHangar(this.Player, this.Coalition).RankedName
        let cmds =
            [
                match af with
                | Some af ->
                    if this.Health > 0.0f then
                        let afCoalition =
                            context.State.GetRegion(context.World.GetAirfield(af).Region).Owner
                        if afCoalition = Some this.Coalition.Other then
                            yield Message(
                                Announce(
                                    this.Coalition.Other,
                                    [sprintf "%s (an enemy!) has landed at %s" rank af.AirfieldName]))
                            if this.Health > 0.5f then
                                yield Message(Violation(this.Player, "landing on an enemy airfield"))
                        else
                            yield Message(
                                Announce(
                                    this.Coalition,
                                    [sprintf "%s is back at %s" rank af.AirfieldName]))
                    else
                        yield Message(
                            Announce(
                                this.Coalition,
                                [sprintf "%s has crashed near %s" rank af.AirfieldName]))

                | None ->
                    if this.Health > 0.0f then
                        yield Message(
                            Announce(
                                this.Coalition,
                                [sprintf "%s landed in the rough" rank]))
                    else
                        yield Message(
                            Announce(
                                this.Coalition,
                                [sprintf "%s has crashed" rank]))
            ]
        { this with State = Landed(af, None) }, cmds

    // Check in what's left of the plane, award reward
    member this.HandleMissionEnd(context : Context, af : AirfieldId option, bombs : int option) =
        assert(this.State <> MissionEnded)
        match af, this.Health with
        | Some af, health when health > 0.0f ->
            let isCorrectCoalition = context.GetAirfieldCoalition(af) = Some this.Coalition
            let suppliesTransfered =
                match bombs with
                | Some bombs when bombs = this.NumBombs -> this.InitialBombs
                | _ -> 0.0f<K>
            let supplyReward = context.SupplyFlightFactor(this.StartAirfield, af) * suppliesTransfered
            { this with State = MissionEnded },
            [
                let healthUp = ceil(health * 10.0f) / 10.0f
                yield PlaneCheckIn(this.Player, this.Plane, healthUp, af)
                if isCorrectCoalition then
                    yield AddReservedPlane(this.Player, this.Plane, healthUp, af, this.Coalition)
                // Try to show PIN
                match
                    (try
                        Message(PlayerEntered(Guid(this.Player.UserId)))
                        |> Some
                     with _ -> None)
                    with
                    | Some m -> yield m
                    | None -> ()
                // Reward real flights, i.e. exclude phony flights, landing at the same airfield take-off took place from.
                match this.State with
                | Landed(Some af2, _) when af2 <> af ->
                    yield DeliverSupplies(bombCost * (this.Cargo + suppliesTransfered), context.World.GetAirfield(af).Region)
                    yield RewardPlayer(this.Player, this.Coalition, supplyReward * bombCost + this.Reward)
                | _ ->
                    ()
            ]
        | _ ->
            { this with State = MissionEnded }, []

    // Player disconnected, return plane to start airfield if undamaged
    member this.HandlePrematureMissionEnd(context : Context) =
        assert(this.State <> MissionEnded)
        { this with State = MissionEnded },
        [
            match this with
            | { CheckoutCost = Denied _ } ->
                ()
            | { Health = health } when health >= 1.0f ->
                yield PlaneCheckIn(this.Player, this.Plane, 1.0f, this.StartAirfield)
                yield AddReservedPlane(this.Player, this.Plane, this.Health, this.StartAirfield, this.Coalition)
            | _ ->
                ()
        ]

    // Player ended mission before taking off, cancel mission
    member this.HandleAbortionBeforeTakeOff(context, doCheckIn) =
        assert(this.State <> MissionEnded)
        { this with State = MissionEnded },
        [
            yield PlaneCheckIn(this.Player, this.Plane, this.Health, this.StartAirfield)
            if doCheckIn then
                yield AddReservedPlane(this.Player, this.Plane, this.Health, this.StartAirfield, this.Coalition)
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
        | _, (:? KillEntry as kill) when kill.AttackerId = this.Vehicle ->
            this.HandleKill(context, kill)
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
        | Spawned, (:? PlayerMissionEndEntry as finish) when finish.VehicleId = this.Vehicle ->
            this.HandleAbortionBeforeTakeOff(context, not this.CheckoutCost.IsDeniedCase)
        | Spawned, (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this.HandleAbortionBeforeTakeOff(context, not this.CheckoutCost.IsDeniedCase)
        | MissionEnded, (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this, []
        | _, (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this.HandleDisconnection(context)
        | _ ->
            this, []

        // Wait a second after landing to properly handle crashes when announcing landings
        |> fun (this2, cmds) ->
            match this.State with // "this": Check old value, we might have just switched to MissionEnded
            | Landed(af, Some ts) when (entry.Timestamp - ts).TotalSeconds > 1.0 ->
                let this2, cmds2 = this2.AnnounceLanding(context, af) // "this2": Use updated health value
                this2, cmds @ cmds2
            | _ ->
                this2, cmds

