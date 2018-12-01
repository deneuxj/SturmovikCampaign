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
open System.ServiceModel
open System.Text.RegularExpressions
open Campaign

let private logger = LogManager.GetCurrentClassLogger()

type PlaneGift =
    { GiverGuid : string
      Recipient : (string * string) option
      Airfield : AirfieldId
      Plane : PlaneModel
    }
with
    override this.ToString() =
        let recipient =
            match this.Recipient with
            | None -> "ALL"
            | Some (guid, name) -> sprintf "'%s'/'%s'" guid (name.Replace("'", ""))
        sprintf "PLANEGIFT:'%s' RECIPIENT:%s PLANE:'%s' AIRFIELD:'%s'" this.GiverGuid recipient this.Plane.PlaneName this.Airfield.AirfieldName

    static member TryFromString(s) =
        let m = Regex.Match(s, "PLANEGIFT:'(.*)' RECIPIENT:(ALL|'.*'/'.*') PLANE:'(.*)' AIRFIELD:'(.*)'")
        if m.Success then
            let planeName = m.Groups.[3].Value
            let recipient =
                match m.Groups.[2].Value with
                | "ALL" -> Ok None
                | s ->
                    let m2 = Regex.Match(s, "'(.*)'/'(.*)'")
                    if m2.Success then
                        Ok(Some(m2.Groups.[1].Value, m2.Groups.[1].Value))
                    else
                        Error()
            match recipient, PlaneModel.AllModels |> List.tryFind (fun plane -> plane.PlaneName = planeName) with
            | Ok(recipient), Some plane ->
                Some {
                    GiverGuid = m.Groups.[1].Value
                    Recipient = recipient
                    Plane = plane
                    Airfield = AirfieldId(m.Groups.[4].Value)
                }
            | _, None | Error _, _->
                None
        else
            None

/// Messages sent to the live commenter
type PlaneAvailabilityMessage =
    | PlayerEntered of Guid
    | Overview of UserIds * delay:int * string list
    | Warning of UserIds * delay:int * string list
    | Announce of CoalitionId * string list
    | Violation of UserIds * string
    | Status of Map<string * CoalitionId, PlayerHangar> * Map<AirfieldId, Map<PlaneModel, float32>>
    | PlanesAtAirfield of AirfieldId * Map<PlaneModel, float32>

/// Show hangar to its player
let showHangar(hangar : PlayerHangar, delay) =
    [
        let userIds = { UserId = string hangar.Player; Name = hangar.PlayerName }
        let getNumFreshSpawns planeType =
            hangar.FreshSpawns.TryFind(planeType) |> Option.defaultValue 0.0f
        let numFighters = getNumFreshSpawns PlaneType.Fighter
        let numAttackers = getNumFreshSpawns PlaneType.Attacker
        let numBombers = getNumFreshSpawns PlaneType.Bomber
        let numTransports = getNumFreshSpawns PlaneType.Transport
        yield Overview(userIds, delay,
            [
                sprintf "Welcome back %s" hangar.RankedName
                sprintf "Fresh spawns at rear AF: F:%0.1f A:%0.1f B:%0.1f T:%0.1f" numFighters numAttackers numBombers numTransports
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
let private emptyHangar (playerId : string, playerName, coalition, cash, freshSpawns) =
    { Player = Guid(playerId); PlayerName = playerName; Coalition = coalition; Reserve = cash; Airfields = Map.empty; FreshSpawns = freshSpawns }

/// Commands sent during state transitions in PlayerStateData to the main asyncSeq computation
type Command =
    | PlaneCheckOut of user:UserIds * PlaneModel * health:float32 * isBorrowed:bool * AirfieldId
    | PlaneCheckIn of user:UserIds * userCoalition:CoalitionId * PlaneModel * health:float32 * isBorrowed:bool * AirfieldId
    | PlayerPayed of user:UserIds * CoalitionId * float32<E>
    | PlayerFreshSpawn of user:UserIds * CoalitionId * PlaneType * float32
    | PlayerReturnedBorrowedPlane of user:UserIds * PlaneModel * originalCost:float32<E> * AirfieldId
    | DeliverSupplies of float32<E> * RegionId
    | RewardPlayer of user:UserIds * CoalitionId * float32<E>
    | InformPlayerHangar of UserIds * CoalitionId
    | PunishThief of user:UserIds * PlaneModel * AirfieldId
    | Message of PlaneAvailabilityMessage
    | PlaneGifted of PlaneGift

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

type Limits =
    {
      InitialCash : float32<E>
      MaxCash : float32<E>
      PlaneRentalAllowed : bool
      MoneyBackFactor : float32
      MaxReservedPlanes : int
      MaxTotalReservedPlanes : int
      SpawnsAreRestricted : bool
      RearAirfieldCostFactor : float32
      MaxRentalGain : float32<E>
      FreshSpawns : Map<PlaneType, float32>
    }
with
    static member FromConfig(config : Campaign.Configuration.Configuration) =
        let freshSpawns =
            [(Fighter, config.FreshFighterSpawns); (Attacker, config.FreshAttackerSpawns); (Bomber, config.FreshBomberSpawns); (Transport, config.FreshTransportSpawns)]
            |> List.map (fun (planeType, qty) -> (planeType, float32 qty))
            |> Map.ofList
        { InitialCash = 1.0f<E> * float32 config.InitialCash
          MaxCash = 1.0f<E> * float32 config.MaxCash
          MoneyBackFactor = config.MoneyBackFactor
          MaxReservedPlanes = config.MaxReservedPlanes
          MaxTotalReservedPlanes = config.MaxTotalReservedPlanes
          SpawnsAreRestricted = config.SpawnsAreRestricted
          RearAirfieldCostFactor = config.RearAirfieldCostFactor
          PlaneRentalAllowed = config.PlaneRentalAllowed
          MaxRentalGain = PlaneModel.basePlaneCost / 5.0f
          FreshSpawns = freshSpawns
        }

    member this.ShowCashReserves = this.PlaneRentalAllowed

/// Keeps track of information needed for the state transitions in PlayerFlightData
type Context =
    { World : WorldFastAccess
      State : WorldStateFastAccess
      Binding : Map<int, CoalitionId * ObjectInstance>
      ObjectHealth : Map<int, float32>
      Hangars : Map<string * CoalitionId, PlayerHangar>
      Airfields : Map<AirfieldId, Map<PlaneModel, float32>>
      RearAirfields : Set<AirfieldId>
      RegionNeeds : Map<RegionId, float32<E>>
      Limits : Limits
    }
with
    static member Create(missionLength : float32<H>, world : World, state : WorldState, hangars : Map<string * CoalitionId, PlayerHangar>, limits : Limits) =
        let rearAirfields =
            [Axis; Allies]
            |> List.choose (fun coalition -> world.RearAirfields.TryFind coalition)
            |> Set.ofList
        let needs = AutoOrder.computeSupplyNeeds missionLength world state
        let airfields =
            state.Airfields
            |> Seq.map (fun afs -> afs.AirfieldId, afs.NumPlanes)
            |> Map.ofSeq
        {
            World = world.FastAccess
            State = state.FastAccess
            Binding = Map.empty
            ObjectHealth = Map.empty
            Hangars = hangars
            Airfields = airfields
            RearAirfields = rearAirfields
            RegionNeeds = needs
            Limits = limits
        }

    /// Bind a vehicle ID to a coalition and a type of object
    member this.HandleBinding(spawn : ObjectSpawnedEntry) =
        let (|PlaneObjectType|_|) = planeObjectType this.World.World.PlaneSet
        let (|StaticPlaneType|_|) = staticPlaneType this.World.World.PlaneSet
        let (|Named|_|) (pat : string) (x : string) =
            if x.ToLowerInvariant().Contains(pat.ToLowerInvariant()) then
                Some()
            else
                None

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
            | Named "PzKpfw III Ausf.H", _
            | Named "_PzKpfw III Ausf.L", _
            | Named "T-34-76 STZ", _
            | Named "_T-34-76 STZ", _ -> DynamicTank HeavyTank
            | Named "Destroyer Type 7", _ -> BigEscortShip
            | Named "Opel Blitz", _
            | Named "GAZ-AA", _
            | Named "GAZ-M", _ -> ConvoyTruck
            | Named "Landing Boat type A", _ -> LandingShip
            | Named "River Cargo Ship type Georgia AAA", _
            | Named "Large Cargo Ship type 1", _
            | Named "Large Tanker Ship type 1", _ -> CargoShip
            | Named "Locomotive_E", _
            | Named "Locomotive_G8", _ -> Locomotive
            | Named "T-70", _
            | Named "PzKpfw IV Ausf.F1", _ -> DynamicTank MediumTank
            | Named "Sd Kfz 10 Flak 38", _
            | Named "Sd Kfz 251 Wurfrahmen 40", _ -> DynamicTank LightArmor
            | Named "Torpedo Boat type S-38", _
            | Named "Torpedo Boat G-5 series 11-bis 213", _ -> SmallEscortShip
            | Named "wagon_tankb", _ -> TrainWagon
            | Named "zis-3", _ -> ConvoyTruck
            | Named "ZiS-5 72-K", _ -> DynamicTank LightArmor
            | Named "zis-5", _ -> ConvoyTruck
            | Named "ZiS-6 BM-13", _ -> DynamicTank LightArmor
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

    member this.GetObjectHealth(idx : int) =
        this.ObjectHealth
        |> Map.tryFind idx
        |> Option.defaultValue 1.0f

    member this.HandleDamage(damage : DamageEntry) =
        let oldHealth = this.GetObjectHealth(damage.TargetId)
        let newHealth = oldHealth - damage.Damage
        { this with ObjectHealth = this.ObjectHealth.Add(damage.TargetId, newHealth) }

    member this.HandleKill(kill : KillEntry) =
        { this with ObjectHealth = this.ObjectHealth.Add(kill.TargetId, 0.0f) }

    /// Execute a command.
    /// Commands are typically created by PlayerFlightData instances, when they hand game log entries.
    member this.Execute(command : Command) =
        match command with
        | PlaneCheckOut(user, plane, health, isBorrowed, af) ->
            let coalition =
                this.State.GetRegion(this.World.GetAirfield(af).Region).Owner
            match coalition with
            | Some coalition ->
                let hangar : PlayerHangar = this.GetHangar(user, coalition)
                let planes =
                    this.Airfields.TryFind(af)
                    |> Option.defaultValue(Map.empty)
                let oldQty =
                    planes.TryFind(plane)
                    |> Option.defaultValue(0.0f)
                let newQty = oldQty - 1.0f
                let hangar =
                    if not isBorrowed then
                        hangar.RemovePlane(af, plane, health, 0.0f<E>)
                    else
                        hangar
                let hangars = this.Hangars.Add((user.UserId, coalition), hangar)
                let airfields = this.Airfields.Add(af, planes.Add(plane, max 0.0f newQty))
                { this with Hangars = hangars; Airfields = airfields },
                [
                    yield Status(hangars, airfields)
                    if oldQty >= 1.0f && newQty < 1.0f then
                        yield PlanesAtAirfield(af, airfields.[af])
                    if int newQty <= 0 then
                        yield Announce(coalition, [ sprintf "%s took the last %s from %s" hangar.RankedName plane.PlaneName af.AirfieldName ])
                    else
                        yield Announce(coalition, [ sprintf "%s entered a %s from %s (%d left)" hangar.RankedName plane.PlaneName af.AirfieldName (int newQty)])
                ]
            | None ->
                logger.Error("Attempt to check out plane from neutral region")
                this, []

        | PlayerPayed(user, coalition, cost) ->
            let hangar : PlayerHangar = this.GetHangar(user, coalition)
            let hangar = { hangar with Reserve = hangar.Reserve - cost }
            let hangars = this.Hangars.Add((user.UserId, coalition), hangar)
            { this with Hangars = hangars },
            [ Overview(user, 0, [ sprintf "You have have spent %0.0f" cost ]) ]

        | PlayerFreshSpawn(user, coalition, planeType, factor) ->
            let hangar : PlayerHangar = this.GetHangar(user, coalition)
            let oldSpawns = hangar.FreshSpawns.TryFind(planeType) |> Option.defaultValue 0.0f
            let newSpawns = oldSpawns - factor |> max 0.0f
            let hangar = { hangar with FreshSpawns = hangar.FreshSpawns.Add(planeType, newSpawns) }
            let hangars = this.Hangars.Add((user.UserId, coalition), hangar)
            { this with Hangars = hangars },
            []

        | PlayerReturnedBorrowedPlane(user, plane, cost, af) ->
            match this.GetAirfieldCoalition(af) with
            | Some coalition ->
                let qty =
                    this.Airfields.TryFind(af)
                    |> Option.defaultValue Map.empty
                    |> Map.tryFind plane
                    |> Option.defaultValue 0.0f
                let factor = getPriceFactor af plane qty this.Hangars
                let moneyBack = plane.Cost * factor |> min (this.Limits.MaxRentalGain + cost)
                let hangar = this.GetHangar(user, coalition)
                let hangar = { hangar with Reserve = hangar.Reserve + moneyBack }
                let hangars = this.Hangars.Add((user.UserId, coalition), hangar)
                { this with Hangars = hangars },
                [ Overview(user, 0, [ sprintf "You got %0.0f back for bringing back the %s you requisitioned" moneyBack plane.PlaneName ]) ]
            | None ->
                logger.Error("Attempt to return borrowed plane to neutral region")
                this, []

        | PlaneCheckIn(user, userCoalition, plane, health, isBorrowed, af) ->
            match this.GetAirfieldCoalition(af) with
            | Some coalition ->
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
                let hangar = this.GetHangar(user, coalition)
                let hangar =
                    if userCoalition = coalition &&
                        not isBorrowed &&
                        this.GetNumReservedPlanes(user, af, plane) <= float32 this.Limits.MaxReservedPlanes &&
                        this.GetTotalNumReservedPlanes(user, userCoalition) <= float32 this.Limits.MaxTotalReservedPlanes then
                        hangar.AddPlane(af, plane, health)
                    else
                        hangar
                let hangars = this.Hangars.Add((user.UserId, coalition), hangar)
                { this with Hangars = hangars; Airfields = airfields },
                [
                    yield Status(hangars, airfields)
                    if not isBorrowed && this.GetNumReservedPlanes(user, af, plane) > float32 this.Limits.MaxReservedPlanes then
                        yield Overview(user, 0, [sprintf "You have reached the limit on number of %s reserved at %s" plane.PlaneName af.AirfieldName])
                    if oldQty < 1.0f && newQty >= 1.0f then
                        yield PlanesAtAirfield(af, airfields.[af])
                        match this.State.GetRegion(this.World.GetAirfield(af).Region).Owner with
                        | Some coalition ->
                            yield Announce(coalition, [sprintf "%s available at %s again" plane.PlaneName af.AirfieldName])
                        | None ->
                            ()
                ]
            | None ->
                this, []

        | PlaneGifted(gift) ->
            let coalition =
                try
                    this.GetAirfieldCoalition(gift.Airfield)
                with
                | _ -> None
            match coalition with
            | Some coalition ->
                let hangarOut = this.TryGetHangar(gift.GiverGuid, coalition)
                let hangarIn =
                    gift.Recipient
                    |> Option.map (fun (guid, name) -> this.GetHangar( { UserId = guid; Name = name }, coalition))
                let newHangars, messages =
                    match hangarOut, hangarIn with
                    | None, _ ->
                        // Could not find giver
                        logger.Error(sprintf "Could not find giver with %s" gift.GiverGuid)
                        [], []
                    | Some (hangarOut : PlayerHangar), Some hangarIn ->
                        // Gift to an individual
                        let giverUserId = { UserId = string hangarOut.Player; Name = hangarOut.PlayerName }
                        let recipientUserId = { UserId = string hangarIn.Player; Name = hangarIn.PlayerName }
                        logger.Info(sprintf "Gift from %s to %s" hangarOut.PlayerName hangarIn.PlayerName)
                        [hangarOut.RemovePlane(gift.Airfield, gift.Plane, 1.0f, 0.0f<E>)
                         hangarIn.AddPlane(gift.Airfield, gift.Plane, 1.0f)],
                        [Overview (giverUserId, 0, [sprintf "You have gifted a %s to %s at %s" gift.Plane.PlaneName hangarIn.PlayerName gift.Airfield.AirfieldName])
                         Overview (recipientUserId, 0, [sprintf "You have been gifted a %s at %s by %s" gift.Plane.PlaneName gift.Airfield.AirfieldName hangarOut.PlayerName])]
                    | Some (hangarOut : PlayerHangar), None ->
                        // Gift to public
                        logger.Info(sprintf "Gift from %s to the public" hangarOut.PlayerName)
                        [hangarOut.RemovePlane(gift.Airfield, gift.Plane, 1.0f, 0.0f<E>)],
                        [Announce(coalition, [sprintf "%s has gifted a %s to the public at %s" hangarOut.PlayerName gift.Plane.PlaneName gift.Airfield.AirfieldName])]
                let hangars =
                    newHangars
                    |> List.fold (fun hangars h -> hangars |> Map.add ((string h.Player), coalition) h) this.Hangars
                let status = Status(hangars, this.Airfields)
                { this with Hangars = hangars },
                status :: messages
            | None ->
                logger.Error(sprintf "Failed gift attempt at neutral airfield %s" gift.Airfield.AirfieldName)
                this,
                []

        | DeliverSupplies(supplies, region) ->
            let oldNeeds =
                this.RegionNeeds.TryFind(region)
                |> Option.defaultValue 0.0f<E>
            let newNeeds = oldNeeds - supplies
            { this with RegionNeeds = this.RegionNeeds.Add(region, newNeeds) }, []

        | RewardPlayer(user, coalition, reward) ->
            let hangar = this.GetHangar(user, coalition)
            let reserve = min this.Limits.MaxCash (hangar.Reserve + reward)
            let hangar = { hangar with Reserve = reserve }
            let hangars = this.Hangars.Add((user.UserId, coalition), hangar)
            { this with Hangars = hangars },
            [
                if this.Limits.ShowCashReserves then
                    yield Overview(user, 15, [sprintf "You have been awarded %1.0f" reward])
                yield Status(hangars, this.Airfields)
            ]

        | InformPlayerHangar(user, coalition) ->
            let hangar = this.GetHangar(user, coalition)
            this,
            showHangar(hangar, 5)

        | PunishThief(user, plane, af) ->
            this,
            [
                Violation(user, "unauthorized take off")
            ]

        | Message m ->
            this, [m]

    member this.GetNumPlanesAt(af : AirfieldId, plane : PlaneModel) =
        this.Airfields.[af].TryFind plane |> Option.defaultValue 0.0f

    member this.GetPlanePrice(af : AirfieldId, plane : PlaneModel) =
        let qty = this.GetNumPlanesAt(af, plane)
        let factor = getPriceFactor af plane qty this.Hangars
        factor * plane.Cost

    member this.GetNumReservedPlanes(user : UserIds, af : AirfieldId, plane : PlaneModel) =
        match this.GetAirfieldCoalition(af) with
        | Some coalition ->
            let hangar = this.GetHangar(user, coalition)
            hangar.Airfields.TryFind(af)
            |> Option.bind (fun planes -> planes.Planes.TryFind plane)
            |> Option.defaultValue 0.0f
        | None ->
            0.0f

    member this.GetTotalNumReservedPlanes(user : UserIds, coalition : CoalitionId) : float32 =
        let hangar = this.GetHangar(user, coalition)
        hangar.Airfields
        |> Map.toSeq
        |> Seq.map snd
        |> Seq.collect (fun ah -> ah.Planes |> Map.toSeq)
        |> Seq.sumBy snd

    member this.GetClosestAirfield(v : Vector2) =
        this.State.State.Airfields
        |> Seq.minBy (fun afs -> let pos, _ = afs.Runway in (pos - v).Length())
        |> fun afs -> afs.AirfieldId

    member this.IsSpawnRestricted(af : AirfieldId, plane : PlaneModel, coalition : CoalitionId) =
        let isFree =
            let numOwned =
                lazy
                    this.State.State.Regions
                    |> Seq.filter (fun region -> region.Owner = Some coalition)
                    |> Seq.length
            (not this.Limits.SpawnsAreRestricted ||
             numOwned.Value * 4 <= this.State.State.Regions.Length ||
             this.RearAirfields.Contains(af) ||
             this.State.GetRegion(this.World.GetAirfield(af).Region).HasInvaders)
        not isFree

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

    member this.GetAirfieldCoalition(af : AirfieldId) =
        this.State.GetRegion(this.World.GetAirfield(af).Region).Owner

    member this.GetHangar(user : UserIds, coalition) =
        let freshSpawns = this.Limits.FreshSpawns
        this.Hangars.TryFind((user.UserId, coalition))
        |> Option.defaultValue (emptyHangar(user.UserId, user.Name, coalition, this.Limits.InitialCash, freshSpawns))

    member this.TryGetHangar(playerGuid : string, coalition : CoalitionId) =
        this.Hangars.TryFind((playerGuid, coalition))

    member this.GetFreshSpawnAlternatives(planeTypes : Set<PlaneType>, af : AirfieldId) =
        let planes = this.State.GetAirfield(af).NumPlanes
        planes
        |> Map.filter (fun plane qty -> qty >= 1.0f && planeTypes.Contains(plane.PlaneType))
        |> Map.toSeq
        |> Seq.map fst

    member this.GetRearValueFactor(plane : PlaneModel) =
        this.World.World.PlaneSet.Planes.TryFind(plane)
        |> Option.map (fun data -> data.RearValueFactor)
        |> Option.defaultValue 1.0f


type TransactionCost =
    | Rent of float32<E>
    | Buy of float32<E>
    | Denied of reason:string
    | FreshSpawn of PlaneType * float32
    | Free
with
    member this.IsRental =
        match this with
        | Rent _ -> true
        | _ -> false

    member this.IsDeniedCase =
        match this with
        | Denied _ -> true
        | _ -> false

    member this.Amount =
        match this with
        | Rent x | Buy x -> x
        | Denied _ | Free | FreshSpawn _ -> 0.0f<E>

/// States in the PlayerFlightData state machine
type PlayerFlightState =
    | Spawned of TransactionCost
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
                if context.IsSpawnRestricted(af, plane, coalition) then
                    if hangar.HasReservedPlane(af, plane) then
                        Free
                    else
                        let price = context.GetPlanePrice(af, plane)
                        if price = 0.0f<E> || price <= hangar.Reserve then
                            Rent price
                        else
                            Denied "Another pilot has reserved that plane; Bring one from the rear airfield to earn a reservation"
                elif context.RearAirfields.Contains(af) then
                    let numFreshSpawnsLeft = hangar.FreshSpawns.TryFind(plane.PlaneType) |> Option.defaultValue 0.0f
                    let rearValueFactor = context.GetRearValueFactor(plane)
                    match numFreshSpawnsLeft, plane.Cost * context.Limits.RearAirfieldCostFactor with
                    | x, _ when x >= rearValueFactor ->
                        FreshSpawn(plane.PlaneType, rearValueFactor)
                    | x, price ->
                        let available = context.GetHangar(user, coalition)
                        if available.Reserve >= price then
                            Buy price
                        else
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
            // Deny rental if it's not allowed in the limits
            let cost =
                if not context.Limits.PlaneRentalAllowed && cost.IsRental then
                    Denied "Another pilot has reserved that plane; Bring one from the rear airfield to earn a reservation"
                else
                    cost
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
                        yield PlaneCheckOut(user, plane, 1.0f, false, af)
                    | Rent cost ->
                        yield Message(Overview(user, 0, [sprintf "%s, it will cost you %0.0f to requisition a %s from %s" rank cost plane.PlaneName af.AirfieldName]))
                        yield PlaneCheckOut(user, plane, 1.0f, true, af)
                    | Buy cost ->
                        if cost > 0.0f<E> then
                            yield Message(Overview(user, 0, [sprintf "%s, it will cost you %0.0f to take a %s from %s" rank cost plane.PlaneName af.AirfieldName]))
                        yield PlaneCheckOut(user, plane, 1.0f, false, af)
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

    member this.IsBorrowed =
        this.CheckoutCost.IsRental

    // Handle first take off after spawn
    member this.HandleTakeOff(context : Context, takeOff : TakeOffEntry) =
        match this.State with
        | Spawned(Denied _) ->
            { this with State = StolePlane },
            [ PunishThief(this.Player, this.Plane, this.StartAirfield) ]
        | Spawned(cost) ->
            { this with State = InFlight },
            [
                if this.IsBorrowed then
                    yield PlayerPayed(this.Player, this.Coalition, cost.Amount)

                match cost with
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
        match af, this.Health with
        | Some af, health when health > 0.0f ->
            let suppliesTransfered =
                match bombs with
                | Some bombs when bombs = this.NumBombs -> this.InitialBombs
                | _ -> 0.0f<K>
            let supplyReward = context.SupplyFlightFactor(this.StartAirfield, af) * suppliesTransfered
            let isCorrectCoalition = context.GetAirfieldCoalition(af) = Some this.Coalition
            { this with State = MissionEnded },
            [
                assert(this.State <> MissionEnded)
                let healthUp = ceil(health * 10.0f) / 10.0f
                if this.IsBorrowed && isCorrectCoalition then
                    yield PlayerReturnedBorrowedPlane(this.Player, this.Plane, this.CheckoutCost.Amount, af)
                yield PlaneCheckIn(this.Player, this.Coalition, this.Plane, healthUp, this.IsBorrowed, af)
                yield DeliverSupplies(bombCost * (this.Cargo + suppliesTransfered), context.World.GetAirfield(af).Region)
                yield RewardPlayer(this.Player, this.Coalition, supplyReward * bombCost + this.Reward)
                yield InformPlayerHangar(this.Player, this.Coalition)
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
                yield PlaneCheckIn(this.Player, this.Coalition, this.Plane, 1.0f, this.IsBorrowed, this.StartAirfield)
        ]

    // Player ended mission before taking off, cancel mission
    member this.HandleAbortionBeforeTakeOff(context, doCheckIn) =
        assert(this.State <> MissionEnded)
        { this with State = MissionEnded },
        [
            if doCheckIn then
                yield PlaneCheckIn(this.Player, this.Coalition, this.Plane, this.Health, this.IsBorrowed, this.StartAirfield)
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
        | Spawned cost, (:? PlayerMissionEndEntry as finish) when finish.VehicleId = this.Vehicle ->
            this.HandleAbortionBeforeTakeOff(context, not cost.IsDeniedCase)
        | Spawned cost, (:? LeaveEntry as left) when string left.UserId = this.Player.UserId ->
            this.HandleAbortionBeforeTakeOff(context, not cost.IsDeniedCase)
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


/// Monitor events and check that players don't take off in planes they are not allowed to fly according to player hangars.
/// Also send information about player hangars.
let checkPlaneAvailability (missionLength : float32<H>) (limits : Limits) (world : World) (state : WorldState) (hangars : Map<string * CoalitionId, PlayerHangar>) (entries : AsyncSeq<LogEntry>) =

    asyncSeq {
        let mutable context = Context.Create(missionLength, world, state, hangars, limits)
        let mutable players : Map<int, PlayerFlightData> = Map.empty

        yield Status(context.Hangars , context.Airfields)

        for entry in entries do
            let mutable cmds0 = []
            // Handle special entries
            match entry with
            | :? JoinEntry as joined ->
                yield PlayerEntered(joined.UserId)
                for coalition in [Axis; Allies] do
                    match context.Hangars.TryFind((string joined.UserId, coalition)) with
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
            | :? ArtificialEntry as artificial ->
                match PlaneGift.TryFromString artificial.Data with
                | Some gift ->
                    cmds0 <- PlaneGifted gift :: cmds0
                | None ->
                    ()
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

            // Update context with kill entries
            let context2 =
                match entry with
                | :? KillEntry as kill -> context2.HandleKill(kill)
                | _ -> context2

            context <- context2

            // Yield messages generated while updating player data
            yield! AsyncSeq.ofSeq msgs
    }
