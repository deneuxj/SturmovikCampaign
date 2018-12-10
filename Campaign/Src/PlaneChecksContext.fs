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

module Campaign.PlaneChecksContext

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
        match s with
        | null ->
            logger.Error "Attempt to parse PlaneGift from null string"
            None
        | _ ->
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
                            logger.Error(sprintf "Failed to parse PlaneGift recipient'%s'" s)
                            Error()
                match recipient, PlaneModel.AllModels |> List.tryFind (fun plane -> plane.PlaneName = planeName) with
                | Ok(recipient), Some plane ->
                    logger.Debug(sprintf "Successfully parsed PlaneGift '%s'" s)
                    Some {
                        GiverGuid = m.Groups.[1].Value
                        Recipient = recipient
                        Plane = plane
                        Airfield = AirfieldId(m.Groups.[4].Value)
                    }
                | _, None | Error _, _->
                    None
            else
                logger.Error(sprintf "Failed to parse PlaneGift '%s'" s)
                None

/// Messages sent to the live commenter
type PlaneAvailabilityMessage =
    | Unmute
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
    | PlayerFreshSpawn of user:UserIds * CoalitionId * PlaneType * float32
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
      SpawnsAreRestricted : bool
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
          SpawnsAreRestricted = config.SpawnsAreRestricted
          FreshSpawns = freshSpawns
        }

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

        | PlayerFreshSpawn(user, coalition, planeType, factor) ->
            let hangar : PlayerHangar = this.GetHangar(user, coalition)
            let oldSpawns = hangar.FreshSpawns.TryFind(planeType) |> Option.defaultValue 0.0f
            let newSpawns = oldSpawns - factor |> max 0.0f
            let hangar = { hangar with FreshSpawns = hangar.FreshSpawns.Add(planeType, newSpawns) }
            let hangars = this.Hangars.Add((user.UserId, coalition), hangar)
            { this with Hangars = hangars },
            []

        | PlaneCheckIn(user, userCoalition, plane, health, isBorrowed, af) ->
            logger.Info (sprintf "Plane check in by %s of a %s at %s, health %3.0f" user.Name plane.PlaneName af.AirfieldName (health * 100.0f))
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
                    if userCoalition = coalition && not isBorrowed then
                        hangar.AddPlane(af, plane, health)
                    else
                        hangar
                logger.Debug (sprintf "Hangar after check in: %A" hangar)
                let hangars = this.Hangars.Add((user.UserId, coalition), hangar)
                { this with Hangars = hangars; Airfields = airfields },
                [
                    yield Status(hangars, airfields)
                    if oldQty < 1.0f && newQty >= 1.0f then
                        yield PlanesAtAirfield(af, airfields.[af])
                        match this.State.GetRegion(this.World.GetAirfield(af).Region).Owner with
                        | Some coalition ->
                            yield Announce(coalition, [sprintf "%s available at %s again" plane.PlaneName af.AirfieldName])
                        | None ->
                            ()
                ]
            | None ->
                logger.Warn (sprintf "%s checked in at neutral airfield" user.Name)
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
                        logger.Error(sprintf "Could not find giver with GUID %s" gift.GiverGuid)
                        [], []
                    | Some (hangarOut : PlayerHangar), Some hangarIn ->
                        let giverUserId = { UserId = string hangarOut.Player; Name = hangarOut.PlayerName }
                        let recipientUserId = { UserId = string hangarIn.Player; Name = hangarIn.PlayerName }
                        if hangarOut.Player = hangarIn.Player then
                            [], [ Overview(giverUserId, 0, ["You cannot gift a plane to yourself"]) ]
                        else
                            // Gift to an individual
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
