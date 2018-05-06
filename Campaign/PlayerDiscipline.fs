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

module Campaign.PlayerDiscipline

open System
open Campaign.BasicTypes
open Campaign.Configuration
open Util
open ploggy
open System.Numerics
open FSharp.Control
open Campaign.ResultExtraction
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.PlayerHangar
open Campaign.NewWorldState
open MBrace.FsPickler
open NLog

let private logger = LogManager.GetCurrentClassLogger()

type JudgementDecision =
    | Kicked
    | Banned of hours:int
    | Informed of string

type UserIds =
    { UserId : string
      Name : string }

type Judgement =
    { Player : UserIds
      Decision : JudgementDecision
    }

type FriendlyDamage =
    { Time : DateTime
      Amount : float32<E>
    }
with
    static member Judge(config : Configuration, damages : FriendlyDamage seq) =
        let threshold = 0.005f * PlaneModel.I16.Cost
        // Accumulate damages, resetting whenever no damage is done for 10s or more
        let accumulatedDamages =
            seq {
                let mutable acc = 0.0f<E>
                yield acc
                match Seq.tryHead damages with
                | Some curr ->
                    acc <- curr.Amount
                    yield acc
                | None ->
                    ()
                for (prec, curr) in Seq.pairwise damages do
                    let span = curr.Time - prec.Time
                    if span > TimeSpan(0, 0, 10) then
                        acc <- 0.0f<E>
                        yield acc
                        acc <- curr.Amount
                        yield acc
                    else
                        acc <- acc + curr.Amount
                        yield acc
            }
        let numExcesses =
            accumulatedDamages
            |> Seq.pairwise
            |> Seq.sumBy (fun (d0, d1) -> if d0 < threshold && d1 >= threshold then 1 else 0)
        logger.Debug(sprintf "Someone is up to %d friendly damage excesses" numExcesses)
        if numExcesses >= config.MaxFriendlyFireEvents then
            Some(Banned config.FriendlyFireBanDuration)
        else
            None

/// Watch game event logs for friendly fire, and emit bans when abuse is detected
let disciplinePlayers (config : Configuration) (world : World) (events : AsyncSeq<LogEntry>) =
    asyncSeq {
        let (|PlaneObjectType|_|) = planeObjectType world.PlaneSet
        let mutable nameOf = Map.empty // Vehicle ID -> player ID
        let mutable coalitionOf = Map.empty // Vehicle ID -> coalition
        let mutable damagesOf = Map.empty // Vehicle ID -> friendly fire
        let mutable objects = Map.empty // Vehicle ID -> object type
        let mutable missionStart = (DateTime())
        let mutable tookOfAt = Map.empty // Vehicle ID -> take-off time
        let mutable takenDamageAt = Map.empty // Vehicle ID -> time when took damage from other object
        let mutable noobScore = Map.empty // player ID -> "noobishness" score (wrecked own plane without causes)

        // Expand "noob score" of a player who's being clumsy by wrecking their own ship or inflicting friendly damage
        let addNoobScore player score =
            asyncSeq {
                let old =
                    noobScore.TryFind player
                    |> Option.defaultValue 0.0f
                let newScore = old + score
                noobScore <- Map.add player newScore noobScore
                // Send notification when the player passes an int boundary (e.g. 1.5 -> 2.1)
                if floor newScore > floor old then
                    yield {
                        Player = player
                        Decision = Informed (sprintf "%3.1f wrecking penalty" newScore)
                    }
                if newScore > config.MaxNoobScore then
                    yield {
                        Player = player
                        Decision = Informed "wrecking limit exceeded"
                    }
                    yield {
                        Player = player
                        Decision = Banned config.NoobBanDuration
                    }
            }

        for event in events do
            match event with
            // Reset state
            | :? MissionStartEntry as start ->
                nameOf <- Map.empty
                coalitionOf <- Map.empty
                damagesOf <- Map.empty
                objects <- Map.empty
                missionStart <- start.MissionTime
                tookOfAt <- Map.empty
                takenDamageAt <- Map.empty
                noobScore <- Map.empty
            // Map object id to object type and to country
            | :? ObjectSpawnedEntry as spawned ->
                objects <- Map.add spawned.ObjectId spawned.ObjectType objects
                coalitionOf <- Map.add spawned.ObjectId spawned.Country coalitionOf
                damagesOf <- Map.add spawned.ObjectId (ResizeArray<FriendlyDamage>()) damagesOf
            // Map object id to player ids and to country
            | :? PlayerPlaneEntry as entry ->
                let data =
                    { UserId = string entry.UserId
                      Name = entry.Name }
                nameOf <- Map.add entry.VehicleId data nameOf
                coalitionOf <- Map.add entry.VehicleId entry.Country coalitionOf
            // Record time of take-off. Self-inflicted damage before take-off grants extra high noob points
            | :? TakeOffEntry as takeoff ->
                tookOfAt <- Map.add takeoff.VehicleId takeoff.Timestamp tookOfAt
            // Register friendly damage
            | :? DamageEntry as damage ->
                match nameOf.TryFind damage.AttackerId with
                | Some player ->
                    match coalitionOf.TryFind(damage.AttackerId), coalitionOf.TryFind(damage.TargetId) with
                    | Some countryA, Some countryB when countryA = countryB ->
                        let cost =
                            match objects.TryFind(damage.TargetId) with
                            | Some(StaticPlaneType world.PlaneSet plane) ->
                                plane.Cost
                            | Some(StaticVehicleType tank) ->
                                tank.Cost
                            | Some(BuildingObjectType model) ->
                                let model = { Script = ""; Model = model; Pos = Unchecked.defaultof<OrientedPosition> }
                                model.Storage(world.SubBlockSpecs) + model.Production(world.SubBlockSpecs, 1.0f) * 24.0f<H>
                            | Some(PlaneObjectType model) ->
                                model.Cost
                            | _ ->
                                // Some other kind of object. Arbitrarily pick half the cost of an i16
                                0.5f * PlaneModel.I16.Cost
                        let cost = cost * damage.Damage
                        let entry =
                            { Time = missionStart + damage.Timestamp
                              Amount = cost }
                        let record =
                            damagesOf.[damage.AttackerId]
                        record.Add(entry)
                        match FriendlyDamage.Judge(config, record) with
                        | Some penalty ->
                            yield {
                                Player = player
                                Decision = Informed "Friendly fire limit exceeded"
                            }
                            yield {
                                Player = player
                                Decision = penalty
                            }
                        | None ->
                            ()
                        if cost > 0.0f<E> then
                            yield! addNoobScore player damage.Damage
                    | _ ->
                        // Coalition of attacker or target not known
                        ()
                | None ->
                    // Attacker is not a player
                    ()
                // Record that object has taken damage.
                if damage.AttackerId <> -1 then
                    takenDamageAt <- Map.add damage.TargetId damage.Timestamp takenDamageAt
                else
                    match objects.TryFind damage.TargetId,  nameOf.TryFind damage.TargetId with
                    | Some(PlaneObjectType _), Some player ->
                        let factor =
                            // Damage is self-inflicted
                            if takenDamageAt.TryFind(damage.TargetId).IsNone then
                                // No damage was ever inflicted by someone else, looks like a player wrecked their own plane without external cause
                                match tookOfAt.TryFind(damage.TargetId) with
                                | None ->
                                    // Did not even take off, apply extra penalty
                                    2.0f
                                | Some t ->
                                    // 2.0 right after take-off, linear decrease to 0 after 15min
                                    let delta = float32 (damage.Timestamp - t).TotalMinutes
                                    2.0f - delta / 7.5f
                                    |> min 2.0f
                                    |> max 0.0f
                            else
                                0.0f
                        logger.Debug(sprintf "Wreck penalty: id = %d, factor = %f, damage = %f" damage.TargetId factor damage.Damage)
                        let extraNoobScore = factor * damage.Damage
                        if extraNoobScore > 0.0f then
                            yield! addNoobScore player extraNoobScore
                    | Some _, _ // Player not flying a plane. Other vehicles such as tanks apparently don't report damage taken from AIs, and we don't inflict wreck damage for them.
                    | None, _ 
                    | _, None -> // Non player-controlled entity (e.g. AI) took damage from unknown source.
                        ()
            | _ ->
                // Other kind of event
                ()
    }


type PlaneAvailabilityMessage =
    | Overview of UserIds * delay:int * string list
    | Warning of UserIds * delay:int * string list
    | Announce of CoalitionId * string list
    | Violation of UserIds
    | Status of Map<string, PlayerHangar> * Map<AirfieldId, AirfieldState>
    | PlanesAtAirfield of AirfieldId * Map<PlaneModel, float32>


let checkPlaneAvailability (world : World) (state : WorldState) (hangars : Map<string, PlayerHangar>) (damages : AsyncSeq<Damage>) (events : AsyncSeq<LogEntry>) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let rearAirfields =
        [Axis; Allies]
        |> Seq.map (fun coalition -> tryFindRearAirfield world coalition state)
        |> Set.ofSeq
    let (|PlaneObjectType|_|) = planeObjectType world.PlaneSet
    let emptyHangar (playerId : string, playerName : string) = { Player = Guid(playerId); PlayerName = playerName; Reserve = 0.0f<E>; Airfields = Map.empty }
    asyncSeq {
        let mutable playerOf = Map.empty // Vehicle ID -> UserIds
        let mutable planes = Map.empty // Vehicle ID -> plane model
        let mutable airfields =
            state.Airfields
            |> Seq.map (fun afs -> afs.AirfieldId, afs)
            |> Map.ofSeq
        let mutable hangars = hangars // User ID -> player hangar
        let mutable rogues = Set.empty // set of User ID, players that spawned in a plane not available to them
        let mutable healthOf = Map.empty // Vehicle ID -> health
        let mutable landedAt = Map.empty // Vehicle ID -> (Airfield, actually landed : bool)
        let mutable coalitionOf = Map.empty // Player name -> Coalition
        let mutable rewards = Map.empty // Map player name to rewards, gathered during current flight, to be collected upon landing
        let mutable tookOffFrom = Map.empty // Vehicle ID -> TookOff
        let mutable regionNeeds = AutoOrder.computeSupplyNeeds world state

        let computeSupplyRewardFactor regStart regEnd =
            // Only give a positive reward if the transfer contributed to improve the overall picture
            if regionNeeds.[regEnd] > regionNeeds.[regStart] then
                // Reward long flights better than short flights
                let distance = wg.GetRegion(regStart).Position - wg.GetRegion(regEnd).Position
                let distance = distance.Length()
                distance / 70000.0f
                |> min 1.0f
                |> max 0.0f
            else
                0.0f

        let checkoutPlane(af : Airfield, plane, availableAtAirfield, user, vehicle) =
            asyncSeq {
                let afs = airfields.[af.AirfieldId]
                let afs = { afs with NumPlanes = Map.add plane (availableAtAirfield - 1.0f) afs.NumPlanes }
                match sg.GetRegion(wg.GetAirfield(af.AirfieldId).Region).Owner with
                | Some coalition ->
                    yield Announce(coalition,
                        [sprintf "%s at %s is authorized to take off in a %s" user.Name af.AirfieldId.AirfieldName plane.PlaneName])
                | None ->
                    logger.Warn(sprintf "Player spawned in a %s at neutral airfield %s" plane.PlaneName af.AirfieldId.AirfieldName)
                if availableAtAirfield < 2.0f then
                    yield PlanesAtAirfield(af.AirfieldId, afs.NumPlanes)
                airfields <- Map.add af.AirfieldId afs airfields
                rogues <- Set.remove user.UserId rogues
                landedAt <- Map.add vehicle (af, false) landedAt
                planes <- Map.add vehicle plane planes
            }

        let handledSupplyMissionStart(user : UserIds, regStart : RegionId, coalition : CoalitionId) =
            asyncSeq {
                let bestDestinations =
                    state.Regions
                    |> Seq.filter (fun region -> region.Owner = Some coalition)
                    |> Seq.sortByDescending (fun regState -> computeSupplyRewardFactor regStart regState.RegionId)
                    |> Seq.filter(fun regState -> computeSupplyRewardFactor regStart regState.RegionId > 0.0f)
                    |> Seq.truncate 3
                    |> List.ofSeq
                match bestDestinations with
                | [] ->
                    yield Overview(user, 15, ["This airfield is not a good place to start a supply mission from"])
                | _ :: _ as x ->
                    yield Overview(user, 15, ["The following regions would benefit from resupplies: " + (x |> List.map (fun r -> string r.RegionId) |> String.concat ", ")])
            }

        let startFlight(entry : PlayerPlaneEntry) =
            asyncSeq {
                let user = { UserId = string entry.UserId; Name = entry.Name }
                playerOf <- Map.add entry.VehicleId user playerOf
                healthOf <- Map.add entry.VehicleId 1.0f healthOf
                match CoalitionId.FromLogEntry(entry.Country) with
                | Some x ->
                    coalitionOf <- coalitionOf.Add(user.Name, x)
                | None ->
                    ()
                let pos = Vector2(entry.Position.X, entry.Position.Z)
                let af = world.GetClosestAirfield(pos)
                let hangar = hangars.TryFind user.UserId |> Option.defaultValue (emptyHangar(user.UserId, user.Name))
                let hangar = { hangar with PlayerName = user.Name }
                match entry.VehicleType with
                | PlaneObjectType plane ->
                    // If spawning in a Ju52 or other cargo transporter, suggest destinations
                    let bigBombLoad = lazy(plane.BombLoads |> Seq.exists (fun (loadout, weight) -> loadout = entry.Payload && weight >= 1000.0f<K>))
                    if plane.Roles.Contains(CargoTransporter) || bigBombLoad.Value then
                        match CoalitionId.FromLogEntry(entry.Country) with
                        | Some coalition -> yield! handledSupplyMissionStart(user, af.Region, coalition)
                        | None -> ()
                    let availableAtAirfield =
                        airfields.TryFind af.AirfieldId
                        |> Option.map (fun afs -> afs.NumPlanes)
                        |> Option.bind (fun planes -> planes.TryFind plane)
                        |> Option.defaultValue 0.0f
                    let isRestricted =
                        if rearAirfields.Contains(Some af.AirfieldId) then
                            false
                        elif sg.GetRegion(af.Region).HasInvaders then
                            false
                        else
                            match plane.PlaneType with
                            | Fighter -> availableAtAirfield < 10.0f
                            | Attacker -> availableAtAirfield < 7.0f
                            | Bomber -> availableAtAirfield < 5.0f
                            | Transport -> false

                    let descr =
                        match hangar.ShowAvailablePlanes(af.AirfieldId) with
                        | [] -> [sprintf "You do not have any reserved planes at %s" af.AirfieldId.AirfieldName]
                        | planes -> (sprintf "You have the following planes reserved at %s:" af.AirfieldId.AirfieldName) :: planes
                    yield Overview(user, 15, descr)

                    if availableAtAirfield < 1.0f then
                        yield Warning(user, 15,
                            [
                                sprintf "There are no %s available at %s" plane.PlaneName af.AirfieldId.AirfieldName
                                "CANCEL your flight, or you will be KICKED"
                            ])
                        rogues <- Set.add user.UserId rogues
                    else if isRestricted then
                        // Player hangar restrictions apply
                        match hangar.TryRemovePlane(af.AirfieldId, plane, 1.0f) with
                        | None ->
                            yield Warning(user, 15,
                                [
                                    sprintf "The staff at %s won't let you take a %s" af.AirfieldId.AirfieldName plane.PlaneName
                                    "It has been assigned to another pilot"
                                    "CANCEL your flight or you will be KICKED"
                                ] @ hangar.ShowAvailablePlanes(af.AirfieldId))
                            rogues <- Set.add user.UserId rogues
                        | Some hangar ->
                            yield! checkoutPlane(af, plane, availableAtAirfield, user, entry.VehicleId)
                            hangars <- Map.add user.UserId hangar hangars
                    else
                        yield! checkoutPlane(af, plane, availableAtAirfield, user, entry.VehicleId)
                | _ -> // Vehicle other than plane
                    ()
            }

        let endFlight(user, plane, af : Airfield, vehicle, collectRewards) =
            asyncSeq {
                let hangar = hangars.TryFind user.UserId |> Option.defaultValue (emptyHangar(user.UserId, user.Name))
                let hangar = { hangar with PlayerName = user.Name }
                let health = healthOf.TryFind vehicle |> Option.defaultValue 1.0f
                let airfield = airfields.[af.AirfieldId]
                let oldNumPlanes = airfield.NumPlanes.TryFind plane |> Option.defaultValue 0.0f
                let airfield = { airfield with NumPlanes = airfield.NumPlanes.Add(plane, oldNumPlanes + health) }
                let hangar = hangar.AddPlane(af.AirfieldId, plane, health)
                let coalition = coalitionOf.TryFind(user.Name)
                let collectedReward =
                    if collectRewards && sg.GetRegion(af.Region).Owner = coalition then
                        rewards.TryFind(user.Name) |> Option.defaultValue 0.0f<E> |> max 0.0f<E>
                    else
                        0.0f<E>
                if collectRewards then
                    match coalition with
                    | Some coalition ->
                        yield Announce(coalition, [ sprintf "%s has collected a reward of %0.0f" user.Name collectedReward ])
                    | None ->
                        ()
                let hangar = { hangar with Reserve = hangar.Reserve + collectedReward }
                if oldNumPlanes < 1.0f && oldNumPlanes + health > 1.0f then
                    yield PlanesAtAirfield(af.AirfieldId, airfield.NumPlanes)
                airfields <- airfields.Add(af.AirfieldId, airfield)
                hangars <- hangars.Add(user.UserId, hangar)
                landedAt <- landedAt.Remove(vehicle)
                rogues <- rogues.Remove(user.UserId)
                playerOf <- playerOf.Remove(vehicle)
                healthOf <- healthOf.Remove(vehicle)
                planes <- planes.Remove(vehicle)
                coalitionOf <- coalitionOf.Remove(user.Name)
                rewards <- rewards.Remove(user.Name)
            }

        let showHangar(userId : string, delay) =
            asyncSeq {
                match hangars.TryFind(userId) with
                | Some hangar ->
                    let userIds = { UserId = string hangar.Player; Name = hangar.PlayerName }
                    yield Overview(userIds, 60,
                        [
                            sprintf "Welcome back %s" userIds.Name
                            sprintf "You cash reserve is %0.0f" hangar.Reserve
                        ])
                    yield Overview(userIds, delay,
                        [
                            for kvp in hangar.Airfields do
                                let planes = match hangar.ShowAvailablePlanes(kvp.Key) with
                                                | [] -> "None"
                                                | planes -> String.concat ", " planes
                                yield sprintf "Your reserved planes at %s: %s" kvp.Key.AirfieldName planes
                        ])
                | None ->
                    let userIds : UserIds = { UserId = userId; Name = "" }
                    yield Overview(userIds, delay,
                        [
                            "Welcome new player"
                            "Please choose an airfield with its name in UPPER CASE for your first spawn"
                            "Spawning at another airfield requires you to land an undamaged plane there first"
                            "Only then can you take off from that airfield, with that exact plane model"
                        ])
            }

        let handleLogEvent(event : LogEntry) =
            asyncSeq {
                match event with
                | :? JoinEntry as entry ->
                    yield! showHangar(string entry.UserId, 60)

                | :? PlayerPlaneEntry as entry ->
                    yield! startFlight(entry)
                    yield Status(hangars, airfields)

                | :? TakeOffEntry as entry ->
                    match playerOf.TryFind entry.VehicleId with
                    | Some user when rogues.Contains user.UserId ->
                        yield Violation(user)
                        landedAt <- Map.remove entry.VehicleId landedAt
                        rogues <- Set.remove user.UserId rogues
                        playerOf <- Map.remove entry.VehicleId playerOf
                    | _ ->
                        ()

                | :? DamageEntry as entry ->
                    let oldHealth = healthOf.TryFind entry.TargetId |> Option.defaultValue 1.0f
                    let health = oldHealth - entry.Damage
                    healthOf <- Map.add entry.TargetId health healthOf

                | :? KillEntry as entry ->
                    healthOf <- Map.add entry.TargetId 0.0f healthOf

                | :? LandingEntry as entry ->
                    let pos = Vector2(entry.Position.X, entry.Position.Z)
                    let af = world.GetClosestAirfield(pos)
                    landedAt <- Map.add entry.VehicleId (af, true) landedAt

                | :? PlayerMissionEndEntry as entry ->
                    match playerOf.TryFind(entry.VehicleId), planes.TryFind(entry.VehicleId), landedAt.TryFind(entry.VehicleId) with
                    | Some user, Some plane, Some (af, reallyLanded) ->
                        yield! endFlight(user, plane, af, entry.VehicleId, reallyLanded)
                        yield! showHangar(user.UserId, 5)
                    | _ ->
                        ()
                    yield Status(hangars, airfields)

                | :? LeaveEntry as entry ->
                    let vehicleAndUser =
                        playerOf
                        |> Map.toSeq
                        |> Seq.tryFind (fun (_, user) -> user.UserId = string entry.UserId)
                    match vehicleAndUser with
                    | Some(vehicle, user) ->
                        match planes.TryFind(vehicle), landedAt.TryFind(vehicle) with
                        | Some plane, Some (af, _) ->
                            yield! endFlight(user, plane, af, vehicle, false)
                        | _ ->
                            ()
                    | None ->
                        ()
                    yield Status(hangars, airfields)

                | _ -> ()
            }

        let handleDamage(damage : Damage) =
            match damage.Data.ByPlayer with
            | None -> ()
            | Some player ->
                let reward = rewards.TryFind(player) |> Option.defaultValue 0.0f<E>
                let factor =
                    match coalitionOf.TryFind(player) with
                    | Some coalition ->
                        if Some coalition = damage.Object.Coalition(wg, sg) then
                            -1.0f
                        else
                            1.0f
                    | None ->
                        0.0f
                rewards <- rewards.Add(player, reward + factor * damage.Value(wg, sg))

        let handleSupplyMission(landed : Landed) =
            asyncSeq {
                match tookOffFrom.TryFind(landed.PlaneId), playerOf.TryFind(landed.PlaneId) with
                | _, None
                | None, _ ->
                    ()
                | Some (tko : TookOff), Some userIds ->
                    let reward = landed.Cargo * bombCost
                    let regStart = wg.GetAirfield(tko.Airfield).Region
                    let regEnd = wg.GetAirfield(landed.Airfield).Region
                    let factor =
                        let coalitionStart = tko.Coalition
                        let coalitionEnd = landed.Coalition
                        if coalitionStart = coalitionEnd then
                            computeSupplyRewardFactor regStart regEnd
                        else
                            // Delivering goods to the enemy: negative reward!
                            -1.0f
                    let hangar =
                        hangars.TryFind(userIds.UserId)
                        |> Option.defaultValue (emptyHangar(userIds.UserId, userIds.Name))
                    let reward = factor * reward
                    let hangar = { hangar with Reserve = hangar.Reserve + reward }
                    hangars <- hangars.Add(userIds.UserId, hangar)
                    match landed.Coalition with
                    | Some coalition ->
                        yield Announce(coalition,
                            [ sprintf "%s has received a reward of %0.0f for their supply mission" userIds.Name reward ])
                    | None ->
                        ()
                    // Update region needs
                    regionNeeds <- regionNeeds
                                        .Add(regStart, regionNeeds.[regStart] + landed.Cargo * bombCost)
                                        .Add(regEnd, regionNeeds.[regEnd] - landed.Cargo * bombCost)
                    yield Status(hangars, airfields)
            }

        let takeOffsAndLandings = extractTakeOffsAndLandings world state events
        for choice in AsyncSeq.mergeChoice3 events damages takeOffsAndLandings do
            match choice with
            | Choice1Of3 event ->
                yield! handleLogEvent(event)
            | Choice2Of3 damage ->
                handleDamage(damage)
            | Choice3Of3(TookOff tko) ->
                tookOffFrom <- tookOffFrom.Add(tko.PlaneId, tko)
            | Choice3Of3(Landed landed) ->
                yield! handleSupplyMission landed
    }