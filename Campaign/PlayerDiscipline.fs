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
open MBrace.FsPickler
open NLog

let private logger = LogManager.GetCurrentClassLogger()

type JudgementDecision =
    | Kicked
    | Banned of hours:int
    | Informed of string

type UserIds =
    { NickId : string
      UserId : string
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
                    { NickId = string entry.NickId
                      UserId = string entry.UserId
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

