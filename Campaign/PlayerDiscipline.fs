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

type JudgementDecision =
    | Kicked
    | Banned of hours:int

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
    static member Judge(damages: FriendlyDamage seq) =
        let threshold = 0.005f * PlaneModel.I16.Cost
        // Accumulate damages, resetting whenever no damage is done for 10s or more
        let accumulatedDamages =
            seq {
                let mutable acc = 0.0f<E>
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
        printfn "Someone is up to %d friendly damage excesses" numExcesses
        if numExcesses >= 2 then
            Some(Banned 48)
        else
            None

/// Watch game event logs for friendly fire, and emit bans when abuse is detected
let disciplinePlayers (world : World) (events : AsyncSeq<LogEntry>) =
    let nameOf = ref Map.empty
    let coalitionOf = ref Map.empty
    let damagesOf = ref Map.empty
    let objects = ref Map.empty
    let missionStart = ref (DateTime())
    asyncSeq {
        for event in events do
            match event with
            // Reset state
            | :? MissionStartEntry as start ->
                nameOf := Map.empty
                coalitionOf := Map.empty
                damagesOf := Map.empty
                objects := Map.empty
                missionStart := start.MissionTime
            // Map object id to object type and to country
            | :? ObjectSpawnedEntry as spawned ->
                objects := Map.add spawned.ObjectId spawned.ObjectType objects.Value
                coalitionOf := Map.add spawned.ObjectId spawned.Country coalitionOf.Value
                damagesOf := Map.add spawned.ObjectId (ResizeArray<FriendlyDamage>()) damagesOf.Value
            // Map object id to player ids and to country
            | :? PlayerPlaneEntry as entry ->
                let data =
                    { NickId = string entry.NickId
                      UserId = string entry.UserId
                      Name = entry.Name }
                nameOf := Map.add entry.VehicleId data nameOf.Value
                coalitionOf := Map.add entry.VehicleId entry.Country coalitionOf.Value
            // Register friendly damage
            | :? DamageEntry as damage ->
                match nameOf.Value.TryFind damage.AttackerId with
                | Some player ->
                    match coalitionOf.Value.TryFind(damage.AttackerId), coalitionOf.Value.TryFind(damage.TargetId) with
                    | Some countryA, Some countryB when countryA = countryB ->
                        let cost =
                            match objects.Value.TryFind(damage.TargetId) with
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
                            | None ->
                                0.0f<E>
                        let cost = cost * damage.Damage
                        let entry =
                            { Time = missionStart.Value + damage.Timestamp
                              Amount = cost }
                        let record =
                            damagesOf.Value.[damage.AttackerId]
                        record.Add(entry)
                        match FriendlyDamage.Judge record with
                        | Some penalty ->
                            yield {
                                Player = player
                                Decision = penalty
                            }
                        | None ->
                            ()
                    | _ ->
                        // Coalition of attacker or target not known
                        ()
                | None ->
                    // Attacker is not a player
                    ()
            | _ ->
                // Other kind of event
                ()
    }

