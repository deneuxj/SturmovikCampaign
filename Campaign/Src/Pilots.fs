module Campaign.Pilots

open System
open WorldDescription
open Targets
open BasicTypes
open PilotRanks

[<RequireQualifiedAccess>]
type BanStatus =
    | Clear
    | Banned of Since: DateTime * Duration: TimeSpan
    | Probation of {| Since: DateTime; Duration: TimeSpan; Penality: TimeSpan |}
with
    override this.ToString() =
        match this with
        | Clear -> "No ban"
        | Banned(since, duration) -> sprintf "Banned until %s" ((since + duration).ToShortDateString())
        | Probation x -> sprintf "On probation until %s (penalty of %d days)" ((x.Since + x.Duration).ToShortDateString()) (int(ceil x.Penality.TotalDays))

type Player =
    {
        Guid : string
        Name : string
        OtherNames : Set<string>
        BanStatus : BanStatus
    }

type PilotHealth =
    | Healthy
    | Injured of Until: DateTime // game time
with
    override this.ToString() =
        match this with
        | Healthy -> "healthy"
        | Injured until -> sprintf "injured until %s" (until.ToShortDateString())

let computeInstancesOfAward (award : Award) (flights : FlightRecord list) =
    let rec forEachFlight damage flights =
        seq {
            match flights with
            | [] -> ()
            | (flight : FlightRecord) :: flights ->
                let damage =
                    if award.SingleFlight then
                        0.0f
                    else
                        damage
                yield! forEachDamage flights damage flight.TargetsDamaged
        }

    and forEachDamage flights damage targets =
        seq {
            match targets with
            | [] ->
                yield! forEachFlight damage flights
            | (target, _, dmg) :: targets ->
                let isMatch =
                    match award.Target, target.Kind with
                    | (Bridge | Building), (Bridge | Building)
                    | ParkedPlane, ParkedPlane
                    | Air, Air -> true
                    | x, y -> x = y
                if isMatch then
                    if damage < award.MinDamage && damage + dmg >= award.MinDamage then
                        yield award.AwardName
                        if not award.Unique then
                            yield! forEachDamage flights (damage + dmg) targets
                    else
                        yield! forEachDamage flights (damage + dmg) targets
                else
                    yield! forEachDamage flights (damage + dmg) targets
        }

    forEachFlight 0.0f flights

let computeInstancesOfAwards (awards : Award list) (flights : FlightRecord list) =
    awards
    |> Seq.collect (fun award -> computeInstancesOfAward award flights)

[<Struct>]
type PilotId = PilotId of int
with
    member this.AsInt =
        match this with
        | PilotId x -> x

type Pilot =
    {
        Id : PilotId
        PlayerGuid : string
        PilotFirstName : string
        PilotLastName : string
        Health : PilotHealth
        Country : CountryId
        Flights : FlightRecord list
    }

let countCompletedFlights (flights : FlightRecord list) =
    (0, flights)
    ||> List.fold (fun flights flight ->
        match flight.Return with
        | CrashedInFriendlyTerritory _ when flight.TargetsDamaged |> List.isEmpty |> not ->
            flights + 1
        | AtAirfield _ ->
            if flight.TargetsDamaged |> List.isEmpty |> not then
                flights + 1
            elif flight.Length >= TimeSpan(0, 20, 0) then // 20 minutes
                flights + 1
            else
                flights
        | _ ->
            flights
    )

let tryComputeRank (db : RanksDatabase) (pilot : Pilot) =
    let completedFlights = countCompletedFlights pilot.Flights
    db.Ranks.TryFind(pilot.Country)
    |> Option.bind (fun ranks ->
        ranks
        |> Seq.tryFindBack (fun rank -> rank.Flights <= completedFlights)
    )