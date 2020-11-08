module Campaign.Pilots

open System
open Util

open Campaign.Common.Targets
open Campaign.Common.BasicTypes

open PilotRanks

type BanData =
    {
        BannedSince : DateTime
        [<Json.TimeSpanJsonField>]
        Duration : TimeSpan
    }
type ProbationData =
    {
        ProbationSince: DateTime
        [<Json.TimeSpanJsonField>]
        Duration: TimeSpan
        [<Json.TimeSpanJsonField>]
        Penality: TimeSpan
    }

[<RequireQualifiedAccess>]
type BanStatus =
    | Clear
    | Banned of BanData
    | Probation of ProbationData
with
    override this.ToString() =
        match this with
        | Clear -> "No ban"
        | Banned ban -> sprintf "Banned until %s" ((ban.BannedSince + ban.Duration).ToShortDateString())
        | Probation x -> sprintf "On probation until %s (penalty of %d days)" ((x.ProbationSince + x.Duration).ToShortDateString()) (int(ceil x.Penality.TotalDays))

type Player =
    {
        Guid : string
        Name : string
        OtherNames : string list
        BanStatus : BanStatus
    }

type PilotHealth =
    | Healthy
    | Dead
    | Injured of Until: DateTime // game time
with
    override this.ToString() =
        match this with
        | Healthy -> "healthy"
        | Dead -> "dead"
        | Injured until -> sprintf "injured until %s" (until.ToShortDateString())

let computeInstancesOfAward (airKills0 : int, award : Award, flights : FlightRecord list) =
    let rec forEachFlight airKills flights =
        seq {
            match flights with
            | [] -> ()
            | (flight : FlightRecord) :: flights ->
                match award.Target with
                | AirKills(singleFlight, threshold) ->
                    if singleFlight && flight.AirKills > threshold then
                        yield award.AwardName
                    if not singleFlight && flight.AirKills + airKills > threshold then
                        yield award.AwardName
                | Wounded threshold ->
                    if flight.PilotHealth <= threshold then
                        yield award.AwardName
                | Damaged threshold ->
                    if flight.PlaneHealth <= threshold then
                        yield award.AwardName
                yield! forEachFlight (flight.AirKills + airKills) flights
        }

    forEachFlight 0 flights

[<Struct>]
type PilotId = PilotId of Guid
with
    member this.Guid =
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
        InitialAwards : string list
        InitialAirKills : int
        InitialNumFlights : int
        Flights : FlightRecord list
    }
with
    member this.FullName =
        sprintf "%s %s" this.PilotFirstName this.PilotLastName

let countCompletedFlights (flights : FlightRecord list) =
    (0, flights)
    ||> List.fold (fun flights flight ->
        if flight.TargetsDamaged |> List.isEmpty |> not then
            flights + 1
        elif flight.Length >= TimeSpan(0, 20, 0) then // 20 minutes
            flights + 1
        else
            flights
    )

let tryComputeRank (db : RanksDatabase) (pilot : Pilot) =
    let completedFlights = countCompletedFlights pilot.Flights
    db.Ranks.TryFind(pilot.Country)
    |> Option.bind (fun ranks ->
        ranks
        |> Seq.tryFindBack (fun rank -> rank.Flights <= completedFlights + pilot.InitialNumFlights)
    )