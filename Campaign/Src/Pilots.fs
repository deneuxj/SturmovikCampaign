module Campaign.Pilots

open System
open WorldDescription
open Targets
open BasicTypes

[<RequireQualifiedAccess>]
type BanStatus =
    | Clear
    | Banned of Since: DateTime * Duration: TimeSpan
    | Probation of {| Since: DateTime; Duration: TimeSpan; Penality: TimeSpan |}

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

type NameDatabase =
    {
        FirstNames: Map<CountryId, Set<string>>
        LastNames: Map<CountryId, Set<string>>
    }

type Rank =
    {
        RankName : string
        Flights : int
    }

type RanksDatabase =
    {
        Ranks: Map<CountryId, Rank list>
    }
with
    static member Default =
        CountryId.All
        |> Seq.map (fun country -> country, [ {RankName = "Rookie"; Flights = 0 } ])
        |> Map.ofSeq
        |> fun ranks -> { Ranks = ranks }

type Award =
    {
        AwardName : string
        Description : string
        Target : TargetType
        MinDamage : float32
        SingleFlight : bool
        Unique : bool
    }

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

type AwardDatabase =
    {
        Awards : Map<CountryId, Award list>
    }
with
    static member Default =
        CountryId.All
        |> Seq.map (fun country -> country, [])
        |> Map.ofSeq
        |> fun awards -> { Awards = awards }

type Pilot =
    {
        PlayerGuid : string
        PilotFistName : string
        PilotLastName : string
        Country : CountryId
        Index : int
        Home : AirfieldId option
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