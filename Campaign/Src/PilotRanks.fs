module Campaign.PilotRanks

open BasicTypes
open Targets

type NameDatabase =
    {
        FirstNames: Map<CountryId, Set<string>>
        LastNames: Map<CountryId, Set<string>>
    }
with
    static member Default =
        let firstNames country = Map [country, Set ["John"]]
        let lastNames country = Map [country, Set ["Doe"]]
        CountryId.All
        |> Seq.map (fun country -> country, { FirstNames = firstNames country; LastNames = lastNames country })

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
