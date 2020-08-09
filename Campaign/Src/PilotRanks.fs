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
        let firstNames =
            CountryId.All
            |> Seq.map (fun country -> country, Set ["John"])
            |> Map.ofSeq
        let lastNames =
            CountryId.All
            |> Seq.map (fun country -> country, Set ["Doe"])
            |> Map.ofSeq
        {
            FirstNames = firstNames
            LastNames = lastNames
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

type AwardObjectType =
    | AirKills of SingleFlight: bool * int
    | Wounded of float32
    | Damaged of float32

type Award =
    {
        AwardName : string
        Description : string
        Target : AwardObjectType
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
