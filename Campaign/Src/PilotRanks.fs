module Campaign.PilotRanks

open System.IO
open FSharp.Json

open Campaign.Common.BasicTypes

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

    static member FromFile(path : string) =
        use file = File.OpenText(path)
        let names = Json.deserialize<{| Countries : {| Country : string; FirstNames : string list; LastNames : string list |} list |}>(file.ReadToEnd())
        let mergeMap =
            Seq.groupBy fst
            >> Seq.map (fun (country, nameSets) ->
                country,
                nameSets
                |> Seq.map snd
                |> Set.unionMany
            )
            >> Map.ofSeq
        let firstNames =
            names.Countries
            |> Seq.choose (fun country ->
                match CountryId.FromString country.Country with
                | None -> None
                | Some countryId -> Some(countryId, country.FirstNames |> Set.ofList)
            )
            |> mergeMap
        let lastNames =
            names.Countries
            |> Seq.choose (fun country ->
                match CountryId.FromString country.Country with
                | None -> None
                | Some countryId -> Some(countryId, country.LastNames |> Set.ofList)
            )
            |> mergeMap
        {
            FirstNames = firstNames
            LastNames = lastNames
        }

type Rank =
    {
        RankName : string
        RankAbbrev : string
        Flights : int
    }

type RanksDatabase =
    {
        Ranks: Map<CountryId, Rank list>
    }
with
    static member Default =
        CountryId.All
        |> Seq.map (fun country -> country, [ { RankName = "Flight Officer"; RankAbbrev = "FO"; Flights = 0 } ])
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
