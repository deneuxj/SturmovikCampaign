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
        {
            FirstNames = Map.empty
            LastNames = Map.empty
        }

    static member FromJsonFile(path : string) =
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

    member private this.AddNamesFromFile(getOldNames, setNewNames, country, path : string) =
        let names =
            seq {
                use file = File.OpenText(path)
                while not file.EndOfStream do
                    let name = file.ReadLine()
                    if not(System.String.IsNullOrWhiteSpace(name)) then
                        yield name.Trim()
            }
            |> Set.ofSeq
        let added =
            getOldNames(country)
            |> Option.defaultValue Set.empty
            |> Set.union names
        setNewNames(added, country)

    member this.AddFirstNamesFromFile(country, path) =
        this.AddNamesFromFile(
            (fun country -> this.FirstNames.TryFind country),
            (fun (added, country) -> { this with FirstNames = this.FirstNames.Add(country, added) }),
            country,
            path
        )

    member this.AddLastNamesFromFile(country, path) =
        this.AddNamesFromFile(
            (fun country -> this.LastNames.TryFind country),
            (fun (added, country) -> { this with LastNames = this.LastNames.Add(country, added) }),
            country,
            path
        )

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
