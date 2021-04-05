module Campaign.PilotRanks

open System.IO
open FSharp.Json
open Util

open Campaign.Common.BasicTypes

type NameDatabase =
    {
        [<Json.AsArrayJsonField(typeof<CountryId>, typeof<string[]>)>]
        FirstNames: Map<CountryId, string[]>
        [<Json.AsArrayJsonField(typeof<CountryId>, typeof<string[]>)>]
        LastNames: Map<CountryId, string[]>
        [<Json.AsArrayJsonField(typeof<CountryId>, typeof<string[]>)>]
        FemaleFirstNames: Map<CountryId, string[]> option
        [<Json.AsArrayJsonField(typeof<CountryId>, typeof<string[]>)>]
        FemaleLastNames: Map<CountryId, string[]> option
    }
with
    static member Default =
        {
            FirstNames = Map.empty
            LastNames = Map.empty
            FemaleFirstNames = Some Map.empty
            FemaleLastNames = Some Map.empty
        }

    member this.TryFindFirstNames(country, isFemale) =
        let m =
            if isFemale then
                this.FemaleFirstNames |> Option.defaultValue Map.empty
            else
                this.FirstNames
        m.TryFind(country)

    member this.TryFindLastNames(country, isFemale) =
        let m =
            if isFemale then
                this.FemaleLastNames |> Option.defaultValue Map.empty
            else
                this.LastNames
        m.TryFind(country)

    member private this.AddNamesFromFile(getOldNames, setNewNames, country, path : string) =
        let names =
            seq {
                use file = File.OpenText(path)
                while not file.EndOfStream do
                    let name = file.ReadLine()
                    if not(System.String.IsNullOrWhiteSpace(name)) then
                        yield name.Trim()
            }
            |> Seq.distinct
        let added =
            getOldNames(country)
            |> Option.defaultValue [||]
            |> Seq.append names
            |> Seq.distinct
            |> Array.ofSeq
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

    member this.AddFemaleFirstNamesFromFile(country, path) =
        this.AddNamesFromFile(
            (fun country -> this.FemaleFirstNames |> Option.bind (fun m -> m.TryFind country)),
            (fun (added, country) -> { this with FemaleFirstNames = (this.FemaleFirstNames |> Option.defaultValue Map.empty).Add(country, added) |> Some }),
            country,
            path
        )

    member this.AddFemaleLastNamesFromFile(country, path) =
        this.AddNamesFromFile(
            (fun country -> this.FemaleLastNames |> Option.bind(fun m -> m.TryFind country)),
            (fun (added, country) -> { this with FemaleLastNames = (this.FemaleLastNames |> Option.defaultValue Map.empty).Add(country, added) |> Some }),
            country,
            path
        )

type Rank =
    {
        RankName : string
        RankAbbrev : string
        Flights : int
    }
with
    static member FromFile(path) =
        let lines = System.IO.File.ReadAllLines(path)
        [
            for line in lines do
                let fields = line.Split(',')
                match List.ofArray fields with
                | rank :: abbrev :: flights :: _ ->
                    match System.Int32.TryParse (flights.Trim()) with
                    | true, n -> yield { RankName = rank.Trim(); RankAbbrev = abbrev.Trim(); Flights = n }
                    | false, _ -> ()
                | _ -> ()
        ]

type RanksDatabase =
    {
        [<Json.AsArrayJsonField(typeof<CountryId>, typeof<Rank list>)>]
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
        [<Json.AsArrayJsonField(typeof<CountryId>, typeof<Award list>)>]
        Awards : Map<CountryId, Award list>
    }
with
    static member Default =
        CountryId.All
        |> Seq.map (fun country -> country, [])
        |> Map.ofSeq
        |> fun awards -> { Awards = awards }
