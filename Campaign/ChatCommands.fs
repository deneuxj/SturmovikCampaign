module Campaign.ChatCommands

open Campaign.WatchLogs
open Campaign.WorldDescription
open Campaign.PlaneModel
open Campaign.PlayerHangar

open SturmovikMission.DataProvider.Parsing
open Campaign.WorldState

type ChatCommands =
    | ShowReservedPlanesAtAirfield of AirfieldId
    | ShowAirfieldsWithPlane of PlaneModel
    | ShowCashReserve
    | GiftCashReserve of Beneficiary:string * Amount : int
with
    static member Parse(world : World, raw : string) =
        match Stream.FromString raw with
        | ReLit "sp" (ReString (w, EOF _)) ->
            let fs = Fastenshtein.Levenshtein(w.Trim().ToLower())
            let af =
                world.Airfields
                |> Seq.minBy (fun af -> fs.Distance(af.AirfieldId.AirfieldName.ToLower()))
            Result.Ok (ShowReservedPlanesAtAirfield af.AirfieldId)

        | ReLit "sp" _ ->
            Result.Error "Usage: <sp airfield"

        | ReLit "sa" (ReString (w, EOF _)) ->
            let fs = Fastenshtein.Levenshtein(w.Trim().ToLower())
            let plane =
                world.PlaneSet.AllModels
                |> Seq.minBy (fun plane -> fs.Distance(plane.PlaneName.ToLower()))
            Result.Ok (ShowAirfieldsWithPlane plane)

        | ReLit "sp" _ ->
            Result.Error "Usage: <sa plane"

        | ReLit "cash" _ ->
            Result.Ok ShowCashReserve

        | ReLit "give" (ReString (w, ReInt (n, EOF _)))
        | ReLit "give" (ReInt (n, ReString (w, EOF _)))
        | ReLit "gift" (ReString (w, ReInt (n, EOF _)))
        | ReLit "gift" (ReInt (n, ReString (w, EOF _))) ->
            Result.Ok (GiftCashReserve(w.Trim(), n))

        | ReLit "give" _ ->
            Result.Error "Usage: <give player amount"

        | ReLit "gift" _ ->
            Result.Error "Usage: <gift player amount"

        | ReString(x, _) ->
            Result.Error (sprintf "Unknown command %s" x)

        | _ ->
            Result.Error "Improperly formatted command. Usage: <command arguments, where command can be sp, sa, cash or give"

type ChatCommand =
    { Player : string
      Command : ChatCommands }
with
    static member Parse(world, rawCmd : Command) =
        match ChatCommands.Parse(world, rawCmd.Command) with
        | Result.Error err -> Result.Error err
        | Result.Ok cmd -> Result.Ok { Player = rawCmd.Author; Command = cmd }

    member this.Interpret(hangars : Map<string, PlayerHangar>, airfields : Map<AirfieldId, AirfieldState>) =
        []