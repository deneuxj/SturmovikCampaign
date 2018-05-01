module Campaign.ChatCommands

open Campaign.WatchLogs
open Campaign.WorldDescription
open Campaign.PlaneModel
open Campaign.PlayerHangar

open SturmovikMission.DataProvider.Parsing
open Campaign.WorldState
open SturmovikMission.Blocks.Util.String
open System.Numerics
open BasicTypes

let closestMatch f target xs =
    let fs = Fastenshtein.Levenshtein(target)
    xs
    |> Seq.minBy (fun x ->
        let x : string = f x
        let val1 =
            if x.StartsWith(target) then 0
            elif x.Contains(target) then 1
            else 2
        val1, fs.Distance(x))

type ChatCommands =
    | ShowReservedPlanesAtAirfield of AirfieldId
    | ShowAirfieldsWithPlane of PlaneModel
    | ShowCashReserve
    | GiftCashReserve of Beneficiary:string * Amount : int
    | Invalid of string
with
    static member Parse(world : World, raw : string) =
        match Stream.FromString raw with
        | ReLit "sp" (ReId (w, EOF _)) ->
            let w = w.Trim().ToLower()
            let fs = Fastenshtein.Levenshtein(w)
            let af =
                world.Airfields
                |> closestMatch (fun af -> af.AirfieldId.AirfieldName.ToLower()) w
            Result.Ok (ShowReservedPlanesAtAirfield af.AirfieldId)

        | ReLit "sp" _ ->
            Result.Error "Usage: <sp airfield"

        | ReLit "sa" (ReId (w, EOF _)) ->
            let w = w.Trim().ToLower()
            let fs = Fastenshtein.Levenshtein(w)
            let plane =
                world.PlaneSet.AllModels
                |> closestMatch (fun plane -> plane.PlaneName.ToLower()) w
            Result.Ok (ShowAirfieldsWithPlane plane)

        | ReLit "sp" _ ->
            Result.Error "Usage: <sa plane"

        | ReLit "cash" _ ->
            Result.Ok ShowCashReserve

        | ReLit "give" (ReString (w, ReInt (n, EOF _)))
        | ReLit "give" (ReInt (n, ReString (w, EOF _)))
        | ReLit "gift" (ReString (w, ReInt (n, EOF _)))
        | ReLit "gift" (ReInt (n, ReString (w, EOF _))) ->
            Result.Error "Not implemented"
            //Result.Ok (GiftCashReserve(w.Trim(), n))

        | ReLit "give" _ ->
            Result.Error "Not implemented"
            //Result.Error "Usage: <give \"player\" amount"

        | ReLit "gift" _ ->
            Result.Error "Not implemented"
            //Result.Error "Usage: <gift \"player\" amount"

        | ReString(x, _) ->
            Result.Error (sprintf "Unknown command %s" x)

        | _ ->
            Result.Error "Improperly formatted command. Usage: <command arguments, where command can be sp, sa, cash or give"

let tryGetHangarByPlayerName playerName hangars =
    hangars
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.tryFind (fun (hangar : PlayerHangar) -> hangar.PlayerName = playerName)

type ChatCommand =
    { Player : string
      Command : ChatCommands }
with
    static member Parse(world, rawCmd : Command) =
        match ChatCommands.Parse(world, rawCmd.Command) with
        | Result.Error err -> { Player = rawCmd.Author; Command = Invalid err }
        | Result.Ok cmd -> { Player = rawCmd.Author; Command = cmd }

    member this.Interpret(hangars : Map<string, PlayerHangar>, airfields : Map<AirfieldId, AirfieldState>) =
        match this.Command with
        | ShowReservedPlanesAtAirfield afId ->
            let planes =
                tryGetHangarByPlayerName this.Player hangars
                |> Option.map (fun h ->
                    h.Airfields.TryFind(afId)
                    |> Option.map (fun af -> af.Planes)
                    |> Option.defaultValue Map.empty)
                |> Option.defaultValue Map.empty
                |> Map.toSeq
                |> Seq.filter (fun (_, qty) -> qty >= 1.0f)
                |> Seq.map fst
                |> Seq.map (fun plane -> plane.PlaneName)
                |> Seq.sort
                |> String.concat ", "
            [
                yield "Planes reserved at " + afId.AirfieldName
                yield match planes with
                        | "" -> "None"
                        | x-> x
            ]
        | ShowAirfieldsWithPlane plane ->
            let airfields =
                tryGetHangarByPlayerName this.Player hangars
                |> Option.map (fun h -> h.Airfields)
                |> Option.defaultValue Map.empty
                |> Map.filter (fun afId planes ->
                    Map.exists (fun plane2 qty -> plane2 = plane && qty >= 1.0f) planes.Planes)
                |> Map.toSeq
                |> Seq.map fst
                |> Seq.map (fun id -> id.AirfieldName)
                |> String.concat ", "
            [
                yield "Airfields with reserved " + plane.PlaneName
                yield match airfields with
                        | "" -> "None"
                        | x -> x
            ]
        | ShowCashReserve ->
            let reserve =
                tryGetHangarByPlayerName this.Player hangars
                |> Option.map (fun h -> h.Reserve)
                |> Option.defaultValue 0.0f<E>
            [ sprintf "Your cash reserve is %0.0f" reserve ]
        | GiftCashReserve _ ->
            [ "Gifts not implemented yet" ]
        | Invalid err->
            [ err ]