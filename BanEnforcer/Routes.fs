module Coconutside.BanEnforcer.Routes
open System
open FSharp.Control
open FSharp.Json
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Writers
open NLog

open Config
open Players

let setJsonMimeType = setMimeType "application/json; charset=utf-8"
let setTextMimeType = setMimeType "application/text; charset=utf-8"

let allowAnyOrigin = setHeader "Access-Control-Allow-Origin" "*"

let private usage = """
GET /bans?name=<name>
 => { Results = { Guid: string; Until: DateTime }[] }

GET /bans/<guid>
 => { Value = { Until: DateTime }? }

POST /bans/new (requires AUTH)
 with { Guid: string; Days : int; Hours : int }

DELETE /bans/<guid> (requires AUTH)
"""

let toDateTime (t : DateTime) =
    {|
        Year = t.Year
        Month = t.Month
        Day = t.Day
        Hour = t.Hour
        Minute = t.Minute
        Second = t.Second
    |}

let fromBase64 = System.Convert.FromBase64String >> System.Text.Encoding.UTF8.GetString

let getEncoder (ctx : HttpContext) =
    match ctx.request.header "Content-Type" with
    | Choice1Of2 value ->
        let m = System.Text.RegularExpressions.Regex.Match(value, @"application=.*; charset=(.*)")
        if m.Success then
            try
                System.Text.Encoding.GetEncoding(m.Groups.[1].Value)
            with _ ->
                System.Text.Encoding.UTF8
        else
            System.Text.Encoding.UTF8
    | _ ->
        System.Text.Encoding.UTF8

let handleJson<'T> handler (ctx : HttpContext) : WebPart =
    let encoder = getEncoder ctx
    let json = ctx.request.rawForm |> encoder.GetString
    let data =
        try
            Json.deserialize<'T> json
            |> Ok
        with
        | _ -> Error(BAD_REQUEST "Invalid json payload")
    match data with
    | Ok data ->
        handler data
    | Error wb ->
        wb

let mkRoutes (config : Config, monitor : Monitor) =
    let inline serializeAsync task (ctx : HttpContext) =
        async {
            let! x = task
            let webpart =
                match x with
                | Ok x ->
                    let json =
                        try
                            Json.serializeEx { JsonConfig.Default with allowUntyped = true } x
                        with
                        | :? JsonSerializationError ->
                            Json.serializeEx { JsonConfig.Default with allowUntyped = true } {| Value = x |}
                    OK json >=> setJsonMimeType
                | Error s ->
                    CONFLICT s >=> setTextMimeType
            return! webpart ctx
        }
    let inControlRoom = Suave.Authentication.authenticateBasic (function ("admin", password) -> config.CheckPassword(password) | _ -> false)
    
    let tryGetPlayer guid =
        async {
            let! hit = monitor.TryGetPlayer(HashedGuid guid)
            return Ok hit
        }

    let findPlayers name =
        async {
            match name with
            | Choice1Of2 name ->
                let! hits = monitor.Find(name)
                let hits =
                    hits
                    |> List.map (fun player ->
                        {|
                            Guid = player.HashedGuid.String
                            Until = toDateTime player.BannedUntil
                        |}
                    )
                return Ok hits
            | _ ->
                return Error "Missing 'name' query parameter"
        }

    let postBan (data : {| Guid : string; Days : int option; Hours : int option |}) =
        async {
            monitor.Add(data.Guid, TimeSpan(defaultArg data.Days 0, defaultArg data.Hours 0, 0, 0))
            return Ok()
        }

    let clearBan guid =
        async {
            monitor.Clear(guid)
            return Ok()
        }

    choose [
        GET >=> choose [
            pathScan "/bans/%s" (tryGetPlayer >> serializeAsync)
            path "/bans" >=> context (fun ctx -> let name = ctx.request.queryParam("name") in findPlayers name |> serializeAsync)
        ]
        POST >=> choose [
            path "/bans/new" >=>
                (inControlRoom
                    (context (handleJson (postBan >> serializeAsync))
                )
            )
        ]
        DELETE >=> choose [
            pathScan "/bans/%s" (fun guid ->
                inControlRoom
                    (context (fun _ -> clearBan guid |> serializeAsync))
                )
        ]
        GET >=> path "/help" >=> OK usage >=> setTextMimeType
        GET >=> pathStarts "/html/" >=> Files.browseHome
        GET >=> pathStarts "/js/" >=> Files.browseHome
        context (fun _ ->
            "Invalid request. Try 'GET <url>/help' for a list of valid requests."
            |> NOT_FOUND) >=> setTextMimeType
    ]
    >=> allowAnyOrigin