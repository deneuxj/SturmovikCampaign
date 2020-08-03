// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Campaign.WebController.Routes

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Writers
open FSharp.Json

open Campaign.WebController.Dto
open Campaign.Passwords

type IRoutingResponse =
    abstract GetWorld : unit -> Async<Result<World, string>>
    abstract GetWarState : int option -> Async<Result<WarState, string>>
    abstract GetSimulation : int -> Async<Result<SimulationStep[], string>>
    abstract GetDates : unit -> Async<Result<DateTime[], string>>
    abstract GetSyncState : unit -> Async<Result<string, string>>

type IControllerInteraction =
    abstract ResetCampaign : scenario:string -> Async<Result<string, string>>
    abstract Advance : unit -> Async<Result<string, string>>
    abstract Run : unit -> Async<Result<string, string>>
    abstract StartSyncLoop : unit -> Async<Result<string, string>>
    abstract StartSyncOnce : unit -> Async<Result<string, string>>
    abstract StopSyncAfterMission : unit -> Async<Result<string, string>>
    abstract InterruptSync : unit -> Async<Result<string, string>>

let setJsonMimeType = setMimeType "application/json; charset=utf-8"
let setTextMimeType = setMimeType "application/text; charset=utf-8"

let allowAnyOrigin = setHeader "Access-Control-Allow-Origin" "*"

let private usage = """
GET /query/world
GET /query/current
GET /query/past/<n>
GET /query/simulation/<n>
GET /query/dates

PUT /control/reset
PUT /control/advance
PUT /control/run
PUT /control/sync/loop
PUT /control/sync/once
PUT /control/sync/stop
PUT /control/sync/interrupt
"""

let fromBase64 = System.Convert.FromBase64String >> System.Text.Encoding.UTF8.GetString

let withBasicAuthentication checkUser realm apply (ctx : HttpContext) =
    let unauth msg =
        UNAUTHORIZED msg
        >=> setHeader "WWW-Authenticate" (sprintf "Basic Realm=\"%s\", charset=\"UTF-8\"" realm)
        >=> setTextMimeType

    match ctx.request.["Authorization"] with
    | None ->
        unauth "This resource requires authentication"
    | Some encoded when encoded.TrimStart().StartsWith("Basic") ->
        let base64 = encoded.Substring(5).Trim()
        let pair = fromBase64 base64
        match pair.Split(':', 2) with
        | [| user; password |] when checkUser(user, password)->
            apply ctx
        | _ ->
            unauth "Authentication failed"
    | _ ->
        unauth "Authentication failed"

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

let setPassword (passwords : PasswordsManager) (ctx : HttpContext) =
    let encoder = getEncoder ctx
    let json = ctx.request.rawForm |> encoder.GetString
    try
        let data = Json.deserialize<{| User: string; Password: string |}> json
        match passwords.SetPassword(data.User, data.Password) with
        | Ok ->
            OK (sprintf "Password set for %s" data.User) >=> setTextMimeType
        | Error err ->
            CONFLICT err >=> setTextMimeType
    with
    | _ -> BAD_REQUEST "Invalid content"

let mkRoutes (passwords : PasswordsManager, allowAdminPasswordChange : bool, rr : IRoutingResponse, ctrl : IControllerInteraction) =
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
    let inControlRoom = withBasicAuthentication passwords.Validate "Control room"
    choose [
        GET >=> choose [
            path "/query/world" >=> context (fun _ -> rr.GetWorld() |> serializeAsync)
            path "/query/current" >=> context (fun _ -> rr.GetWarState None |> serializeAsync)
            path "/query/dates" >=> context (fun _ -> rr.GetDates() |> serializeAsync)
            path "/query/sync/state" >=> context (fun _ -> rr.GetSyncState() |> serializeAsync)
            pathScan "/query/past/%d" (fun n -> rr.GetWarState(Some n) |> serializeAsync)
            pathScan "/query/simulation/%d" (fun n -> rr.GetSimulation(n) |> serializeAsync)
        ]
        POST >=> choose [
            path "/control/reset" >=> context (inControlRoom (fun _ -> ctrl.ResetCampaign("RheinlandSummer") |> serializeAsync))
            path "/control/advance" >=> context (inControlRoom (fun _ -> ctrl.Advance() |> serializeAsync))
            path "/control/run" >=> context (inControlRoom(fun _ -> ctrl.Run() |> serializeAsync))
            path "/control/sync/loop" >=> context (inControlRoom(fun _ -> ctrl.StartSyncLoop() |> serializeAsync))
            path "/control/sync/once" >=> context (inControlRoom(fun _ -> ctrl.StartSyncOnce() |> serializeAsync))
            path "/control/sync/stop" >=> context (inControlRoom(fun _ -> ctrl.StopSyncAfterMission() |> serializeAsync))
            path "/control/sync/interrupt" >=> context (inControlRoom(fun _ -> ctrl.InterruptSync() |> serializeAsync))
        ]
        POST >=> choose [
            if allowAdminPasswordChange then
                yield path "/admin/set-password" >=> context (setPassword passwords)
        ]
        GET >=> path "/help" >=> OK usage >=> setTextMimeType
        GET >=> pathStarts "/html/" >=> Files.browseHome
        GET >=> pathStarts "/js/" >=> Files.browseHome
        context (fun ctx ->
            "Invalid request. Try 'GET <url>/help' for a list of valid requests."
            |> NOT_FOUND) >=> setTextMimeType
    ]
    >=> allowAnyOrigin