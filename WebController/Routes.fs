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

open Util.StringPatterns

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Writers
open FSharp.Json

open Campaign.WebController.Dto
open Campaign.Passwords

type HealthFilter =
    | OnlyHealthy
    | NoDead

type PilotSearchFilter =
    {
        Health : HealthFilter option
        Country : int option
        Coalition : int option
        NamePattern : string option
    }

type IRoutingResponse =
    abstract GetWorld : unit -> Async<Result<World, string>>
    abstract GetWarState : int option -> Async<Result<WarState, string>>
    abstract GetSimulation : int -> Async<Result<SimulationStep[], string>>
    abstract GetDates : unit -> Async<Result<DateTime[], string>>
    abstract GetSyncState : unit -> Async<Result<string, string>>
    abstract GetPilots : PilotSearchFilter -> Async<Result<Pilot list, string>>
    abstract GetPilot : int -> Async<Result<Pilot * MissionRecord list, string>>

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

POST /control/reset
POST /control/advance
POST /control/run
POST /control/sync/loop
POST /control/sync/once
POST /control/sync/stop
POST /control/sync/interrupt
"""

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

let setPassword (passwords : PasswordsManager) =
    handleJson<{| User: string; Password: string |}>
        (fun data ->
            match passwords.SetPassword(data.User, data.Password) with
            | Ok ->
                OK (sprintf "Password set for %s" data.User) >=> setTextMimeType
            | Error err ->
                CONFLICT err >=> setTextMimeType
        )

let resetCampaign reset =
    handleJson<{| Scenario: string |}>
        (fun data -> reset data.Scenario)

/// Extract pilot search filter from URL args and run search
let searchPilots handler (ctx : HttpContext) =
    let healthFilter =
        match ctx.request.queryParam "health" with
        | Choice1Of2 "OnlyHealthy" -> Some OnlyHealthy
        | Choice1Of2 "NoDead" -> Some NoDead
        | _ -> None
    let country =
        match ctx.request.queryParam "country" with
        | Choice1Of2(AsInt32 n) -> Some n
        | _ -> None
    let coalition =
        match ctx.request.queryParam "coalition" with
        | Choice1Of2(AsInt32 n) -> Some n
        | _ -> None
    let namePattern =
        match ctx.request.queryParam "name" with
        | Choice1Of2(pattern) -> Some pattern
        | _ -> None
    let filter =
        {
            Health = healthFilter
            Country = country
            Coalition = coalition
            NamePattern = namePattern
        }
    handler filter

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
    let inControlRoom = Suave.Authentication.authenticateBasic (function ("admin", password) -> passwords.Validate("admin", password) | _ -> false)
    choose [
        GET >=> choose [
            path "/query/world" >=> context (fun _ -> rr.GetWorld() |> serializeAsync)
            path "/query/current" >=> context (fun _ -> rr.GetWarState None |> serializeAsync)
            path "/query/dates" >=> context (fun _ -> rr.GetDates() |> serializeAsync)
            path "/query/sync/state" >=> context (fun _ -> rr.GetSyncState() |> serializeAsync)
            path "/query/pilots" >=> context (fun ctx -> searchPilots rr.GetPilots ctx |> serializeAsync)
            pathScan "/query/past/%d" (fun n -> rr.GetWarState(Some n) |> serializeAsync)
            pathScan "/query/simulation/%d" (fun n -> rr.GetSimulation(n) |> serializeAsync)
            pathScan "/query/pilot/%d" (fun n -> rr.GetPilot(n) |> serializeAsync)
        ]
        POST >=> choose [
            path "/control/reset" >=>
                inControlRoom
                    (context
                        (resetCampaign
                            (fun scenario ->
                                ctrl.ResetCampaign(scenario)
                                |> serializeAsync)))
            path "/control/advance" >=> inControlRoom (context (fun _ -> ctrl.Advance() |> serializeAsync))
            path "/control/run" >=> inControlRoom (context (fun _ -> ctrl.Run() |> serializeAsync))
            path "/control/sync/loop" >=> inControlRoom(context( fun _ -> ctrl.StartSyncLoop() |> serializeAsync))
            path "/control/sync/once" >=> inControlRoom(context(fun _ -> ctrl.StartSyncOnce() |> serializeAsync))
            path "/control/sync/stop" >=> inControlRoom(context(fun _ -> ctrl.StopSyncAfterMission() |> serializeAsync))
            path "/control/sync/interrupt" >=> inControlRoom(context(fun _ -> ctrl.InterruptSync() |> serializeAsync))
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