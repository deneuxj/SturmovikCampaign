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
        Country : string option
        Coalition : string option
        NamePattern : string option
    }

type IRoutingResponse =
    abstract GetScenarioNames : unit -> Async<Result<string list, string>>
    abstract GetWorld : unit -> Async<Result<World, string>>
    abstract GetWarState : int option -> Async<Result<WarState, string>>
    abstract GetSimulation : int -> Async<Result<SimulationStep[], string>>
    abstract GetDates : unit -> Async<Result<DateTime[], string>>
    abstract GetSyncState : unit -> Async<Result<string, string>>
    abstract GetPilots : PilotSearchFilter -> Async<Result<Pilot list, string>>
    abstract GetPilot : string -> Async<Result<{| Pilot: Pilot; Missions: MissionRecord list |}, string>>
    abstract GetPlayerPilots : string -> Async<Result<Pilot list, string>>
    abstract FindPlayersByName : string -> Async<Result<Player list, string>>
    abstract AllPlayers : unit -> Async<Result<Player list, string>>
    abstract GetPlayer : string -> Async<Result<Player option, string>>
    abstract GetOnlinePlayers : unit -> Async<Result<{| Players : string list |}, string>>
    abstract GetRegionSupplies : int * string -> Async<Result<float, string>>
    abstract GetRegionCapacity : int * string -> Async<Result<float, string>>
    abstract GetAirfieldCapacity : int * string -> Async<Result<float, string>>
    abstract GetLandTransportCapacity : int * string * string -> Async<Result<float, string>>

type IControllerInteraction =
    abstract ResetCampaign : scenario:string -> Async<Result<string, string>>
    abstract RebuildWorld : unit -> Async<Result<string, string>>
    abstract Advance : int -> Async<Result<string, string>>
    abstract BackTo : int -> Async<Result<string, string>>
    abstract Forward : unit -> Async<Result<string, string>>
    abstract StartSyncLoop : unit -> Async<Result<string, string>>
    abstract StartSyncOnce : unit -> Async<Result<string, string>>
    abstract StopSyncAfterMission : unit -> Async<Result<string, string>>
    abstract InterruptSync : unit -> Async<Result<string, string>>
    abstract ResolveError : unit -> Async<Result<string, string>>

let setJsonMimeType = setMimeType "application/json; charset=utf-8"
let setTextMimeType = setMimeType "application/text; charset=utf-8"

let allowAnyOrigin = setHeader "Access-Control-Allow-Origin" "*"

let private usage = """
GET /query/scenarios
GET /query/sync/state
GET /query/world
GET /query/state/<n>/summary
GET /query/state/<n>/regions/<region>/supplies
GET /query/state/<n>/regions/<region>/capacity
GET /query/state/<n>/airfields/<airfield>/capacity
GET /query/state/<n>/transport/<region>/<region>/land
GET /query/state/<n>/transport/<region>/<region>/air (TODO)
GET /query/state/<n>/transport/<region>/<region>/sea (TODO)
GET /query/simulation/<n>
GET /query/dates
GET /query/pilots?country=<country>&coalition=<coalition>&health=<Healthy or NoDead>&name=<substring>
GET /query/pilot/<guid>
GET /query/players?name=<name>
GET /query/players/<guid>
GET /query/players/<guid>/pilots
GET /query/online

POST /control/reset with json { Scenario = <name> }
POST /control/rebuild
POST /control/advance with json { NumSteps = <n> }
POST /control/backto with json { Index = <n> }
POST /control/forward
POST /control/sync/loop
POST /control/sync/once
POST /control/sync/stop
POST /control/sync/interrupt
POST /control/resolve
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
        | Choice1Of2(s) -> Some s
        | _ -> None
    let coalition =
        match ctx.request.queryParam "coalition" with
        | Choice1Of2(s) -> Some s
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

let advance advance =
    handleJson<{| NumSteps: int option |}>(fun data -> advance(data.NumSteps |> Option.defaultValue 1))

let backTo func =
    handleJson<{| Index: int |}>(fun data -> func(data.Index))

let mkRoutes (passwords : PasswordsManager, rr : IRoutingResponse, ctrl : IControllerInteraction) =
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
            path "/query/scenarios" >=> context (fun _ -> rr.GetScenarioNames() |> serializeAsync)
            path "/query/sync/state" >=> context (fun _ -> rr.GetSyncState() |> serializeAsync)
            path "/query/world" >=> context (fun _ -> rr.GetWorld() |> serializeAsync)
            path "/query/current" >=> context (fun _ -> rr.GetWarState None |> serializeAsync)
            path "/query/dates" >=> context (fun _ -> rr.GetDates() |> serializeAsync)
            path "/query/pilots" >=> context (fun ctx -> searchPilots rr.GetPilots ctx |> serializeAsync)
            pathScan "/query/state/%d/summary" (fun n -> rr.GetWarState(Some n) |> serializeAsync)
            pathScan "/query/state/%d/regions/%s/capacity" (fun (idx, region) -> rr.GetRegionCapacity(idx, region) |> serializeAsync)
            pathScan "/query/state/%d/regions/%s/supplies" (fun (idx, region) -> rr.GetRegionSupplies(idx, region) |> serializeAsync)
            pathScan "/query/state/%d/airfields/%s/capacity" (fun (idx, airfield) -> rr.GetAirfieldCapacity(idx, airfield) |> serializeAsync)
            pathScan "/query/state/%d/transport/%s/%s/land" (fun (idx, regionA, regionB) -> rr.GetLandTransportCapacity(idx, regionA, regionB) |> serializeAsync)
            pathScan "/query/simulation/%d" (fun n -> rr.GetSimulation(n) |> serializeAsync)
            pathScan "/query/pilot/%s" (fun n -> rr.GetPilot(n) |> serializeAsync)
            pathScan "/query/players/%s/pilots" (rr.GetPlayerPilots >> serializeAsync)
            pathScan "/query/players/%s" (rr.GetPlayer >> serializeAsync)
            path "/query/players" >=>
                (context
                    (fun ctx ->
                        match ctx.request.queryParam("name") with
                        | Choice1Of2 name ->
                            rr.FindPlayersByName(name)
                            |> serializeAsync
                        | Choice2Of2 _ ->
                            rr.AllPlayers()
                            |> serializeAsync
                    )
                )
            path "/query/online" >=> context (fun _ -> rr.GetOnlinePlayers() |> serializeAsync)
        ]
        POST >=> choose [
            path "/control/reset" >=>
                inControlRoom
                    (context
                        (resetCampaign
                            (fun scenario ->
                                ctrl.ResetCampaign(scenario)
                                |> serializeAsync)))
            path "/control/rebuild" >=> inControlRoom (context (fun _ -> ctrl.RebuildWorld() |> serializeAsync))
            path "/control/advance" >=> inControlRoom (context (advance (fun numSteps -> ctrl.Advance(numSteps) |> serializeAsync)))
            path "/control/backto" >=> inControlRoom (context (backTo (fun idx -> ctrl.BackTo(idx) |> serializeAsync)))
            path "/control/forward" >=> inControlRoom (context (fun _ -> ctrl.Forward() |> serializeAsync))
            path "/control/resolve" >=> inControlRoom (context (fun _ -> ctrl.ResolveError() |> serializeAsync))
            path "/control/sync/loop" >=> inControlRoom(context( fun _ -> ctrl.StartSyncLoop() |> serializeAsync))
            path "/control/sync/once" >=> inControlRoom(context(fun _ -> ctrl.StartSyncOnce() |> serializeAsync))
            path "/control/sync/stop" >=> inControlRoom(context(fun _ -> ctrl.StopSyncAfterMission() |> serializeAsync))
            path "/control/sync/interrupt" >=> inControlRoom(context(fun _ -> ctrl.InterruptSync() |> serializeAsync))
        ]
        GET >=> path "/help" >=> OK usage >=> setTextMimeType
        GET >=> pathStarts "/doc/" >=> Files.browse (System.IO.Path.Combine(System.Environment.CurrentDirectory))
        GET >=> pathStarts "/html/" >=> Files.browseHome
        GET >=> pathStarts "/js/" >=> Files.browseHome
        GET >=> pathStarts "/img/" >=> Files.browseHome
        GET >=> path "/favicon.ico" >=> Files.browse (System.IO.Path.Combine(System.Environment.CurrentDirectory, "favicon_package"))
        GET >=> path "/" >=> Redirection.found "/html/map.html"
        context (fun ctx ->
            "Invalid request. Try 'GET <url>/help' for a list of valid requests."
            |> NOT_FOUND) >=> setTextMimeType
    ]
    >=> allowAnyOrigin