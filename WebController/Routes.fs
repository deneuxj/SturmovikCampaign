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

type IRoutingResponse =
    abstract GetWorld : unit -> Async<Result<World, string>>
    abstract GetWarState : int option -> Async<Result<WarState, string>>
    abstract GetSimulation : int -> Async<Result<SimulationStep[], string>>
    abstract GetDates : unit -> Async<Result<DateTime[], string>>

type IControllerInteraction =
    abstract ResetCampaign : scenario:string -> Async<Result<string, string>>
    abstract Advance : unit -> Async<Result<SimulationStep[], string>>
    abstract Run : unit -> Async<Result<string, string>>

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
"""

let mkRoutes (rr : IRoutingResponse, ctrl : IControllerInteraction) =
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
    choose [
        GET >=> choose [
            path "/query/world" >=> context (fun _ -> rr.GetWorld() |> serializeAsync)
            path "/query/current" >=> context (fun _ -> rr.GetWarState None |> serializeAsync)
            path "/query/dates" >=> context (fun _ -> rr.GetDates() |> serializeAsync)
            pathScan "/query/past/%d" (fun n -> rr.GetWarState(Some n) |> serializeAsync)
            pathScan "/query/simulation/%d" (fun n -> rr.GetSimulation(n) |> serializeAsync)
        ]
        PUT >=> choose [
            path "/control/reset" >=> context (fun _ -> ctrl.ResetCampaign("RheinlandSummer") |> serializeAsync)
            path "/control/advance" >=> context (fun _ -> ctrl.Advance() |> serializeAsync)
            path "/control/run" >=> context (fun _ -> ctrl.Run() |> serializeAsync)
        ]
        GET >=> path "/help" >=> OK usage >=> setTextMimeType
        context (fun ctx ->
            "Invalid request. Try 'GET <url>/help' for a list of valid requests."
            |> NOT_FOUND) >=> setTextMimeType
    ]
    >=> allowAnyOrigin