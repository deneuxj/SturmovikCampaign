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

open Campaign.WebController.Dto

type IRoutingResponse =
    abstract GetWorld : unit -> Async<Result<World, string>>
    abstract GetWarState : int option -> Async<Result<WarState, string>>
    abstract GetDates : unit -> Async<Result<DateTime[], string>>

type IControllerInteraction =
    abstract ResetCampaign : scenario:string -> Async<Result<string, string>>
    abstract Advance : unit -> Async<Result<SimulationStep[], string>>

type ISerializer =
    abstract SerializeAsync<'T> : Async<Result<'T, string>> -> string

let setJsonMimeType = setMimeType "application/json; charset=utf-8"

let private usage = """
GET /query/world
GET /query/current
GET /query/past/<n>
GET /query/dates

PUT /control/reset
PUT /control/advance
"""

let mkRoutes (ser : ISerializer, rr : IRoutingResponse, ctrl : IControllerInteraction) =
    let inline serializeAsync x = ser.SerializeAsync x
    choose [
        GET >=> choose [
            path "/query/world" >=> context (fun _ -> rr.GetWorld() |> serializeAsync |> OK)
            path "/query/current" >=> context (fun _ -> rr.GetWarState None |> serializeAsync |> OK)
            pathScan "/query/past/%d" (fun n -> rr.GetWarState(Some n) |> serializeAsync |> OK)
            path "/query/dates" >=> context (fun _ -> rr.GetDates() |> serializeAsync |> OK)
        ] >=> setJsonMimeType
        PUT >=> choose [
            path "/control/reset" >=> context (fun _ -> ctrl.ResetCampaign("RheinlandSummer") |> serializeAsync |> OK)
            path "/control/advance" >=> context (fun _ -> ctrl.Advance() |> serializeAsync |> OK)
        ] >=> setJsonMimeType
        GET >=> path "/help" >=> OK usage
        context (fun ctx ->
            "Invalid request. Try 'GET <url>/help' for a list of valid requests."
            |> NOT_FOUND)
    ]