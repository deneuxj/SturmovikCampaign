// CampaignControlApp A small standalone controller that can run SturmovikCampaign
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
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

module SdsFile

open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Ast

let parseKeyStringValue s =
    match s with
    | ReId(key, ReLit "=" (ReString(value, EOF _))) ->
        Some(key, value)
    | _ ->
        None

let parseKeyIntValue s =
    match s with
    | ReId(key, ReLit "=" (ReInt(value, EOF _))) ->
        Some(key, value)
    | _ ->
        None

let parseSds sdsFile =
    System.IO.File.ReadAllLines sdsFile
    |> Seq.choose (fun line ->
        let s = Stream.FromString line
        match parseKeyStringValue s with
        | Some (k, v) -> Some(k, Value.String v)
        | None ->
            match parseKeyIntValue s with
            | Some (k, v) -> Some(k, Value.Integer v)
            | None -> None)
    |> Map.ofSeq
