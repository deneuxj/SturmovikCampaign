// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2021 Johann Deneux <johann.deneux@gmail.com>
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

/// Description of a ship and its roles.
module Campaign.Common.Ship

open SturmovikMission.Blocks

[<RequireQualifiedAccess>]
type ShipRole =
    | Defensive
    | Offensive
    | Cargo
    | TroopLanding
with
    static member FromString(s : string) =
        match s with
        | "Cargo" -> ShipRole.Cargo
        | "Defensive" -> ShipRole.Defensive
        | "Offensive" -> ShipRole.Offensive
        | "TroopLanding" -> ShipRole.TroopLanding
        | _ -> failwithf "Unrecognized ship role '%s'" s

/// Extract ship roles from a description string of the form "...;Roles:Cargo,Defensive;..."
let extractShipRoles (desc : string) =
    desc.Split(";")
    |> Array.choose(fun assign ->
        match assign.Split(":", 2) with
        | [| "Roles"; roles |] ->
            let roles =
                roles.Split(",")
                |> Array.choose (fun s ->
                    try
                        ShipRole.FromString s
                        |> Some
                    with _ -> None
                )
            if roles.Length > 0 then
                Some roles
            else
                None
        | _ ->
            None
    )
    |> Array.concat

type ShipProperties =
    {
        Name : string
        LogName : string
        Roles : ShipRole list
        ScriptModel : Vehicles.VehicleTypeData
    }

let knownShipLogNames =
    [
        ("Destroyer Type 7", "destroyertype7.txt")
        ("Large Cargo Ship type 1", "largecargoshiptype1.txt")
        ("Large Tanker Ship type 1", "largetankershiptype1.txt")
        ("Gun Boat 1124 AAA", "1124.txt")
        ("Gun Boat 1124 MRLS", "1124bm13.txt")
        ("River Cargo Ship type Georgia AAA", "rivershipgeorgiaaaa.txt")
        ("Landing Boat type A", "landboata.txt")
        ("Large Cargo Ship type 1", "largecargoshiptype1.txt")
        ("Large Tanker Ship type 1", "largetankershiptype1.txt")
        ("Large Tanker Ship type 1 ger", "largetankershiptype1ger.txt")
        ("Peniche type A", "penichea.txt")
        ("Peniche type B", "penicheb.txt")
        ("British AAA Peniche", "penichegb.txt")
        ("German AAA Peniche", "penicheger.txt")
        ("Gun Ship type A", "rivergunshipa.txt")
        ("River Cargo Ship type Georgia", "rivershipgeorgia.txt")
        ("Submarine Type IIB", "subtype2b.txt")
        ("Submarine Type Shch ser.10", "subtypesh10.txt")
        ("Torpedo Boat G-5 series 11-bis", "torpboatg5s11b.txt")
        ("Torpedo Boat G-5 series 11-bis 213", "torpboatg5s11b213.txt")
        ("Torpedo Boat type S-38", "torpboats38.txt")
    ]