// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
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

module Campaign.BasicTypes

open System.Numerics
open VectorExtension

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData.CommonMethods

open Util

[<Measure>]
/// Cost (energy)
type E

[<Measure>]
/// Time
type H

[<Measure>]
/// Mass
type K

type CoalitionId = Axis | Allies
with
    /// <summary>
    /// Convert from a country value from a log entry.
    /// </summary>
    static member FromLogEntry(country : ploggy.Country) =
        match country with
        | ploggy.Country.Germany | ploggy.Country.OtherAxis -> Some Axis
        | ploggy.Country.USSR | ploggy.Country.OtherAllies -> Some Allies
        | ploggy.Country.None | ploggy.Country.Neutral -> None // Should not happen
        | _ -> failwithf "Unknown country value %d" (int country)

    /// <summary>
    /// Convert to a numerical country value suitable for use in mission files.
    /// </summary>
    member this.ToCountry =
        match this with
        | Axis -> Mcu.CountryValue.Germany
        | Allies -> Mcu.CountryValue.Russia

    /// <summary>
    /// Convert to a numerical coalition value suitable for use in mission files.
    /// </summary>
    member this.ToCoalition =
        match this with
        | Axis -> Mcu.CoalitionValue.Axis
        | Allies -> Mcu.CoalitionValue.Allies

    /// <summary>
    /// Return the opposite coalition.
    /// </summary>
    member this.Other =
        match this with
        | Axis -> Allies
        | Allies -> Axis

    override this.ToString() =
        match this with
        | Axis -> "Axis"
        | Allies -> "Allies"

/// A position on the map and a rotation around the vertical axis.
type OrientedPosition = {
    Pos : Vector2
    Rotation : float32
    Altitude : float32
}

let bigDamage = 500.0f<E>
let mediumDamage = 300.0f<E>
let smallDamage = 25.0f<E>

type PlaneParkingSize =
    | SizeFighter
    | SizeAttacker
    | SizeBomber

type ReferencePositions =
    { RefPos : Vector2
      Positions : (Vector3 * PlaneParkingSize) list // Z is orientation, in degrees (0 .. 360)
    }

open SturmovikMission.DataProvider.Parsing

let private defaultDurability = 25000

/// A static block model substring and a list of sub-block identifiers
type SubBlockSpec = {
    Pattern : string
    SubBlocks : int[]
    Production : float32<E/H>
    Storage : float32<E>
    IsAirfield : bool
    Durability : int
}
with
    /// Create from a pattern and a string representation of the list of sub-blocks
    static member Create(pattern, subBlocks : string, production : float, storage : float, isAirfield, durability : int) =
        let reInt = regex(@"\G\s*([+-]?\d+)")
        let (|ReInt|_|) (SubString(data, offset)) =
            let m = reInt.Match(data, offset)
            let g = m.Groups.[1]
            if m.Success then
                Some (System.Int32.Parse g.Value, SubString(data, g.Index + g.Length))
            else
                None
        let parseError s = parseError(sprintf "Failed to parse sub-blocks of pattern '%s'" pattern, s)
        let rec parseUnits s =
            match s with
            | ReInt (n, ReLit ";" s) ->
                match parseUnits s with
                | None -> parseError s
                | Some(x, s) -> Some(n :: x, s)
            | ReInt (n, ReLit "]" s) ->
                Some([n], s)
            | ReLit "]" s ->
                Some([], s)
            | _ -> parseError s
        let parseSquare s =
            match s with
            | ReLit "[" (ReInt (n0, ReLit ".." (ReInt (n1, ReLit "]" s)))) ->
                Some([n0..n1], s)
            | ReLit "[" s  ->
                parseUnits s
            | _ ->
                None
        let rec parseAll s =
            match parseSquare s with
            | None -> parseError s
            | Some(x, EOF _) -> [x]
            | Some(x, ReLit "@" s) ->
                x :: parseAll s
            | Some(x, s) -> parseError s
        { Pattern = pattern
          SubBlocks = Stream.FromString subBlocks |> parseAll|> List.concat |> Array.ofList
          Production = 1.0f<E/H> * float32 production
          Storage = 1.0f<E> * float32 storage
          IsAirfield = isAirfield
          Durability = if durability > 0 then durability else defaultDurability }

/// A group of buildings or some other static objects.
type StaticGroup = {
    Model : string
    Script : string
    Pos : OrientedPosition
}
with
    static member inline FromBlock(block : ^T) =
        { Model = block |> getModel |> valueOf
          Script = block |> getScript |> valueOf
          Pos = { Pos = Vector2.FromPos block
                  Rotation = block |> getYOri |> valueOf |> float32 
                  Altitude = block |> getAlt |> valueOf |> float32 }
        }

    /// List of sub-block numbers that represent objects with significant storage or production capabilities
    /// When adding buildings here, one must also remember to update the code in ResultExtraction, active pattern BuildingObjectType
    member this.SubBlocks(subBlocksSpecs) =
        subBlocksSpecs
        |> List.tryPick (fun spec ->
            if this.Model.Contains(spec.Pattern) then
                Some spec.SubBlocks
            else
                None)
        |> Option.defaultValue [||]

    member this.Production(subBlocksSpecs, factor : float32) =
        subBlocksSpecs
        |> List.tryPick (fun spec ->
            if this.Model.Contains(spec.Pattern) then
                Some spec.Production
            else
                None)
        |> Option.defaultValue 0.0f<E/H>
        |> (*) factor

    member this.Storage(subBlocksSpecs) =
        subBlocksSpecs
        |> List.tryPick (fun spec ->
            if this.Model.Contains(spec.Pattern) then
                Some spec.Storage
            else
                None)
        |> Option.defaultValue 0.0f<E>

    member this.IsAirfieldStorage(subBlocksSpecs) =
        subBlocksSpecs
        |> List.tryPick (fun spec ->
            if this.Model.Contains(spec.Pattern) then
                Some spec.IsAirfield
            else
                None)
        |> Option.defaultValue false

    member this.Durability(subBlocksSpecs) =
        subBlocksSpecs
        |> List.tryPick (fun spec ->
            if this.Model.Contains(spec.Pattern) then
                Some spec.Durability
            else
                None)
        |> Option.defaultValue defaultDurability

    member this.PlaneParkingPositions =
        match this.Model with
        | Contains "arf_caponiers_10_h_3.mgm" ->
            { RefPos = Vector2(0.0f, 700.0f)
              Positions =
              [
                Vector3(0.0f, 631.0f, 0.0f), SizeBomber
                Vector3(0.0f, 700.0f, 0.0f), SizeBomber
                Vector3(0.0f, 768.0f, 0.0f), SizeBomber
              ]
            }
            |> Some
        | Contains "arf_caponiers_11_h_5.mgm" ->
            { RefPos = Vector2(0.0f, 700.0f)
              Positions =
              [
                Vector3(0.0f, 631.0f, 0.0f), SizeAttacker
                Vector3(0.0f, 700.0f, 0.0f), SizeAttacker
                Vector3(0.0f, 768.0f, 0.0f), SizeAttacker
                Vector3(47.0f, 575.0f, 90.0f), SizeBomber
                Vector3(47.0f, 825.0f, 270.0f), SizeBomber
              ]
            }
            |> Some
        | Contains "arf_caponiers_12_h_5.mgm" ->
            { RefPos = Vector2(0.0f, 700.0f)
              Positions =
              [
                Vector3(0.0f, 631.0f, 0.0f), SizeFighter
                Vector3(0.0f, 700.0f, 0.0f), SizeFighter
                Vector3(0.0f, 768.0f, 0.0f), SizeFighter
                Vector3(47.0f, 575.0f, 90.0f), SizeFighter
                Vector3(47.0f, 825.0f, 270.0f), SizeFighter
              ]
            }
            |> Some
        | Contains "arf_caponiers_1_v_5.mgm" ->
            { RefPos = Vector2(1000.0f, 700.0f)
              Positions =
              [
                Vector3(948.0f, 604.0f, 0.0f), SizeAttacker
                Vector3(931.0f, 696.0f, 0.0f), SizeAttacker
                Vector3(946.0f, 781.0f, 0.0f), SizeAttacker
                Vector3(1067.0f, 649.0f, 180.0f), SizeBomber
                Vector3(1067.0f, 747.0f, 180.0f), SizeAttacker
              ]
            }
            |> Some
        | Contains "arf_caponiers_2_v_5.mgm" ->
            { RefPos = Vector2(1000.0f, 700.0f)
              Positions =
              [
                Vector3(948.0f, 604.0f, 0.0f), SizeFighter
                Vector3(931.0f, 696.0f, 0.0f), SizeFighter
                Vector3(946.0f, 781.0f, 0.0f), SizeFighter
                Vector3(1067.0f, 649.0f, 180.0f), SizeFighter
                Vector3(1067.0f, 747.0f, 180.0f), SizeFighter
              ]
            }
            |> Some
        | Contains "arf_caponiers_3_v_5.mgm" ->
            { RefPos = Vector2(300.0f, 1000.0f)
              Positions =
              [
                Vector3(353.0f, 941.0f, 170.0f), SizeBomber
                Vector3(961.0f, 1049.0f, 198.0f), SizeBomber
                Vector3(257.0f, 909.0f, 7.0f), SizeBomber
                Vector3(232.0f, 993.0f, 0.0f), SizeAttacker
                Vector3(250.0f, 1072.0f, 351.0f), SizeBomber
              ]
            }
            |> Some
        | Contains "arf_caponiers_4_r_6.mgm" ->
            { RefPos = Vector2(1000.0f, 700.0f)
              Positions =
              [
                Vector3(1060.0f, 537.0f, 348.0f), SizeBomber
                Vector3(1089.0f, 818.0f, 14.0f), SizeBomber
                Vector3(984.0f, 713.0f, 344.0f), SizeAttacker
                Vector3(891.0f, 480.0f, 11.0f), SizeAttacker
                Vector3(844.0f, 731.0f, 2.0f), SizeAttacker
                Vector3(912.0f, 896.0f, 357.0f), SizeBomber
              ]
            }
            |> Some
        | Contains "arf_caponiers_5_r_7.mgm" ->
            { RefPos = Vector2(1000.0f, 700.0f)
              Positions =
              [
                Vector3(1073.0f, 505.0f, 349.0f), SizeFighter
                Vector3(1085.0f, 816.0f, 15.0f), SizeFighter
                Vector3(964.0f, 578.0f, 9.0f), SizeFighter
                Vector3(961.0f, 768.0f, 345.0f), SizeFighter
                Vector3(887.0f, 479.0f, 10.0f), SizeFighter
                Vector3(840.0f, 731.0f, 2.0f), SizeFighter
                Vector3(910.0f, 896.0f, 357.0f), SizeFighter
              ]
            }
            |> Some
        | Contains "arf_caponiers_6_h_4.mgm" ->
            { RefPos = Vector2(1000.0f, 700.0f)
              Positions =
              [
                Vector3(1046.0f, 606.0f, 90.0f), SizeFighter
                Vector3(986.0f, 665.0f, 358.0f), SizeFighter
                Vector3(984.0f, 732.0f, 359.0f), SizeFighter
                Vector3(987.0f, 801.0f, 8.0f), SizeFighter
              ]
            }
            |> Some
        | Contains "arf_caponiers_7_h_4.mgm" ->
            { RefPos = Vector2(1000.0f, 700.0f)
              Positions =
              [
                Vector3(1046.0f, 606.0f, 90.0f), SizeBomber
                Vector3(986.0f, 665.0f, 358.0f), SizeAttacker
                Vector3(984.0f, 732.0f, 359.0f), SizeAttacker
                Vector3(987.0f, 801.0f, 8.0f), SizeAttacker
              ]
            }
            |> Some
        | Contains "arf_caponiers_8_h_4.mgm" ->
            { RefPos = Vector2(1000.0f, 700.0f)
              Positions =
              [
                Vector3(986.0f, 665.0f, 358.0f), SizeAttacker
                Vector3(984.0f, 732.0f, 359.0f), SizeAttacker
                Vector3(987.0f, 801.0f, 8.0f), SizeBomber
                Vector3(1046.0f, 793.0f, 270.0f), SizeAttacker
              ]
            }
            |> Some
        | Contains "arf_caponiers_9_h_3.mgm" ->
            { RefPos = Vector2(300.0f, 1000.0f)
              Positions =
              [
                Vector3(309.0f, 931.0f, 358.0f), SizeAttacker
                Vector3(309.0f, 1000.0f, 359.0f), SizeAttacker
                Vector3(310.0f, 1067.0f, 8.0f), SizeAttacker
              ]
            }
            |> Some
        | Contains "arf_hangararc.mgm" ->
            { RefPos = Vector2(1000.0f, 1000.0f)
              Positions =
              [
                Vector3(937.0f, 984.0f, 180.0f), SizeBomber
                Vector3(939.0f, 1016.0f, 180.0f), SizeBomber
              ]
            }
            |> Some
        | Contains "arf_hangarbox.mgm" ->
            { RefPos = Vector2(1000.0f, 1000.0f)
              Positions =
              [
                Vector3(954.0f, 1000.0f, 180.0f), SizeBomber
              ]
            }
            |> Some
        | Contains "arf_hangars_1.mgm" ->
            { RefPos = Vector2(1000.0f, 1000.0f)
              Positions =
              [
                Vector3(958.0f, 958.0f, 180.0f), SizeAttacker
                Vector3(958.0f, 1042.0f, 180.0f), SizeAttacker
              ]
            }
            |> Some
        | Contains "arf_hangars_3.mgm" ->
            { RefPos = Vector2(1000.0f, 1000.0f)
              Positions =
              [
                Vector3(1000.0f, 957.0f, 270.0f), SizeAttacker
              ]
            }
            |> Some
        | Contains "arf_nets_1_4.mgm" ->
            { RefPos = Vector2(1000.0f, 1000.0f)
              Positions =
              [
                Vector3(1004.0f, 928.0f, 10.0f), SizeFighter
                Vector3(1003.0f, 969.0f, 7.0f), SizeFighter
                Vector3(1002.0f, 1038.0f, 10.0f), SizeFighter
                Vector3(1002.0f, 1067.0f, 359.0f), SizeFighter
              ]
            }
            |> Some
        | Contains "arf_nets_2_5.mgm" ->
            { RefPos = Vector2(1000.0f, 1000.0f)
              Positions =
              [
                Vector3(1010.0f, 927.0f, 10.0f), SizeFighter
                Vector3(1000.0f, 967.0f, 7.0f), SizeFighter
                Vector3(1002.0f, 1001.0f, 350.0f), SizeFighter
                Vector3(1005.0f, 1045.0f, 12.0f), SizeAttacker
                Vector3(1001.0f, 1090.0f, 359.0f), SizeFighter
              ]
            }
            |> Some
        | Contains "arf_nets_3_3.mgm" ->
            { RefPos = Vector2(1000.0f, 1000.0f)
              Positions =
              [
                Vector3(1020.0f, 1007.0f, 5.0f), SizeAttacker
                Vector3(1001.0f, 1070.0f, 356.0f), SizeFighter
              ]
            }
            |> Some
        | _ ->
            None

    /// <summary>
    /// Cost of fully repairing something. Depends on the building's capacity (production or storage).
    /// </summary>
    member this.RepairCost(subBlocksSpecs) =
        match this.Production(subBlocksSpecs, 1.0f), this.Storage(subBlocksSpecs) with
        | prod, _ when prod > 0.0f<E/H> -> prod * 10.0f<H>
        | _, storage when storage > 0.0f<E> -> storage
        | _ -> 100.0f<E>
        |> max 60.0f<E> // Assuming repair speed of 10.0f<E/H>, that's at least 6 hours to repair anything.
        |> min 240.0f<E> // At most 24 hours to repair anything.
