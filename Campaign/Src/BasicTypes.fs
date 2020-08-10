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

[<Measure>]
/// Distance
type M

[<Measure>]
/// Military ground force
type MGF

type CoalitionId = Axis | Allies
with
    /// Try to convert from an MCU coalition value
    static member FromMcuValue(v) =
        match v with
        | Mcu.CoalitionValue.Axis -> Some Axis
        | Mcu.CoalitionValue.Allies -> Some Allies
        | Mcu.CoalitionValue.CentralPowers -> Some Axis
        | Mcu.CoalitionValue.Entente -> Some Allies
        | _ -> None

    /// <summary>
    /// Convert a country value from a MCU in a mission to a CoalitionId option.
    /// </summary>
    static member FromCountry(country : Mcu.CountryValue) =
        match McuUtil.coalitionOf country with
        | Mcu.CoalitionValue.Allies -> Some Allies
        | Mcu.CoalitionValue.Axis -> Some Axis
        | Mcu.CoalitionValue.CentralPowers -> Some Axis
        | Mcu.CoalitionValue.Entente -> Some Allies
        | _ -> None

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

    static member FromString(s) =
        match s with
        | "Axis" -> Axis
        | "Allies" -> Allies
        | _ -> failwithf "Invalid coalition '%s'" s

type CountryId =
    | Russia
    | UnitedStates
    | GreatBritain
    | Germany
    | Italy
with
    static member All = [
        Russia
        UnitedStates
        GreatBritain
        Germany
        Italy
    ]
    member this.Coalition =
        match this with
        | Russia | UnitedStates | GreatBritain -> Allies
        | Germany | Italy -> Axis

    member this.ToMcuValue =
        match this with
        | Russia -> Mcu.CountryValue.Russia
        | UnitedStates -> Mcu.CountryValue.UnitedStates
        | GreatBritain -> Mcu.CountryValue.GreatBritain
        | Germany -> Mcu.CountryValue.Germany
        | Italy -> Mcu.CountryValue.Italy

    static member FromMcuValue (x : Mcu.CountryValue) =
        match x with
        | Mcu.CountryValue.Russia -> Some Russia
        | Mcu.CountryValue.UnitedStates -> Some UnitedStates
        | Mcu.CountryValue.GreatBritain -> Some GreatBritain
        | Mcu.CountryValue.Germany -> Some Germany
        | Mcu.CountryValue.Italy -> Some Italy
        | _ -> None

    /// <summary>
    /// Convert from a country value from a log entry.
    /// </summary>
    static member FromLogEntry(country : ploggy.Country) =
        match country with
        | ploggy.Country.Germany | ploggy.Country.OtherAxis -> Some Germany
        | ploggy.Country.USSR | ploggy.Country.OtherAllies -> Some Russia
        | ploggy.Country.None | ploggy.Country.Neutral -> None // Should not happen
        | _ -> failwithf "Unknown country value %d" (int country)

/// A position on the map and a rotation around the vertical axis.
[<CustomComparison>]
[<CustomEquality>]
type OrientedPosition = {
    Pos : Vector2
    Rotation : float32
    Altitude : float32
}
with
    static member inline FromMission(block) =
        {
            Pos = Vector2.FromPos(block)
            Rotation = getYOri block |> valueOf |> float32
            Altitude = getAlt block |> valueOf |> float32
        }

    member private this.AsTuple =
        (this.Pos.X, this.Pos.Y, this.Rotation, this.Altitude)

    override this.ToString() =
        string this.AsTuple

    override this.Equals(other) =
        match other with
        | :? OrientedPosition as other ->
            this.AsTuple = other.AsTuple
        | _ ->
            false

    override this.GetHashCode() =
        hash this.AsTuple

    interface System.IComparable<OrientedPosition> with
        member this.CompareTo(other): int =
            compare this.AsTuple other.AsTuple

    interface System.IComparable with
        member this.CompareTo(other): int =
            match other with
            | :? OrientedPosition as other ->
                compare this.AsTuple other.AsTuple
            | _ ->
                invalidArg "other" "Must be an OrientedPosition"

/// Constants used to pick the kind of fires to display
let bigDamage = 500.0f<E>
let mediumDamage = 300.0f<E>
let smallDamage = 25.0f<E>

/// Kind of plane that can fit in a parking spot
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
    Durability : int
}
with
    /// Create from a pattern and a string representation of the list of sub-blocks
    static member Create(pattern, subBlocks : string, production : float, storage : float, durability : int) =
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
          Durability = if durability > 0 then durability else defaultDurability }


    member this.RepairCost =
        match this.Production, this.Storage with
        | prod, _ when prod > 0.0f<E/H> ->
            prod * 120.0f<H>
            |> max 960.0f<E> // Assuming repair speed of 10.0f<E/H>, that's at least 96 hours to repair factories.
        | _, storage when storage > 0.0f<E> ->
            4.0f * storage
            |> max 60.0f<E> // Assuming repair speed of 10.0f<E/H>, that's at least 6 hours to repair storage.
            |> min 240.0f<E> // At most 24 hours to repair storage.
        | _ -> 100.0f<E>

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

    member this.TryFind(subBlocksSpecs) =
        subBlocksSpecs
        |> List.tryFind (fun spec -> this.Model.Contains(spec.Pattern))

    /// Array of sub-block numbers that represent objects with significant storage or production capabilities
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

    member this.Durability(subBlocksSpecs) =
        // Reduce durability after changes in version 3.010 of the game
        let adjustDurability x =
            let low = min 10000.0f x
            let med = min 15000.0f (x - 10000.0f) |> max 0.0f
            let high = min 10000.0f (x - 25000.0f) |> max 0.0f
            low + 0.5f * med + 0.1f * high

        subBlocksSpecs
        |> List.tryPick (fun spec ->
            if this.Model.Contains(spec.Pattern) then
                Some spec.Durability
            else
                None)
        |> Option.defaultValue defaultDurability
        |> float32
        |> adjustDurability
        |> int

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
        this.TryFind(subBlocksSpecs)
        |> Option.map (fun x -> x.RepairCost)
        |> Option.defaultValue 100.0f<E>
