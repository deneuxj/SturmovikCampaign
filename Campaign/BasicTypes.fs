module Campaign.BasicTypes

open System.Numerics
open Vector

open SturmovikMission.DataProvider
open SturmovikMission.Blocks.BlocksMissionData.CommonMethods

open Campaign.Util

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

/// A position on the map and a rotation around the vertical axis.
type OrientedPosition = {
    Pos : Vector2
    Rotation : float32
}

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
                  Rotation = block |> getYOri |> valueOf |> float32 }
        }

    /// List of sub-block numbers that represent objects with significant storage or production capabilities
    member this.SubBlocks = 
        match this.Model with
        | Contains "arf_barak" -> [1]
        | Contains "arf_dugouts_2" -> [2..5]
        | Contains "arf_ammo_1" -> [2]
        | Contains "industrial_200x140_01" -> [0..12]
        | Contains "industrial_200x140_02" -> [0..19]
        | Contains "industrial_300x100_01" -> [0..13]
        | Contains "industrial_300x100_02" -> [0..15]
        | Contains "industrial_300x100_03" -> [0..9]
        | Contains "industrial_300x100_04" -> [0..7]
        | Contains "industrial_cornerl_01" -> [0..8]
        | Contains "industrial_cornerl_02" -> [0..9]
        | Contains "industrial_cornerr_01" -> [0..6]
        | Contains "industrial_cornerr_02" -> [0..10]
        | _ -> []

    member this.Production =
        match this.Model with
        | Contains "industrial_" -> 25.0f<E/H>
        | _ -> 0.0f<E/H>

    member this.Storage =
        match this.Model with
        | Contains "arf_barak" -> 100.0f<E>
        | Contains "arf_dugouts_2" -> 400.0f<E>
        | Contains "arf_ammo_1" -> 100.0f<E>
        | _ -> 0.0f<E>

    member this.Durability =
        match this.Model with
        | Contains "arf_net" -> 1000
        | Contains "arf_dugout" -> 15000
        | Contains "arf_barak" -> 10000
        | Contains "arf_hangar" -> 10000
        | Contains "industrial" -> 10000
        | Contains "static_" -> 2500
        | _ -> 10000

    member this.RepairCost =
        match this.Model with
        | Contains "arf_barak" -> 100.0f<E>
        | Contains "arf_dugouts_2" -> 400.0f<E>
        | Contains "arf_ammo_1" -> 100.0f<E>
        | Contains "industrial_" -> 750.0f<E>
        | _ -> 100.0f<E>
