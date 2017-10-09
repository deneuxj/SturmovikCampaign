module Campaign.BasicTypes

open System.Numerics
open VectorExtension

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
    member this.SubBlocks = 
        match this.Model with
        | Contains "arf_barak" -> [1]
        | Contains "arf_dugouts_2" -> [2..5]
        | Contains "arf_dugouts_3" -> [0..6]
        | Contains "arf_ammo_1" -> [0] @ [2..10]
        | Contains "arf_ammo_2" -> [0..9]
        | Contains "arf_ammo_3" -> [3..10]
        | Contains "arf_ammo_4" -> [2..8]
        | Contains "arf_sklad" -> [1]
        | Contains "arf_saray" -> [1]
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
        | Contains "vl_pvrz01" -> [1]
        | Contains "vl_pvrz03" -> [3]
        | Contains "vl_rounddepot" -> [1..3]
        | Contains "arf_hangars_1" -> [0..1]
        | Contains "arf_hangars_2" -> [0..1]
        | Contains "arf_hangars_3" -> [0]
        | Contains "arf_hangararc" -> [1]
        | Contains "arf_hangarbox" -> [1]
        | Contains "arf_gsm_1" -> [2..4]
        | Contains "arf_gsm_2" -> [1; 2]
        | Contains "meh_01" -> [0..7]
        | Contains "port_up_crane" -> [0;2]
        | Contains "port_up_group_cargo_190x15" -> [0..15]
        | Contains "port_up_group_cargo_50x25" -> [0..7]
        | Contains "port_up_group_smallsklad" -> [0..1]
        | Contains "port_up_group_smallwarehouse" -> [0..1]
        | Contains "port_up_object_smallsklad" -> [1]
        | Contains "port_up_object_smallwarehouse" -> [1]
        | Contains "port_up_unit_bags" -> [1]
        | Contains "port_up_unit_container" -> [1]
        | Contains "port_up_unit_woodbox_1x4" -> [1]
        | Contains "port_up_unit_woodbox_2x24" -> [1]
        | Contains "rwstation_b" -> [0..1] @ [3..7] @ [9..16]
        | Contains "rwstation_s1" -> 0 :: [2..3] @ [5] @ [7..10]
        | Contains "rwstation_s2" -> 0 :: [2..3] @ [6..8]
        | Contains "scot_01" -> [0..4] @ [15..21]
        | Contains "sklad_01" -> [0..14]
        | Contains "town_lrg_01" -> [0..6] @ [9..10] @ [12]
        | Contains "town_lrg_02" -> [0..4] @ [6..9]
        | Contains "town_lrg_03" -> [0..6] @ [8..13]
        | Contains "town_lrg_04" -> [0..4] @ [8..9] @ [11] @ [13..20]
        | Contains "town_lrg_05" -> [0..3] @ [5] @ [7..9]
        | Contains "town_lrg_06" -> [0..7] @ [9]
        | Contains "town_lrg_corner_01" -> 0 :: 2 :: [4..7]
        | Contains "town_lrg_corner_02" -> [0..6] @ [8]
        | Contains "watertower" -> [0]
        | _ -> []

    member this.Production(factor : float32) =
        match this.Model with
        | Contains "arf_barak" -> 1.0f<E/H>
        | Contains "arf_gsm" -> 1.0f<E/H>
        | Contains "arf_hangars_1" -> 10.0f<E/H>
        | Contains "arf_hangars_2" -> 10.0f<E/H>
        | Contains "arf_hangars_3" -> 5.0f<E/H>
        | Contains "arf_saray" -> 2.0f<E/H>
        | Contains "arf_sklad" -> 2.0f<E/H>
        | Contains "\\buildings\\" -> 0.0f<E/H>
        | Contains "industrial_" -> 25.0f<E/H>
        | Contains "vl_pvrz01" | Contains "vl_pvrz03" -> 25.0f<E/H>
        | Contains "vl_rounddepot" -> 25.0f<E/H>
        | Contains "meh_01" -> 10.0f<E/H>
        | Contains "port_up_crane" -> 5.0f<E/H>
        | Contains "port_up_group_cargo_190x15" -> 10.0f<E/H>
        | Contains "port_up_group_cargo_50x25" -> 5.0f<E/H>
        | Contains "port_up_group_smallsklad" -> 5.0f<E/H>
        | Contains "port_up_group_smallwarehouse" -> 7.5f<E/H>
        | Contains "port_up_object_smallsklad" -> 2.5f<E/H>
        | Contains "port_up_object_smallwarehouse" -> 4.0f<E/H>
        | Contains "port_up_unit_bags" -> 0.5f<E/H>
        | Contains "port_up_unit_container" -> 1.0f<E/H>
        | Contains "port_up_unit_woodbox_1x4" -> 0.5f<E/H>
        | Contains "port_up_unit_woodbox_2x24" -> 2.0f<E/H>
        | Contains "rwstation_b" -> 15.0f<E/H>
        | Contains "rwstation_s" -> 10.0f<E/H>
        | Contains "scot_01" -> 20.0f<E/H>
        | Contains "sklad_01" -> 20.0f<E/H>
        | Contains "school" -> 0.0f<E/H>
        | Contains "town_lrg" -> 2.0f<E/H>
        | Contains "town_lrg_corner" -> 1.0f<E/H>
        | Contains "vl_selsovet" -> 0.0f<E/H>
        | Contains "watertower" -> 5.0f<E/H>
        | _ ->
            printfn "No production in %s" this.Model
            0.0f<E/H>
        |> (*) factor

    member this.Storage =
        match this.Model with
        | Contains "arf_barak" -> 100.0f<E>
        | Contains "arf_dugouts_2" -> 400.0f<E>
        | Contains "arf_dugouts_3" -> 600.0f<E>
        | Contains "arf_ammo_1" -> 100.0f<E>
        | Contains "arf_ammo_2" -> 125.0f<E>
        | Contains "arf_ammo_3" -> 50.0f<E>
        | Contains "arf_ammo_4" -> 75.0f<E>
        | Contains "arf_hangararc" -> 1000.0f<E>
        | Contains "arf_hangarbox" -> 1000.0f<E>
        | Contains "arf_hangars_1" -> 1200.0f<E>
        | Contains "arf_hangars_2" -> 1200.0f<E>
        | Contains "arf_hangars_3" -> 600.0f<E>
        | Contains "arf_gsm_1" -> 100.0f<E>
        | Contains "arf_gsm_2" -> 75.0f<E>
        | Contains "arf_tower"
        | Contains "arf_nets" -> 0.0f<E>
        | Contains "arf_saray" -> 500.0f<E>
        | Contains "arf_sklad" -> 750.0f<E>
        | _ ->
            printfn "No storage in %s" this.Model
            0.0f<E>

    member this.Durability =
        match this.Model with
        | Contains "arf_net" -> 1000
        | Contains "arf_ammo" -> 1000
        | Contains "arf_dugout" -> 15000
        | Contains "arf_barak" -> 7000
        | Contains "arf_hangar" -> 10000
        | Contains "industrial" -> 10000
        | Contains "vl_pvrz01" | Contains "vl_pvrz03" -> 10000
        | Contains "vl_rounddepot" -> 10000
        | Contains "arf_gsm_1" -> 1000
        | Contains "arf_gsm_2" -> 1000
        | Contains "port_up_group_cargo" -> 1000
        | Contains "port_up_unit" -> 1000
        | Contains "scot" -> 7000
        | Contains "sklad" -> 7000
        | Contains "saray" -> 7000
        | Contains "town" -> 7000
        | Contains "meh" -> 7000
        | Contains "warehouse" -> 7000
        | Contains "static_" -> 2500
        | _ ->
            printfn "Default durability for %s" this.Model
            10000

    member this.RepairCost =
        this.Production(1.0f) * 40.0f<H> + this.Storage + 0.1f<E> * float32 this.Durability
