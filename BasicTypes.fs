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

type PlaneParkingSize =
    | SizeFighter
    | SizeAttacker
    | SizeBomber

type ReferencePositions =
    { RefPos : Vector2
      Positions : (Vector3 * PlaneParkingSize) list // Z is orientation, in degrees (0 .. 360)
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
        | Contains "industrial_block_fuel35m_300x100" -> [0..7]
        | Contains "industrial_block_fuel25m_300x100" -> [0..6]
        | Contains "industrial_block_bigwarehouse_300x100" -> [1..4] @ [6..7]
        | Contains "industrial_block_midwarehouse_200x140" -> [0..8]
        | Contains "industrial_block_smallwarehouse_150x100" -> [0..12] @ [19..21] @ [23..29]
        | Contains "industrial_block_smallwarehouse_300x100" -> [0..8] @ [10..12] @ [15] @ [23..28] @ [33..42]
        | Contains "industrial_block_smallwarehouse2_150x100" -> [0..18]
        | Contains "industrial_block_smallwarehouse2_300x100" -> [0..12] @ [15] @ [22..26] @ [33..37]
        | Contains "industrial_block_fuel_300x100" -> [2..3] @ [16..19]
        | Contains "industrial_cornerl_01" -> [0..8]
        | Contains "industrial_cornerl_02" -> [0..9]
        | Contains "industrial_cornerr_01" -> [0..6]
        | Contains "industrial_cornerr_02" -> [0..10]
        | Contains "industrial_object_oil" -> [1]
        | Contains "industrial_object_zavodkorpys45m" -> [1]
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
        | _ -> 0.0f<E/H>
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
        | Contains "arf_saray" -> 200.0f<E>
        | Contains "arf_sklad" -> 300.0f<E>
        | _ -> 0.0f<E>

    member this.IsAirfieldStorage =
        match this.Model with
        | Contains "arf_ammo_1"
        | Contains "arf_ammo_2"
        | Contains "arf_ammo_3"
        | Contains "arf_ammo_4"
        | Contains "arf_hangararc"
        | Contains "arf_hangarbox"
        | Contains "arf_hangars_1"
        | Contains "arf_hangars_2"
        | Contains "arf_hangars_3"
        | Contains "arf_gsm_1"
        | Contains "arf_gsm_2"
        | Contains "arf_saray"
        | Contains "arf_sklad" -> true
        | _ -> false

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
            10000

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

    member this.RepairCost =
        this.Production(1.0f) * 40.0f<H> + this.Storage + 0.1f<E> * float32 this.Durability
