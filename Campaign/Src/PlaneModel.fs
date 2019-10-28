﻿// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
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

module Campaign.PlaneModel

open SturmovikMission.Blocks

open Campaign.BasicTypes
open Util
open System.Numerics

type PlaneType  =
    | Fighter
    | Attacker
    | Bomber
    | Transport
with
    override this.ToString() =
        match this with
        | Fighter -> "fighter"
        | Attacker -> "attacker"
        | Bomber -> "bomber"
        | Transport -> "transport"

type PlaneRole =
    | Interceptor
    | Patroller
    | GroundAttacker
    | LevelBomber
    | CargoTransporter

type PlaneData =
    { Kind : PlaneType
      Name : string
      MissionLogName : string
      Roles : PlaneRole list
      Coalition : CoalitionId
      ScriptModel : Vehicles.VehicleTypeData
      Cost : float32<E>
      BombCapacity : float32<K>
      CargoCapacity : float32<K>
      Payloads : Map<PlaneRole, int>
      ModMasks : Map<PlaneRole, int>
      BombLoads : (int * float32<K>) list
      SpecialLoadsCosts : (int * float<E>) list
      EmptyPayload : int
    }
with
    member this.SerializableValue =
        [
            yield "Kind", this.Kind.ToString() :> obj
            yield "Name", this.Name :> obj
            yield "MissionLogName", this.MissionLogName :> obj
            yield "Roles", this.Roles |> List.map (fun role -> role.ToString()) :> obj
            yield "Coalition", this.Coalition.ToString() :> obj
            yield "ScriptModel", this.ScriptModel :> obj
            yield "Cost", box this.Cost
            yield "BombCapacity", box this.BombCapacity
            yield "CargoCapacity", box this.CargoCapacity
            yield "Payloads", this.Payloads |> Map.toList |> List.map (fun (role, x) -> role.ToString(), x) |> dict :> obj
            yield "ModMasks", this.ModMasks |> Map.toList |> List.map (fun (role, x) -> role.ToString(), x) |> dict :> obj
            yield "BombLoads", this.BombLoads :> obj
            yield "SpecialLoadsCosts", this.SpecialLoadsCosts :> obj
            yield "EmptyPayload", box this.EmptyPayload
        ]
        |> dict

let basePlaneCost = 500.0f<E>

/// times 2 [(4, x); (10, y)] -> [(4, x); (5, x); (10, y); (11, y)]
let private times N xs =
    [
        for (n, x) in xs do
            for c in 0..(N - 1) do
                yield (n + c, x)
    ]
/// vTimes [(3, 10, x)] -> [(10, x); (11, x); (12, x)]
let private vTimes xs =
    [
        for (n, N, x) in xs do
            for c in 0..(N - 1) do
                yield (n + c, x)
    ]
/// xTimes [10; 20] [(1, x); (2, y)] -> [(11, x); (12, y); (21, x); (22, y)]
let private xTimes offsets xs =
    [
        for offset in offsets do
            for (n, x) in xs do
                yield (n + offset, x)
    ]

/// Various kind of planes used in the 1941/42 Moscow theater
type PlaneModel =
    | FokkerD7
    | FokkerD7f
    | FokkerDr1
    | Pfalzd3a
    | SopCamel
    | SopDolphin
    | Spad13
    | AlbatrosD5
    | HalberstadtCl2
    | HalberstadtCl2Au
    | BristolF2Bf2
    | BristolF2Bf3
    | Bf109e7
    | Bf109f2
    | Bf109f4
    | Bf109g2
    | Bf109g4
    | Bf109g6
    | Bf109g14
    | Bf109k4
    | Fw190a3
    | Fw190a5
    | Fw190a8
    | Fw190d9
    | Mc202
    | Bf110e
    | Bf110g
    | Ju88a4
    | Ju52
    | I16
    | IL2M41
    | IL2M42
    | IL2M43
    | Mig3
    | P38 //
    | P39
    | P40
    | P47
    | P51 //
    | Yak1s69
    | Yak1s127
    | Yak7bs36
    | La5
    | La5fns2
    | Lagg3s29
    | SpitfireMkVb
    | SpitfireMkIXe
    | Tempest //
    | Pe2s35
    | Pe2s87
    | Ju87
    | He111h6
    | He111h16
    | Hs129b2
    | A20
    | U2
with
    member this.ScriptModel =
        match this with
        | FokkerD7 -> Vehicles.vehicles.fokkerd7
        | FokkerD7f -> Vehicles.vehicles.fokkerd7f
        | FokkerDr1 -> Vehicles.vehicles.fokkerdr1
        | Pfalzd3a -> Vehicles.vehicles.pfalzd3a
        | SopCamel -> Vehicles.vehicles.sopcamel
        | SopDolphin -> Vehicles.vehicles.sopdolphin
        | Spad13 -> Vehicles.vehicles.spad13
        | AlbatrosD5 -> Vehicles.vehicles.albatrosd5
        | HalberstadtCl2 -> Vehicles.vehicles.haberstadtcl2
        | HalberstadtCl2Au -> Vehicles.vehicles.haberstadtcl2au
        | BristolF2Bf2 -> Vehicles.vehicles.bristolf2bf2
        | BristolF2Bf3 -> Vehicles.vehicles.bristolf2bf3
        | Bf109e7 -> Vehicles.vehicles.GermanFighter1
        | Bf109f2 -> Vehicles.vehicles.GermanFighter2
        | Mc202 -> Vehicles.vehicles.GermanFighter3
        | Bf109f4 -> Vehicles.vehicles.GermanFighter4
        | Bf109g2 -> Vehicles.vehicles.GermanFighter5
        | Fw190a3 -> Vehicles.vehicles.GermanFighter6
        | Fw190a5 -> Vehicles.vehicles.GermanFighter7
        | Fw190a8 -> Vehicles.vehicles.fw190a8
        | Fw190d9 -> Vehicles.vehicles.fw190d9
        | Bf109g4 -> Vehicles.vehicles.GermanFighter8
        | Bf109g6 -> Vehicles.vehicles.GermanFighter9
        | Bf109g14 -> Vehicles.vehicles.bf109g14
        | Bf109k4 -> Vehicles.vehicles.bf109k4
        | Bf110e -> Vehicles.vehicles.GermanAttacker1
        | Bf110g -> Vehicles.vehicles.GermanAttacker3
        | Ju88a4 -> Vehicles.vehicles.GermanBomber1
        | Ju52 -> Vehicles.vehicles.GermanTransport
        | I16 -> Vehicles.vehicles.RussianFighter1
        | IL2M41 -> Vehicles.vehicles.RussianAttacker1
        | IL2M42 -> Vehicles.vehicles.RussianAttacker2
        | IL2M43 -> Vehicles.vehicles.RussianAttacker3
        | Mig3 -> Vehicles.vehicles.RussianFighter2
        | P38 -> Vehicles.vehicles.p38j25
        | P39 -> Vehicles.vehicles.RussianFighter10
        | P40 -> Vehicles.vehicles.RussianFighter3
        | P47 -> Vehicles.vehicles.p47d28
        | P51 -> Vehicles.vehicles.p51d15
        | Yak1s69 -> Vehicles.vehicles.RussianFighter4
        | Yak1s127 -> Vehicles.vehicles.RussianFighter7
        | La5 -> Vehicles.vehicles.RussianFighter6
        | Lagg3s29 -> Vehicles.vehicles.RussianFighter5
        | SpitfireMkVb -> Vehicles.vehicles.RussianFighter8
        | SpitfireMkIXe -> Vehicles.vehicles.spitfiremkixe
        | Tempest -> Vehicles.vehicles.tempestmkvs2
        | Pe2s35 -> Vehicles.vehicles.RussianBomber1
        | Pe2s87 -> Vehicles.vehicles.RussianBomber2
        | Ju87 -> Vehicles.vehicles.GermanAttacker2
        | He111h6 -> Vehicles.vehicles.GermanBomber2
        | He111h16 -> Vehicles.vehicles.GermanBomber3
        | Hs129b2 -> Vehicles.vehicles.GermanAttacker4
        | La5fns2 -> Vehicles.vehicles.RussianFighter9
        | Yak7bs36 -> Vehicles.vehicles.RussianFighter11
        | A20 -> Vehicles.vehicles.RussianBomber3
        | U2 -> Vehicles.vehicles.u2vs

    member this.Cost =
        match this with
        | FokkerD7
        | FokkerD7f
        | FokkerDr1
        | Pfalzd3a
        | SopCamel
        | SopDolphin
        | Spad13 
        | AlbatrosD5
        | HalberstadtCl2
        | HalberstadtCl2Au
        | BristolF2Bf2
        | BristolF2Bf3 -> 0.25f * basePlaneCost
        | Bf109e7 -> basePlaneCost
        | Bf109f4
        | Bf109g2
        | Bf109g4
        | Bf109g6
        | Fw190a3
        | Fw190a5
        | Bf109f2 -> (5.0f / 3.0f) * basePlaneCost
        | Bf109g14 | Bf109k4 | Fw190a8 | Fw190d9 -> 2.0f * basePlaneCost
        | Mc202 -> 1.33f * basePlaneCost
        | Ju87 -> 2.0f * basePlaneCost
        | Hs129b2
        | Bf110g
        | Bf110e -> (7.5f / 3.0f) * basePlaneCost
        | He111h6
        | He111h16
        | Ju88a4 -> (10.0f / 3.0f) * basePlaneCost
        | Ju52 -> (8.0f / 3.0f) * basePlaneCost
        | I16 -> basePlaneCost
        | IL2M42
        | IL2M43
        | IL2M41 -> 2.0f * basePlaneCost
        | Lagg3s29
        | Yak1s69
        | Yak1s127
        | Yak7bs36
        | La5
        | La5fns2
        | P39
        | SpitfireMkVb
        | Mig3 -> (5.0f / 3.0f) * basePlaneCost
        | P40 -> basePlaneCost
        | A20
        | Pe2s87
        | Pe2s35 -> (7.5f / 3.0f) * basePlaneCost
        | P38 | P51 | P47 | SpitfireMkIXe | Tempest -> 2.0f * basePlaneCost
        | U2 -> 0.5f * basePlaneCost

    member this.BombCapacity =
        match this.BombLoads with
        | [] ->0.0f<K>
        | x -> x |> Seq.map snd |> Seq.max

    /// The mod mask and payload ID suitable for ground attack
    member this.AttackPayload =
        let modmask, payload =
            match this with
            | FokkerD7 -> 1, 1
            | FokkerD7f -> 1, 1
            | FokkerDr1 -> 1, 1
            | Pfalzd3a -> 1, 1
            | SopCamel -> 1, 2 // 20lb-4
            | SopDolphin -> 65, 2 // 20lb-4
            | Spad13 -> 33, 3 // 20lb-2
            | HalberstadtCl2
            | HalberstadtCl2Au -> 129, 2 // Bombs
            | BristolF2Bf2
            | BristolF2Bf3 -> 65, 2 // Bombs
            | Bf109e7 -> 5, 2
            | Bf109f2 | Bf109f4 | Bf109g2 | Bf109g4 | Bf109g6-> 17, 2 // SC250-1
            | Bf109g14 -> 17, 2 // SD250-1
            | Bf109k4 -> 9, 2 // SC500-1
            | Fw190a3 -> 9, 3 // SC500-1
            | Fw190a8 -> 101, 43 // SC250-3
            | Fw190a5 -> 73, 9 // "0,1-MG17-AP-1800 + 2,3-MG15120-APHE-500 + SC500-1 + SC50-4"
            | Fw190d9 -> 9, 3 // SC500-1
            | I16 -> 9, 2 // "0,1-SHKAS-AP-1000 + 2,3-SHKAS-AP-1800 + FAB100M-2"
            | La5 -> 37, 5 // "0,1-SHVAK-AP-340 + FAB100M-2"
            | Lagg3s29 -> 17, 14 // "0-UB-APHE-200 + 1-SHVAK-APHE-160 + FAB100M-2"
            | Mc202 -> 9 , 2 // "0,1-BREDA12-APHE-800 + T100-2"
            | Mig3 -> 5, 6 // "0,1-SHKAS-AP-1500 + 2-BS-APHE-300 + FAB100M-2"
            | P38 -> 17, 8 // M8-6
            | P39 -> 5, 12 // FAB250
            | P40 -> 33, 8 // "0,1,2,3,4,5-M250-AP-1410 + FAB500M-1"
            | P47 -> 17, 24 // M65-2
            | P51 -> 33, 12 // M8-6
            | Yak1s127 -> 5, 2 // "0-UB-APHE-220 + 1-SHVAK-APHE-140 + FAB100M-2"
            | Yak1s69 -> 17, 10 // "0,1-SHKAS-AP-1500 + 2-SHVAK-APHE-120 + FAB100M-2"
            | Yak7bs36 -> 5, 2 // "0-UB-APHE-260 + 1-UB-APHE-140 + 2-SHVAK-APHE-120 + FAB100M-2"
            | Ju87 -> 1, 5 // "0,1-MG17-AP-2000 + SC250-3"
            | Bf110e -> 1, 2 // "0,1,2,3-MG17-AP-4000 + 4,5-MGFF-APHE-360 + SC250-2 + SC50-4"
            | Bf110g -> 1, 2 //  "0,1,2,3-MG17-AP-4000 + 4-MG15120-APHE-400 + 5-MG15120-APHE-350 + SC250-2 + SC50-4"
            | IL2M41 -> 1, 32 // "0,1-SHKAS-AP-1500 + 2,3-SHVAK-APHE-420 + FAB100M-4 + ROS82-8"
            | IL2M42 -> 33, 44 // "0,1-SHKAS-AP-1500 + 2,3-SHVAK-APHE-500 + FAB100M-4 + ROS82-8"
            | IL2M43 -> 33, 41 // "0,1-SHKAS-AP-1500 + 2,3-SHVAK-APHE-500 + FAB100M-4 + ROS82-4"
            | Pe2s35 -> 5, 5 // "0-SHKAS-AP-450 + 1-UB-APHE-150 + FAB250SV-4"
            | Pe2s87 -> 5, 5
            | A20 -> 5, 4
            | Hs129b2 -> 1, 4 // "0,1-MG17-AP-2000 + 2,3-MG15115-APHE-500 + SC250-1 + SC50-2"
            | SpitfireMkIXe -> 7, 3 // GPB500-1 + GPB250-2
            | U2 -> 13, 11 // FAB250SV-2 + FAB100M-2
            | AlbatrosD5
            | SpitfireMkVb
            | SpitfireMkIXe
            | Tempest
            | La5fns2
            | Ju88a4
            | Ju52
            | He111h6
            | He111h16 -> 1, 0
        modmask, payload

    /// The mod mask and payload ID suitable for fighter patrols
    member this.FighterPayload =
        match this with
        | U2 -> 5, 6 // "0-SHKAS-AP-500"
        | _ -> 1, 0

    /// The mod mask and payload ID suitable for level-bombing
    member this.BomberPayLoad =
        match this with
        | PlaneModel.He111h6 -> 1, 3
        | PlaneModel.He111h16 -> 1, 3
        | PlaneModel.Ju88a4 -> 1, 4
        | PlaneModel.Pe2s35 -> 1, 2
        | PlaneModel.Pe2s87 -> 1, 2
        | _ -> 1, 0

    /// The mod mask and payload ID suitable for cargo transport
    member this.CargoPayload =
        match this with
        | PlaneModel.Ju52 -> 1, 0
        | _ -> 1, 0

    /// The mod mask and payload ID suitable for a given role
    member this.PayloadForRole(role) =
        match role with
        | Patroller
        | Interceptor -> this.FighterPayload
        | GroundAttacker -> this.AttackPayload
        | LevelBomber -> this.BomberPayLoad
        | CargoTransporter -> this.CargoPayload

    /// Capacity of transporters
    member this.CargoCapacity =
        match this with
        | Ju52 -> 2300.0f<K>
        | _ -> this.BombCapacity

    member this.Coalition =
        match this with
        | FokkerD7
        | FokkerD7f
        | FokkerDr1
        | Pfalzd3a
        | AlbatrosD5
        | HalberstadtCl2
        | HalberstadtCl2Au -> Axis
        | SopCamel
        | SopDolphin
        | Spad13
        | BristolF2Bf2
        | BristolF2Bf3 -> Allies
        | Bf109e7
        | Bf109f2
        | Bf109f4
        | Bf109g2
        | Bf109g4
        | Bf109g6
        | Bf109g14
        | Bf109k4
        | Fw190a3
        | Fw190a5
        | Fw190a8
        | Fw190d9
        | Mc202
        | Bf110e
        | Bf110g
        | Ju87
        | He111h6
        | He111h16
        | Ju88a4
        | Hs129b2
        | Ju52 -> Axis
        | I16
        | IL2M41
        | IL2M42
        | IL2M43
        | La5
        | La5fns2
        | Mig3
        | Yak1s69
        | Yak1s127
        | Yak7bs36
        | Lagg3s29
        | SpitfireMkVb
        | SpitfireMkIXe
        | Tempest
        | P38
        | P39
        | P40
        | P47
        | P51
        | A20
        | Pe2s87
        | Pe2s35
        | U2 -> Allies

    member this.PlaneType =
        match this with
        | FokkerD7
        | FokkerD7f
        | FokkerDr1
        | Pfalzd3a
        | SopCamel
        | SopDolphin
        | Spad13
        | AlbatrosD5
        | HalberstadtCl2
        | HalberstadtCl2Au
        | BristolF2Bf2
        | BristolF2Bf3 -> Fighter
        | Bf109e7
        | Bf109f2
        | Bf109f4
        | Bf109g2
        | Bf109g4
        | Bf109g6
        | Bf109g14
        | Bf109k4
        | Fw190a3
        | Fw190a5
        | Fw190a8
        | Fw190d9
        | Mc202
        | I16
        | P38
        | P40
        | P47
        | P51
        | La5
        | La5fns2
        | Yak1s69
        | Yak1s127
        | Yak7bs36
        | Lagg3s29
        | SpitfireMkVb
        | SpitfireMkIXe
        | Tempest
        | P39
        | Mig3 -> Fighter
        | Hs129b2
        | Bf110e
        | Bf110g
        | Ju87
        | IL2M42
        | IL2M43
        | IL2M41
        | U2 -> Attacker
        | Ju88a4
        | He111h6
        | He111h16
        | A20
        | Pe2s87
        | Pe2s35 -> Bomber
        | Ju52 -> Transport

    member this.PlaneName =
        match this with
        | FokkerD7 -> "Fokker D.7"
        | FokkerD7f -> "Fokker D.7F"
        | FokkerDr1 -> "Fokker Dr.I"
        | Pfalzd3a -> "Pfalz D.IIIa"
        | SopCamel -> "Sopwith Camel"
        | SopDolphin -> "Sopwith Dolphin"
        | Spad13 -> "Spad 13.C1"
        | AlbatrosD5 -> "Albatros D5"
        | HalberstadtCl2 -> "Haberstadt CL2"
        | HalberstadtCl2Au -> "Haberstadt CL2 AU"
        | BristolF2Bf2 -> "Bristol F2BF2"
        | BristolF2Bf3 -> "Bristol F2BF3"
        | Bf109e7 -> "bf109e7"
        | Bf109f2 -> "bf109f2"
        | Bf109f4 -> "bf109f4"
        | Bf109g2 -> "bf109g2"
        | Bf109g4 -> "bf109g4"
        | Bf109g6 -> "bf109g6"
        | Bf109g14 -> "bf109g14"
        | Bf109k4 -> "bf109k4"
        | Fw190a3 -> "fw190a3"
        | Fw190a5 -> "fw190a5"
        | Fw190a8 -> "fw190a8"
        | Fw190d9 -> "fw190d9"
        | Mc202 -> "mc202"
        | Bf110e -> "bf110e"
        | Bf110g -> "bf110g"
        | Ju87 -> "ju87"
        | Ju88a4 -> "ju88"
        | Ju52 -> "ju52"
        | He111h6 -> "he111h6"
        | He111h16 -> "he111h16"
        | Hs129b2 -> "hs129b2"
        | I16 -> "i16"
        | IL2M41 -> "il2mod41"
        | IL2M42 -> "il2mod42"
        | IL2M43 -> "il2mod43"
        | La5 -> "la5s8"
        | La5fns2 -> "la5fns2"
        | Lagg3s29 -> "lagg3s29"
        | Mig3 -> "mig3"
        | P38 -> "p38"
        | P39 -> "p39"
        | P40 -> "p40"
        | P47 -> "p47"
        | P51 -> "p51"
        | A20 -> "a20"
        | Pe2s35 -> "pe2s35"
        | Pe2s87 -> "pe2s87"
        | Yak1s69 -> "yak1s69"
        | Yak1s127 -> "yak1s127"
        | Yak7bs36 -> "yak7bs36"
        | SpitfireMkVb -> "spitfireMkVb"
        | SpitfireMkIXe -> "spitfireMkIXe"
        | Tempest -> "Tempest MkV s2"
        | U2 -> "u2vs"

    /// <summary>
    /// Substring of the TYPE: field in the mission log, in lower case
    /// </summary>
    member this.MissionLogName =
        match this with
        | AlbatrosD5 -> "albatros d.v"
        | FokkerD7 -> "fokker d.vii"
        | FokkerD7f -> "fokker d.viif"
        | FokkerDr1 -> "fokker dr.i"
        | Pfalzd3a -> "pfalz d.iiia"
        | Spad13 -> "spad 13.c1"
        | SopCamel -> "sopwith camel"
        | SopDolphin -> "sopwith dolphin"
        | HalberstadtCl2 -> "halberstadt cl.ii"
        | HalberstadtCl2Au -> "halberstadt cl.ii 200hp"
        | BristolF2Bf2 -> "bristol f2b (f.ii)"
        | BristolF2Bf3 -> "bristol f2b (f.iii)"
        | Bf109e7 -> "bf 109 e-7"
        | Bf109f2 -> "bf 109 f-2"
        | Bf109f4 -> "bf 109 f-4"
        | Bf109g2 -> "bf 109 g-2"
        | Bf109g4 -> "bf 109 g-4"
        | Bf109g6 -> "bf 109 g-6"
        | Bf109g14 -> "bf 109 g-14"
        | Bf109k4 -> "bf 109 k-4"
        | Fw190a3 -> "fw 190 a-3"
        | Fw190a5 -> "fw 190 a-5"
        | Fw190a8 -> "fw 190 a-8"
        | Fw190d9 -> "fw 190 d-9"
        | Mc202 -> "mc.202"
        | Bf110e -> "bf 110 e"
        | Bf110g -> "bf 110 g"
        | Ju87 -> "ju 87"
        | Ju88a4 -> "ju 88"
        | Ju52 -> "ju 52"
        | He111h6 -> "he 111 h-6"
        | He111h16 -> "he 111 h-16"
        | Hs129b2 -> "hs 129 b-2"
        | I16 -> "i-16"
        | IL2M41 -> "il-2 mod.1941"
        | IL2M42 -> "il-2 mod.1942"
        | IL2M43 -> "il-2 mod.1943"
        | La5 -> "la-5 ser.8"
        | La5fns2 -> "la-5fn ser.2"
        | Lagg3s29 -> "lagg-3 ser.29"
        | Mig3 -> "mig-3"
        | P38 -> "p38-j-25"
        | P39 -> "p-39"
        | P40 -> "p-40"
        | P47 -> "p-47d-28"
        | P51 -> "p-51d-15"
        | A20 -> "a-20"
        | Pe2s35 -> "pe-2 ser.35"
        | Pe2s87 -> "pe-2 ser.87"
        | Yak1s69 -> "yak-1 ser.69"
        | Yak1s127 -> "yak-1b ser.127"
        | Yak7bs36 -> "yak-7b ser.36"
        | SpitfireMkVb -> "spitfire mk.vb"
        | SpitfireMkIXe -> "spitfire mk.ixe"
        | Tempest -> "Tempest Mk.V ser.2"
        | U2 -> "u-2vs"

    member this.BombLoads =
        match this with
        | FokkerD7 -> []
        | FokkerD7f -> []
        | FokkerDr1 -> []
        | Pfalzd3a -> []
        | SopCamel -> [(2, 36.0f<K>); (3, 18.0f<K>)]
        | SopDolphin -> [(2, 36.0f<K>); (3, 18.0f<K>)]
        | Spad13 -> [(3, 18.0f<K>); (4, 9.0f<K>)]
        | AlbatrosD5 -> []
        | HalberstadtCl2
        | HalberstadtCl2Au -> xTimes [0; 5] [(2, 150.0f<K>); (3, 150.0f<K>); (4, 150.0f<K>); (5, 50.0f<K>); (6, 50.0f<K>)]
        | BristolF2Bf2
        | BristolF2Bf3 -> xTimes [0; 4] [(2, 120.0f<K>); (3, 200.0f<K>); (4, 200.0f<K>); (5, 100.0f<K>)]
        | Bf109e7 | Bf109f4 | Bf109g2 | Bf109g4 | Bf109g6 -> [(1, 200.0f<K>); (2, 250.0f<K>)]
        | Bf109f2 -> xTimes [1; 4] [(0, 200.0f<K>); (1, 250.0f<K>)]
        | Bf109g14 -> [(1, 280.0f<K>); (2, 250.0f<K>); (10, 250.0f<K>)]
        | Bf109k4 -> [(1, 250.0f<K>); (2, 500.0f<K>)]
        | Fw190a8 -> xTimes [1; 17] [(0, 280.0f<K>); (1, 250.0f<K>); (2, 250.0f<K>)] @ [(34, 280.0f<K>); (35, 500.0f<K>); (38, 280.0f<K>); (39, 560.0f<K>); (40, 500.0f<K>); (41, 250.0f<K>); (42, 330.0f<K>); (43, 750.0f<K>); (44, 500.0f<K>); (45, 780.0f<K>); (46, 1000.0f<K>)]
        | Mc202 -> [(1, 100.0f<K>); (2, 200.0f<K>)]
        | Fw190a3 -> [(1, 200.0f<K>); (2, 250.0f<K>); (3, 500.0f<K>)]
        | Fw190a5 -> [(1, 200.0f<K>); (2, 250.0f<K>); (3, 500.0f<K>); (6, 200.0f<K>); (7, 400.0f<K>); (8, 450.0f<K>); (9, 700.0f<K>)]
        | Fw190d9 -> [(1, 180.0f<K>); (2, 250.0f<K>); (3, 500.0f<K>)]
        | Bf110e -> [(1, 500.0f<K>); (2, 700.0f<K>); (3, 600.0f<K>); (4, 1000.0f<K>); (5, 1200.0f<K>); (6, 1000.0f<K>); (7, 1250.0f<K>); (8, 1200.0f<K>)]
        | Bf110g -> [(1, 500.0f<K>); (2, 700.0f<K>); (3, 600.0f<K>); (4, 1000.0f<K>); (5, 1200.0f<K>); (6, 1000.0f<K>); (7, 1250.0f<K>); (8, 1200.0f<K>); (12, 200.0f<K>); (14, 200.0f<K>)]
        | Ju87 -> [(1, 530.0f<K>); (2, 500.0f<K>); (3, 780.0f<K>); (4, 1000.0f<K>); (5, 750.0f<K>); (6, 1000.0f<K>); (7, 1800.0f<K>)]
        | Ju88a4 -> [(0, 1000.0f<K>); (1, 1500.0f<K>); (2, 1400.0f<K>); (3, 2200.0f<K>); (4, 2400.0f<K>); (5, 2900.0f<K>); (6, 2000.0f<K>); (7, 2500.0f<K>); (8, 2900.0f<K>); (9, 2000.0f<K>); (10, 1800.0f<K>); (11, 2800.0f<K>)]
        | Ju52 -> [(1, 2500.0f<K>)]
        | He111h6 -> [(0, 800.0f<K>); (1, 1000.0f<K>); (2, 1300.0f<K>); (3, 1500.0f<K>); (4, 2000.0f<K>); (5, 1800.0f<K>); (6, 2000.0f<K>); (7, 3600.0f<K>); (8, 2600.0f<K>); (9, 2800.0f<K>); (10, 2800.0f<K>); (11, 2500.0f<K>); (12, 3500.0f<K>)]
        | He111h16 -> [(0, 800.0f<K>); (1, 1600.0f<K>); (2, 1000.0f<K>); (3, 2000.0f<K>); (4, 1800.0f<K>); (5, 1300.0f<K>); (6, 1500.0f<K>); (7, 1000.0f<K>); (8, 1800.0f<K>); (9, 2000.0f<K>); (10, 2000.0f<K>); (11, 2600.0f<K>); (12, 2800.0f<K>); (13, 3600.0f<K>); (14, 2800.0f<K>); (15, 2500.0f<K>); (16, 3300.0f<K>); (17, 3500.0f<K>); (18, 3500.0f<K>)]
        | Hs129b2 -> xTimes [0; 17] [(1, 200.0f<K>); (2, 300.0f<K>); (3, 250.0f<K>); (4, 350.0f<K>); (6, 100.0f<K>)] @ times 2 [(9, 100.0f<K>)] @ times 2 [(26, 100.0f<K>)] @ times 3 [(14, 100.0f<K>)] @ times 3 [(31, 100.0f<K>)]
        | I16 -> [(1, 100.0f<K>); (2, 200.0f<K>); (12, 100.0f<K>); (13, 200.0f<K>)]
        | IL2M41 -> times 4 [(4, 200.0f<K>); (8, 300.0f<K>); (12, 400.0f<K>); (16, 600.0f<K>); (24, 200.0f<K>); (28, 300.0f<K>); (32, 400.0f<K>); (36, 500.0f<K>); (44, 200.0f<K>); (48, 300.0f<K>); (52, 400.0f<K>); (60, 200.0f<K>); (64, 300.0f<K>); (68, 200.0f<K>)]
        | IL2M42 -> vTimes [(7, 7, 200.0f<K>); (14, 4, 300.0f<K>); (18, 4, 400.0f<K>); (22, 3, 200.0f<K>); (25, 4, 600.0f<K>); (36, 4, 200.0f<K>); (40, 4, 300.0f<K>); (44, 4, 400.0f<K>); (48, 4, 500.0f<K>); (59, 4, 200.0f<K>); (63, 4, 300.0f<K>); (67, 4, 400.0f<K>); (75, 4, 200.0f<K>); (79, 4, 300.0f<K>); (84, 4, 200.0f<K>)]
        | IL2M43 ->
            let ptab = 600.0f<K> / 240.0f
            times 7 [(7, 200.0f<K>)] @
            times 4 [(14, 200.0f<K>)] @
            times 4 [(18, 400.0f<K>)] @
            times 2 [(23, 200.0f<K>)] @
            times 4 [(25, 600.0f<K>)] @
            times 4 [(33, 200.0f<K>)] @
            times 4 [(37, 300.0f<K>)] @
            times 4 [(41, 400.0f<K>)] @
            times 4 [(45, 500.0f<K>)] @
            times 4 [(53, 200.0f<K>)] @
            times 4 [(57, 300.0f<K>)] @
            times 4 [(61, 400.0f<K>)] @
            times 4 [(69, 200.0f<K>)] @
            times 4 [(73, 300.0f<K>)] @
            times 4 [(77, 200.0f<K>)] @
            times 16 [(81, 192.0f * ptab)] @
            times 4 [(97, 240.0f * ptab)] @
            times 3 [(101, 120.0f * ptab)]
        | Pe2s35 -> [(1, 400.0f<K>); (2, 600.0f<K>); (3, 500.0f<K>); (4, 1000.0f<K>); (5, 1000.0f<K>); (6, 1000.0f<K>); (8, 400.0f<K>); (9, 600.0f<K>); (10, 500.0f<K>)]
        | Pe2s87 -> [(1, 400.0f<K>); (2, 600.0f<K>); (3, 500.0f<K>); (4, 1000.0f<K>); (5, 1000.0f<K>); (6, 1000.0f<K>); (8, 400.0f<K>); (9, 600.0f<K>); (10, 500.0f<K>)]
        | La5 -> [(1, 100.0f<K>); (2, 200.0f<K>); (4, 100.0f<K>); (5, 200.0f<K>); (7, 100.0f<K>); (8, 200.0f<K>)]
        | La5fns2 -> xTimes [1; 4; 7] [(0, 100.0f<K>); (1, 200.0f<K>)]
        | Lagg3s29 -> vTimes [(7, 7, 100.0f<K>); (14, 7, 200.0f<K>)]
        | Mig3 -> [(5, 100.0f<K>); (6, 200.0f<K>); (13, 100.0f<K>); (14, 200.0f<K>); (21, 100.0f<K>); (22, 200.0f<K>)]
        | P38 -> xTimes [0; 1; 8; 9] [(2, 450.0f<K>); (4, 900.0f<K>)] @ xTimes [0; 1] [(6, 1800.0f<K>)] @ xTimes [0; 1] [(14, 1200.0f<K>); (16, 1800.0f<K>)]
        | P39 -> times 6 [(6, 100.0f<K>)] @ times 6 [(12, 250.0f<K>)]
        | P40 -> times 4 [(4, 250.0f<K>); (8, 500.0f<K>); (28, 250.0f<K>); (32, 500.0f<K>)]
        | P47 -> times 6 [(6, 225.0f<K>)] @ times 6 [(12, 450.0f<K>)] @ times 6 [(18, 675.0f<K>)] @ times 6 [(24, 950.0f<K>)] @ times 6 [(30, 1175.0f<K>)] |> xTimes  [0; 36]
        | P51 -> xTimes [0; 1; 2; 3] [(4, 450.0f<K>); (8, 900.0f<K>)] @ [(16, 450.0f<K>)]
        | SpitfireMkIXe -> [(1, 225.0f<K>); (2, 225.0f<K>); (3, 900.0f<K>); (6, 225.0f<K>); (7, 255.0f<K>)]
        | Yak1s69 -> [(9, 100.0f<K>); (10, 200.0f<K>)]
        | Yak1s127 -> [(1, 100.0f<K>); (2, 200.0f<K>)]
        | Yak7bs36 -> [(1, 100.0f<K>); (2, 200.0f<K>)]
        | SpitfireMkVb -> [(0, 0.0f<K>); (1, 0.0f<K>)]
        | Tempest -> [(1, 450.0f<K>); (2, 900.0f<K>)]
        | A20 -> [(1, 800.0f<K>); (2, 1600.0f<K>); (3, 2000.0f<K>); (4, 1000.0f<K>); (5, 1800.0f<K>)]
        | U2 -> [(1, 100.0f<K>); (2, 200.0f<K>); (3, 300.0f<K>); (4, 200.0f<K>); (5, 300.0f<K>); (7, 100.0f<K>); (8, 200.0f<K>); (9, 300.0f<K>); (10, 200.0f<K>); (11, 300.0f<K>)]
        |> List.sortBy fst

    member this.SpecialLoadsCosts =
        // rockets
        let r82 = 1.5f<E>
        let r132 = 4.2f<E>
        let wgr42 = 11.2f<E>
        let rp3 = 4.3f<E>
        let m8 = 11.6f<E> / 6.0f
        let r4m = 8.7f<E> / 26.0f

        match this with
        | Lagg3s29 -> times 28 [21, 6.0f * r82]
        | Yak1s69 -> times 4 [1, 2.0f * r82] @ times 4 [5, 6.0f * r82]
        | I16 -> xTimes [3; 14] (times 4 [(0, 4.0f * r82); (4, 6.0f * r82)])
        | Mig3 -> xTimes [1; 9; 17] (times 4 [0, 6.0f * r82])
        | P38 -> times 6 [(8, 6.0f * m8)]
        | P40 -> times 24 [12, 4.0f * r82]
        | P47 -> times 36 [36, 6.0f * m8]
        | P51 -> times 8 [(12, 6.0f * m8)]
        | IL2M41 -> xTimes [20; 40] (times 16 [0, 8.0f * r82]) @ times 16 [56, 8.0f * r132]
        | IL2M42 -> xTimes [29; 52] (times 19 [0, 8.0f * r82]) @ times 16 [71, 8.0f * r132]
        | IL2M43 -> xTimes [29; 49] (times 16 [0, 4.0f * r82]) @ times 16 [65, 4.0f * r132] @ times 8 [85, 4.0f * r82] @ times 4 [93, 4.0f * r132]
        | Pe2s35 | Pe2s87 -> times 4 [7, 10.0f * r132]
        | Fw190a8 -> [(4, 2.0f * wgr42)]
        | Fw190d9 -> [(4, 2.0f * wgr42); (8, 26.0f * r4m)]
        | SpitfireMkIXe -> times 4 [(4, 2.0f * rp3)]
        | Bf109g14 -> [(4, 2.0f * wgr42); (12, 2.0f * wgr42)]
        | _ -> []

    member this.GetPayLoadCost(payload, bombCost) =
        let bombLoadWeight =
            this.BombLoads
            |> List.tryPick (fun (loadout, weight) -> if loadout = payload then Some weight else None)
            |> Option.defaultVal 0.0f<K>
        let loadoutCost =
            this.SpecialLoadsCosts
            |> List.tryPick (fun (loadout, cost) -> if loadout = payload then Some cost else None)
            |> Option.defaultValue 0.0f<E>
        let cost = bombLoadWeight * bombCost + loadoutCost
        cost

    member this.EmptyPayload =
        match this with
        | FokkerD7 -> 2
        | FokkerDr1 -> 2
        | FokkerD7f -> 2
        | Pfalzd3a -> 2
        | SopCamel -> 4
        | SopDolphin -> 13
        | Spad13 -> 7
        | AlbatrosD5 -> 3
        | HalberstadtCl2
        | HalberstadtCl2Au -> 12
        | BristolF2Bf2
        | BristolF2Bf3 -> 10
        | Lagg3s29 -> 49
        | Yak1s69 -> 11
        | Yak1s127 -> 3
        | Yak7bs36 ->3
        | La5 -> 9
        | La5fns2 -> 9
        | I16 -> 22
        | Mig3 -> 24
        | P38 -> 18
        | P39 -> 18
        | P40 -> 36
        | P47 -> 79
        | P51 -> 28
        | SpitfireMkVb -> 1
        | SpitfireMkIXe -> 8
        | Tempest -> 3
        | IL2M41 -> 72
        | IL2M42 ->87
        | IL2M43 -> 104
        | A20 -> 6
        | Pe2s35 -> 11
        | Pe2s87 -> 11
        | Bf109e7 -> 3
        | Bf109f2 -> 6
        | Bf109f4 -> 5
        | Bf109g2 -> 4
        | Bf109g4 -> 4
        | Bf109g6 -> 8
        | Bf109g14 -> 16
        | Bf109k4 -> 4
        | Fw190a3 -> 6
        | Fw190a5 -> 10
        | Fw190a8 -> 48
        | Fw190d9 -> 9
        | Mc202 -> 5
        | Bf110e -> 9
        | Bf110g -> 15
        | Ju87 -> 11
        | Hs129b2 -> 34
        | He111h6 -> 13
        | He111h16 -> 19
        | Ju88a4 -> 12
        | Ju52 -> 3
        | U2 -> 0

    member this.Roles =
        match this with
        | AlbatrosD5
        | FokkerD7
        | FokkerD7f
        | FokkerDr1
        | Pfalzd3a -> [ Patroller ]
        | SopCamel
        | SopDolphin
        | Spad13
        | HalberstadtCl2
        | HalberstadtCl2Au
        | BristolF2Bf2 
        | BristolF2Bf3 -> [ GroundAttacker; Patroller ]
        | Bf109e7 -> [ GroundAttacker; Patroller ]
        | Bf109f2
        | Bf109f4
        | Bf109g2
        | Bf109g4 
        | Bf109g6
        | Bf109g14
        | Bf109k4 -> [ GroundAttacker; Interceptor ; Patroller ]
        | Fw190a3
        | Fw190a5
        | Fw190a8
        | Fw190d9 -> [ GroundAttacker; Interceptor ]
        | Mc202 -> [ Patroller ]
        | Bf110e
        | Bf110g -> [ GroundAttacker; Interceptor ]
        | Ju88a4 -> [ LevelBomber ]
        | Ju52 -> [ CargoTransporter ]
        | I16 -> [ Patroller ]
        | IL2M41
        | IL2M42
        | IL2M43 -> [ GroundAttacker ]
        | Mig3 -> [ Interceptor ]
        | P38
        | P39
        | P40
        | P47
        | P51 -> [ GroundAttacker; Interceptor ]
        | Yak1s69 
        | Yak1s127
        | Yak7bs36
        | La5
        | La5fns2
        | SpitfireMkVb
        | Lagg3s29
        | Tempest -> [ Interceptor ; Patroller ]
        | SpitfireMkIXe -> [ GroundAttacker; Interceptor ; Patroller ]
        | Ju87 -> [ GroundAttacker ]
        | A20
        | Pe2s35
        | Pe2s87 -> [ GroundAttacker; LevelBomber ]
        | Hs129b2 -> [ GroundAttacker ]
        | He111h6
        | He111h16 -> [ LevelBomber ]
        | U2 -> [ GroundAttacker ]
        |> Set.ofList

    static member AllModels =
        [ Bf109e7
          Bf109f2
          Bf109f4
          Bf109g2
          Bf109g4
          Bf109g6
          Bf109g14
          Bf109k4
          Fw190a3
          Fw190a5
          Fw190a8
          Fw190d9
          Mc202
          Bf110e
          Bf110g
          Ju88a4
          Ju52
          I16
          IL2M41
          IL2M42
          IL2M43
          Mig3
          P38
          P39
          P40
          P47
          P51
          Yak1s69
          Yak1s127
          Yak7bs36
          La5
          La5fns2
          Lagg3s29
          SpitfireMkVb
          SpitfireMkIXe
          Tempest
          A20
          Pe2s35
          Pe2s87
          Ju87
          He111h6
          He111h16
          Hs129b2
          U2
          FokkerD7
          FokkerD7f
          FokkerDr1
          Pfalzd3a
          SopCamel
          SopDolphin
          Spad13
          AlbatrosD5
          HalberstadtCl2
          HalberstadtCl2Au
          BristolF2Bf2
          BristolF2Bf3
        ]

    static member PlaneTypeShares(coalition) =
        match coalition with
        | Axis -> [ 0.6f; 0.2f; 0.15f; 0.05f ]
        | Allies -> [ 0.5f; 0.3f; 0.2f; 0.0f ]
        |> List.zip [ Fighter; Attacker; Bomber; Transport ]
        |> Map.ofList

let tryGetPlaneByName name =
    PlaneModel.AllModels
    |> List.tryFind (fun plane -> plane.PlaneName = name)