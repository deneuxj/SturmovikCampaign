module Campaign.PlaneModel

open SturmovikMission.Blocks

open Campaign.BasicTypes
open Util

type PlaneType  =
    | Fighter
    | Attacker
    | Bomber
    | Transport

type PlaneRole =
    | Interceptor
    | Patroller
    | GroundAttacker
    | LevelBomber
    | CargoTransporter

let private basePlaneCost = 500.0f<E>

type PlaneSet =
    | Moscow
    | Stalingrad
    | VelikieLuki
    | EarlyAccess
    | All

/// Various kind of planes used in the 1941/42 Moscow theater
type PlaneModel =
    | Bf109e7
    | Bf109f2
    | Bf109f4
    | Bf109g2
    | Bf109g4
    | Fw190a3
    | Fw190a5
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
    | P40
    | Yak1s69
    | Yak1s127
    | La5
    | Lagg3s29
    | SpitfireMkVb
    | Pe2s35
    | Pe2s87
    | Ju87
    | He111h6
    | He111h16
    | Hs129b2
with
    member this.ScriptModel =
        match this with
        | Bf109e7 -> Vehicles.vehicles.GermanFighter1
        | Bf109f2 -> Vehicles.vehicles.GermanFighter2
        | Mc202 -> Vehicles.vehicles.GermanFighter3
        | Bf109f4 -> Vehicles.vehicles.GermanFighter4
        | Bf109g2 -> Vehicles.vehicles.GermanFighter5
        | Fw190a3 -> Vehicles.vehicles.GermanFighter6
        | Fw190a5 -> Vehicles.vehicles.GermanFighter7
        | Bf109g4 -> Vehicles.vehicles.GermanFighter8
        | Bf110e -> Vehicles.vehicles.GermanAttacker1
        | Bf110g -> Vehicles.vehicles.GermanAttacker3
        | Ju88a4 -> Vehicles.vehicles.GermanBomber1
        | Ju52 -> Vehicles.vehicles.GermanTransport
        | I16 -> Vehicles.vehicles.RussianFighter1
        | IL2M41 -> Vehicles.vehicles.RussianAttacker1
        | IL2M42 -> Vehicles.vehicles.RussianAttacker2
        | IL2M43 -> Vehicles.vehicles.RussianAttacker3
        | Mig3 -> Vehicles.vehicles.RussianFighter2
        | P40 -> Vehicles.vehicles.RussianFighter3
        | Yak1s69 -> Vehicles.vehicles.RussianFighter4
        | Yak1s127 -> Vehicles.vehicles.RussianFighter7
        | La5 -> Vehicles.vehicles.RussianFighter6
        | Lagg3s29 -> Vehicles.vehicles.RussianFighter5
        | SpitfireMkVb -> Vehicles.vehicles.RussianFighter8
        | Pe2s35 -> Vehicles.vehicles.RussianBomber1
        | Pe2s87 -> Vehicles.vehicles.RussianBomber2
        | Ju87 -> Vehicles.vehicles.GermanAttacker2
        | He111h6 -> Vehicles.vehicles.GermanBomber2
        | He111h16 -> Vehicles.vehicles.GermanBomber3
        | Hs129b2 -> Vehicles.vehicles.GermanAttacker4

    member this.StaticScriptModel =
        match this with
        // Moscow planeset
        | Bf109e7 -> Vehicles.vehicles.GermanStaBf109e7
        | Bf109f2 -> Vehicles.vehicles.GermanStaBf109e7Net
        | Mc202 -> Vehicles.vehicles.GermanStaBf109e7Open
        | Bf110e -> Vehicles.vehicles.GermanStaAttacker
        | Ju88a4 -> Vehicles.vehicles.GermanStaBomber
        | Ju52 -> Vehicles.vehicles.GermanStaTransport
        | I16 -> Vehicles.vehicles.RussianStaI16
        | IL2M41 -> Vehicles.vehicles.RussianStaAttacker
        | Mig3 -> Vehicles.vehicles.RussianStaMig3
        | P40 -> Vehicles.vehicles.RussianStaMig3Net
        | Pe2s35 -> Vehicles.vehicles.RussianStaBomber
        | Ju87 -> Vehicles.vehicles.GermanStaJu87
        | He111h6 -> Vehicles.vehicles.GermanStaHe111h6
        | SpitfireMkVb -> Vehicles.vehicles.RussianStaLagg3Net
        // VLuki planeset
        | Bf109f4 -> Vehicles.vehicles.GermanStaBf109
        | Bf109g2 -> Vehicles.vehicles.GermanStaBf109Net
        | Fw190a3 -> Vehicles.vehicles.GermanStaBf109e7Open
        | Bf110g -> Vehicles.vehicles.GermanStaAttacker
        | He111h16 -> Vehicles.vehicles.GermanStaHe111h6
        | Yak1s69 -> Vehicles.vehicles.RussianStaYak1
        | Lagg3s29 -> Vehicles.vehicles.RussianStaLagg3
        | IL2M42 -> Vehicles.vehicles.RussianStaAttacker
        | Pe2s87 -> Vehicles.vehicles.RussianStaBomber
        // Other
        | IL2M43 -> Vehicles.vehicles.RussianStaAttacker
        | Bf109g4 -> Vehicles.vehicles.GermanStaBf109e7Net
        | Fw190a5 -> Vehicles.vehicles.GermanStaBf109e7Open
        | La5 -> Vehicles.vehicles.RussianStaLagg3W1
        | Yak1s127 -> Vehicles.vehicles.RussianStaYak1Net
        | Hs129b2 -> Vehicles.vehicles.GermanStaJu87Net

    member this.Cost =
        match this with
        | Bf109e7 -> basePlaneCost
        | Bf109f4
        | Bf109g2
        | Bf109g4
        | Fw190a3
        | Fw190a5
        | Bf109f2 -> (5.0f / 3.0f) * basePlaneCost
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
        | La5
        | SpitfireMkVb
        | Mig3 -> (5.0f / 3.0f) * basePlaneCost
        | P40 -> basePlaneCost
        | Pe2s87
        | Pe2s35 -> (7.5f / 3.0f) * basePlaneCost

    member this.BombCapacity =
        match this with
        | Bf109e7 
        | Bf109f2
        | Bf109f4
        | Bf109g2
        | Bf109g4
        | Fw190a3
        | Fw190a5
        | Mc202 -> 500.0f<K>
        | Bf110g
        | Bf110e -> 1250.0f<K>
        | He111h16
        | He111h6 -> 3600.0f<K>
        | Hs129b2 -> 350.0f<K>
        | Ju87 -> 1800.0f<K>
        | Ju88a4 -> 2800.0f<K>
        | SpitfireMkVb
        | Ju52 -> 0.0f<K>
        | Yak1s69
        | Yak1s127
        | Lagg3s29
        | La5
        | I16 -> 200.0f<K>
        | IL2M42
        | IL2M43
        | IL2M41 -> 600.0f<K>
        | Mig3 -> 200.0f<K>
        | P40 -> 500.0f<K>
        | Pe2s87
        | Pe2s35 -> 1000.0f<K>

    /// The mod mask and payload ID suitable for ground attack
    member this.AttackPayload =
        let modmask, payload =
            match this with
            | PlaneModel.Bf109e7 -> 5, 2
            | PlaneModel.Bf109f2 | PlaneModel.Bf109f4 | PlaneModel.Bf109g2 | PlaneModel.Bf109g4 -> 8, 2 // SC250-1
            | PlaneModel.Fw190a3 -> 9, 3 // SC500-1
            | PlaneModel.Fw190a5 -> 73, 9 // "0,1-MG17-AP-1800 + 2,3-MG15120-APHE-500 + SC500-1 + SC50-4"
            | PlaneModel.I16 -> 9, 2 // "0,1-SHKAS-AP-1000 + 2,3-SHKAS-AP-1800 + FAB100M-2"
            | PlaneModel.La5 -> 37, 5 // "0,1-SHVAK-AP-340 + FAB100M-2"
            | PlaneModel.Lagg3s29 -> 17, 14 // "0-UB-APHE-200 + 1-SHVAK-APHE-160 + FAB100M-2"
            | PlaneModel.Mc202 -> 9 , 2 // "0,1-BREDA12-APHE-800 + T100-2"
            | PlaneModel.Mig3 -> 5, 6 // "0,1-SHKAS-AP-1500 + 2-BS-APHE-300 + FAB100M-2"
            | PlaneModel.P40 -> 33, 8 // "0,1,2,3,4,5-M250-AP-1410 + FAB500M-1"
            | PlaneModel.Yak1s127 -> 5, 2 // "0-UB-APHE-220 + 1-SHVAK-APHE-140 + FAB100M-2"
            | PlaneModel.Yak1s69 -> 17, 10 // "0,1-SHKAS-AP-1500 + 2-SHVAK-APHE-120 + FAB100M-2"
            | PlaneModel.Ju87 -> 1, 5 // "0,1-MG17-AP-2000 + SC250-3"
            | PlaneModel.Bf110e -> 1, 2 // "0,1,2,3-MG17-AP-4000 + 4,5-MGFF-APHE-360 + SC250-2 + SC50-4"
            | PlaneModel.Bf110g -> 1, 2 //  "0,1,2,3-MG17-AP-4000 + 4-MG15120-APHE-400 + 5-MG15120-APHE-350 + SC250-2 + SC50-4"
            | PlaneModel.IL2M41 -> 1, 32 // "0,1-SHKAS-AP-1500 + 2,3-SHVAK-APHE-420 + FAB100M-4 + ROS82-8"
            | PlaneModel.IL2M42 -> 33, 44 // "0,1-SHKAS-AP-1500 + 2,3-SHVAK-APHE-500 + FAB100M-4 + ROS82-8"
            | PlaneModel.IL2M43 -> 33, 41 // "0,1-SHKAS-AP-1500 + 2,3-SHVAK-APHE-500 + FAB100M-4 + ROS82-4"
            | PlaneModel.Pe2s35 -> 5, 5 // "0-SHKAS-AP-450 + 1-UB-APHE-150 + FAB250SV-4"
            | PlaneModel.Hs129b2 -> 1, 4 // "0,1-MG17-AP-2000 + 2,3-MG15115-APHE-500 + SC250-1 + SC50-2"
            | _ -> 1, 0
        modmask, payload

    /// The mod mask and payload ID suitable for fighter patrols
    member this.FighterPayload =
        match this with
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
        | PlaneModel.He111h16 -> 1, 19
        | PlaneModel.He111h6 -> 1, 13
        | PlaneModel.Pe2s35 | PlaneModel.Pe2s87 -> 1, 11
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
        | Bf109e7
        | Bf109f2
        | Bf109f4
        | Bf109g2
        | Bf109g4
        | Fw190a3
        | Fw190a5
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
        | Mig3
        | Yak1s69
        | Yak1s127
        | Lagg3s29
        | SpitfireMkVb
        | P40
        | Pe2s87
        | Pe2s35 -> Allies

    member this.PlaneType =
        match this with
        | Bf109e7 
        | Bf109f2
        | Bf109f4
        | Bf109g2
        | Bf109g4
        | Fw190a3
        | Fw190a5
        | Mc202
        | I16
        | P40
        | La5
        | Yak1s69
        | Yak1s127
        | Lagg3s29
        | SpitfireMkVb
        | Mig3 -> Fighter
        | Hs129b2
        | Bf110e
        | Bf110g
        | Ju87
        | IL2M42
        | IL2M43
        | IL2M41 -> Attacker
        | Ju88a4
        | He111h6
        | He111h16
        | Pe2s87
        | Pe2s35 -> Bomber
        | Ju52 -> Transport

    member this.PlaneName =
        match this with
        | Bf109e7 -> "bf109e7"
        | Bf109f2 -> "bf109f2"
        | Bf109f4 -> "bf109f4"
        | Bf109g2 -> "bf109g2"
        | Bf109g4 -> "bf109g4"
        | Fw190a3 -> "fw190a3"
        | Fw190a5 -> "fw190a5"
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
        | Lagg3s29 -> "lagg3s29"
        | Mig3 -> "mig3"
        | P40 -> "p40"
        | Pe2s35 -> "pe2s35"
        | Pe2s87 -> "pe2s87"
        | Yak1s69 -> "yak1s69"
        | Yak1s127 -> "yak1s127"
        | SpitfireMkVb -> "spitfireMkVb"

    /// <summary>
    /// Substring of the TYPE: field in the mission log
    /// </summary>
    member this.MissionLogName =
        match this with
        | Bf109e7 -> "bf 109 e-7"
        | Bf109f2 -> "bf 109 f-2"
        | Bf109f4 -> "bf 109 f-4"
        | Bf109g2 -> "bf 109 g-2"
        | Bf109g4 -> "bf 109 g-4"
        | Fw190a3 -> "fw 190 a-3"
        | Fw190a5 -> "fw 190 a-5"
        | Mc202 -> "mc 202"
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
        | La5 -> "la-5"
        | Lagg3s29 -> "lagg-3 ser.29"
        | Mig3 -> "mig-3"
        | P40 -> "p-40"
        | Pe2s35 -> "pe-2 ser.35"
        | Pe2s87 -> "pe-2 ser.87"
        | Yak1s69 -> "yak-1 ser.69"
        | Yak1s127 -> "yak-1 ser.127"
        | SpitfireMkVb -> "spitfire mk.vb"

    member this.BombLoads =
        // times 2 [(4, x); (10, y)] -> [(4, x); (5, x); (10, y); (11, y)]
        let times N xs =
            [
                for (n, x) in xs do
                    for c in 0..(N - 1) do
                        yield (n + c, x)
            ]
        // vTimes [(3, 10, x)] -> [(10, x); (11, x); (12, x)]
        let vTimes xs =
            [
                for (n, N, x) in xs do
                    for c in 0..(N - 1) do
                        yield (n + c, x)
            ]
        /// xTimes [10; 20] [(1, x); (2, y)] -> [(11, x); (12, y); (21, x); (22, y)]
        let xTimes offsets xs =
            [
                for offset in offsets do
                    for (n, x) in xs do
                        yield (n + offset, x)
            ]
        match this with
        | Bf109e7 | Bf109f4 | Bf109f2 | Bf109g2 | Bf109g4 -> [(1, 200.0f<K>); (2, 250.0f<K>)]
        | Mc202 -> [(1, 100.0f<K>); (2, 200.0f<K>)]
        | Fw190a3 -> [(1, 200.0f<K>); (2, 250.0f<K>); (3, 500.0f<K>)]
        | Fw190a5 -> [(1, 200.0f<K>); (2, 250.0f<K>); (3, 500.0f<K>); (6, 200.0f<K>); (7, 400.0f<K>); (8, 450.0f<K>); (9, 700.0f<K>)]
        | Bf110e -> [(1, 500.0f<K>); (2, 700.0f<K>); (3, 600.0f<K>); (4, 1000.0f<K>); (5, 1200.0f<K>); (6, 1000.0f<K>); (7, 1250.0f<K>); (8, 1200.0f<K>)]
        | Bf110g -> [(1, 500.0f<K>); (2, 700.0f<K>); (3, 600.0f<K>); (4, 1000.0f<K>); (5, 1200.0f<K>); (6, 1000.0f<K>); (7, 1250.0f<K>); (8, 1200.0f<K>)]
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
        | Lagg3s29 -> vTimes [(7, 7, 100.0f<K>); (14, 7, 200.0f<K>)]
        | Mig3 -> [(5, 100.0f<K>); (6, 200.0f<K>); (13, 100.0f<K>); (14, 200.0f<K>); (21, 100.0f<K>); (22, 200.0f<K>)]
        | P40 -> times 4 [(4, 250.0f<K>); (8, 500.0f<K>); (28, 250.0f<K>); (32, 500.0f<K>)]
        | Yak1s69 -> [(9, 100.0f<K>); (10, 200.0f<K>)]
        | Yak1s127 -> [(1, 100.0f<K>); (2, 200.0f<K>)]
        | SpitfireMkVb -> [(0, 0.0f<K>); (1, 0.0f<K>)]
        |> List.sortBy fst

    member this.BombLoadsOk =
        this.BombLoads
        |> Seq.pairwise
        |> Seq.forall(fun ((l1, w1), (l2, w2)) -> l1 < l2)

    member this.LoadOuts(maxWeight : float32<K>) =
        let rec work (loadout : int) (weights : (int * float32<K>) list) =
            seq {
                match weights with
                | [] ->
                    yield sprintf "%d..999" loadout
                | (loadout2, w) :: rest ->
                    if loadout < loadout2 then
                        if w <= maxWeight then
                            yield! work loadout rest
                        else if loadout + 1 = loadout2 then
                            yield sprintf "%d" loadout
                            yield! work (loadout2 + 1) rest
                        else
                            yield sprintf "%d..%d" loadout (loadout2 - 1)
                            yield! work (loadout2 + 1) rest
                    else if loadout = loadout2 then
                        if w <= maxWeight then
                            yield! work loadout rest
                        else
                            yield! work (loadout + 1) rest
                    else
                        failwith "loadout is larger than loadout2"
            }
        assert(this.BombLoadsOk)
        work 0 this.BombLoads
        |> String.concat "/"

    member this.Roles =
        match this with
        | Bf109e7 -> [ GroundAttacker; Patroller ]
        | Bf109f2
        | Bf109f4
        | Bf109g2
        | Bf109g4 -> [ GroundAttacker; Interceptor ; Patroller ]
        | Fw190a3
        | Fw190a5 -> [ GroundAttacker; Interceptor ]
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
        | P40 -> [ GroundAttacker; Interceptor ]
        | Yak1s69 
        | Yak1s127
        | La5
        | SpitfireMkVb
        | Lagg3s29 -> [ Interceptor ; Patroller ]
        | Ju87 -> [ GroundAttacker ]
        | Pe2s35
        | Pe2s87 -> [ GroundAttacker; LevelBomber; CargoTransporter ]
        | Hs129b2 -> [ GroundAttacker ]
        | He111h6
        | He111h16 -> [ LevelBomber; CargoTransporter ]
        |> Set.ofList

    static member AllModels(planeSet) =
        match planeSet with
        | Moscow ->
            [ Bf109e7
              Bf109f2
              Bf109f4
              Mc202
              Bf110e
              Ju88a4
              Ju87
              Ju52
              He111h6
              I16
              IL2M41
              Mig3
              P40
              Pe2s35 ]
        | Stalingrad ->
            [ Bf109e7
              Bf109f4
              Bf109g2
              Bf110e
              Ju88a4
              Ju87
              Ju52
              He111h6
              Hs129b2
              I16
              IL2M42
              Yak1s69
              Lagg3s29
              P40
              Pe2s87 ]
        | VelikieLuki ->
            [ Bf109e7
              Bf109f4
              Bf109g2
              Fw190a3
              Bf110g
              Ju87
              Ju88a4
              He111h6
              Hs129b2
              Ju52
              Yak1s69
              Lagg3s29
              La5
              I16
              IL2M42
              Pe2s87 ]
        | EarlyAccess ->
            [ Bf109g4
              Bf109g2
              Fw190a5
              Bf110g
              Ju87
              Ju88a4
              He111h16
              Ju52
              Yak1s127
              Lagg3s29
              La5
              P40
              IL2M43
              Pe2s87
              SpitfireMkVb
              Hs129b2 ]
        | All ->
            [ Bf109e7
              Bf109f2
              Bf109f4
              Bf109g2
              Bf109g4
              Fw190a3
              Fw190a5
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
              P40
              Yak1s69
              Yak1s127
              La5
              Lagg3s29
              Pe2s35
              Pe2s87
              Ju87
              He111h6
              He111h16
              Hs129b2
              SpitfireMkVb ]

    static member PlaneTypeShares(coalition) =
        match coalition with
        | Axis -> [ 0.6f; 0.2f; 0.15f; 0.05f ]
        | Allies -> [ 0.5f; 0.3f; 0.2f; 0.0f ]
        |> List.zip [ Fighter; Attacker; Bomber; Transport ]
        |> Map.ofList

    static member AllPlanesOfType(planeSet : PlaneSet, typ : PlaneType, coalition : CoalitionId) =
        PlaneModel.AllModels planeSet
        |> Seq.filter (fun model -> model.PlaneType = typ && model.Coalition = coalition)
        |> Array.ofSeq

    static member RandomPlaneOfType(planeSet : PlaneSet, typ : PlaneType, coalition : CoalitionId) =
        PlaneModel.AllPlanesOfType(planeSet, typ, coalition)
        |> Array.shuffle (System.Random())
        |> Seq.tryHead

    static member RandomPlaneWithRole(planeSet : PlaneSet, role : PlaneRole, coalition : CoalitionId) =
        PlaneModel.AllModels planeSet
        |> Seq.filter (fun model -> model.Roles.Contains(role) && model.Coalition = coalition)
        |> Array.ofSeq
        |> Array.shuffle (System.Random())
        |> Seq.tryHead

type PlaneType
with
    member this.Random(planeSet, coalition) = PlaneModel.RandomPlaneOfType(planeSet, this, coalition)
