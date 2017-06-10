module Campaign.PlaneModel

open SturmovikMission.Blocks

open Campaign.BasicTypes
open Campaign.Util

type PlaneType  =
    | Fighter
    | Attacker
    | Bomber
    | Transport

let private basePlaneCost = 500.0f<E>

type PlaneSet =
    | Moscow
    | VelikieLuki
    | EarlyAccess

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
    | Pe2s35
    | Pe2s87
    | Ju87
    | He111h6
    | He111h16
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
        | Pe2s35 -> Vehicles.vehicles.RussianBomber1
        | Pe2s87 -> Vehicles.vehicles.RussianBomber2
        | Ju87 -> Vehicles.vehicles.GermanAttacker2
        | He111h6 -> Vehicles.vehicles.GermanBomber2
        | He111h16 -> Vehicles.vehicles.GermanBomber3

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
        | Ju87 -> 1800.0f<K>
        | Ju88a4 -> 2800.0f<K>
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
        | Mig3 -> Fighter
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

    static member AllModels(planeSet) =
        match planeSet with
        | Moscow ->
            [ Bf109e7
              Bf109f2
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
        | VelikieLuki ->
            [ Bf109e7
              Bf109f4
              Bf109g2
              Fw190a3
              Bf110g
              Ju87
              Ju88a4
              He111h16
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
              I16
              IL2M43
              Pe2s87 ]


    static member RandomPlaneOfType(planeSet : PlaneSet, typ : PlaneType, coalition : CoalitionId) =
        PlaneModel.AllModels planeSet
        |> Seq.filter (fun model -> model.PlaneType = typ && model.Coalition = coalition)
        |> Array.ofSeq
        |> Array.shuffle (System.Random())
        |> Seq.tryHead

type PlaneType
with
    member this.Random(planeSet, coalition) = PlaneModel.RandomPlaneOfType(planeSet, this, coalition)
