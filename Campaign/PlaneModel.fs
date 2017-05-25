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
        | Bf109e7 -> Vehicles.germanFighter1
        | Bf109f2 -> Vehicles.germanFighter2
        | Mc202 -> Vehicles.germanFighter3
        | Bf109f4 -> Vehicles.germanFighter4
        | Bf109g2 -> Vehicles.germanFighter5
        | Fw190a3 -> Vehicles.germanFighter6
        | Fw190a5 -> Vehicles.germanFighter7
        | Bf109g4 -> Vehicles.germanFighter8
        | Bf110e -> Vehicles.germanAttacker1
        | Bf110g -> Vehicles.germanAttacker3
        | Ju88a4 -> Vehicles.germanBomber1
        | Ju52 -> Vehicles.germanTransport
        | I16 -> Vehicles.russianFighter1
        | IL2M41 -> Vehicles.russianAttacker1
        | IL2M42 -> Vehicles.russianAttacker2
        | IL2M43 -> Vehicles.russianAttacker3
        | Mig3 -> Vehicles.russianFighter2
        | P40 -> Vehicles.russianFighter3
        | Yak1s69 -> Vehicles.russianFighter4
        | Yak1s127 -> Vehicles.russianFighter7
        | La5 -> Vehicles.russianFighter6
        | Lagg3s29 -> Vehicles.russianFighter5
        | Pe2s35 -> Vehicles.russianBomber1
        | Pe2s87 -> Vehicles.russianBomber2
        | Ju87 -> Vehicles.germanAttacker2
        | He111h6 -> Vehicles.germanBomber2
        | He111h16 -> Vehicles.germanBomber3

    member this.StaticScriptModel =
        match this with
        // Moscow planeset
        | Bf109e7 -> Vehicles.germanStaBf109e7
        | Bf109f2 -> Vehicles.germanStaBf109e7Net
        | Mc202 -> Vehicles.germanStaBf109e7Open
        | Bf110e -> Vehicles.germanStaAttacker
        | Ju88a4 -> Vehicles.germanStaBomber
        | Ju52 -> Vehicles.germanStaTransport
        | I16 -> Vehicles.russianStaI16
        | IL2M41 -> Vehicles.russianStaAttacker
        | Mig3 -> Vehicles.russianStaMig3
        | P40 -> Vehicles.russianStaMig3Net
        | Pe2s35 -> Vehicles.russianStaBomber
        | Ju87 -> Vehicles.germanStaJu87
        | He111h6 -> Vehicles.germanStaHe111h6
        // VLuki planeset
        | Bf109f4 -> Vehicles.germanStaBf109
        | Bf109g2 -> Vehicles.germanStaBf109Net
        | Fw190a3 -> Vehicles.germanStaBf109e7
        | Bf110g -> Vehicles.germanStaAttacker
        | He111h16 -> Vehicles.germanStaHe111h6
        | Yak1s69 -> Vehicles.russianStaYak1
        | Lagg3s29 -> Vehicles.russianStaLagg3
        | IL2M42 -> Vehicles.russianStaAttacker
        | Pe2s87 -> Vehicles.russianStaBomber
        // Other
        | IL2M43 -> Vehicles.russianStaAttacker
        | Bf109g4 -> Vehicles.germanStaBf109e7Net
        | Fw190a5 -> Vehicles.germanStaBf109e7Open
        | La5 -> Vehicles.russianStaLagg3W1
        | Yak1s127 -> Vehicles.russianStaYak1Net

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
            [ Bf109f4
              Bf109g2
              Fw190a3
              Bf110g
              Ju87
              Ju88a4
              He111h16
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
