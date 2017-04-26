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

/// Various kind of planes used in the 1941/42 Moscow theater
type PlaneModel =
    | Bf109e7
    | Bf109f2
    | Mc202
    | Bf110e
    | Ju88a4
    | Ju52
    | I16
    | IL2M41
    | Mig3
    | P40
    | Pe2s35
    | Ju87
    | He111h6
with
    member this.ScriptModel =
        match this with
        | Bf109e7 -> Vehicles.germanFighter1
        | Bf109f2 -> Vehicles.germanFighter2
        | Mc202 -> Vehicles.germanFighter3
        | Bf110e -> Vehicles.germanAttacker1
        | Ju88a4 -> Vehicles.germanBomber1
        | Ju52 -> Vehicles.germanTransport
        | I16 -> Vehicles.russianFighter1
        | IL2M41 -> Vehicles.russianAttacker1
        | Mig3 -> Vehicles.russianFighter2
        | P40 -> Vehicles.russianFighter3
        | Pe2s35 -> Vehicles.russianBomber1
        | Ju87 -> Vehicles.germanAttacker2
        | He111h6 -> Vehicles.germanBomber2

    member this.StaticScriptModel =
        match this with
        | Bf109e7 -> Vehicles.germanStaFighter1
        | Bf109f2 -> Vehicles.germanStaFighter2
        | Mc202 -> Vehicles.germanStaFighter3
        | Bf110e -> Vehicles.germanStaAttacker
        | Ju88a4 -> Vehicles.germanStaBomber
        | Ju52 -> Vehicles.germanStaTransport
        | I16 -> Vehicles.russianStaFighter1
        | IL2M41 -> Vehicles.russianStaAttacker
        | Mig3 -> Vehicles.russianStaFighter2
        | P40 -> Vehicles.russianStaFighter3
        | Pe2s35 -> Vehicles.russianStaBomber
        | Ju87 -> Vehicles.germanStaJu87
        | He111h6 -> Vehicles.germanStaHe111h6

    member this.Cost =
        match this with
        | Bf109e7 -> basePlaneCost
        | Bf109f2 -> (5.0f / 3.0f) * basePlaneCost
        | Mc202 -> 1.33f * basePlaneCost
        | Ju87 -> 2.0f * basePlaneCost
        | Bf110e -> (7.5f / 3.0f) * basePlaneCost
        | He111h6
        | Ju88a4 -> (10.0f / 3.0f) * basePlaneCost
        | Ju52 -> (8.0f / 3.0f) * basePlaneCost
        | I16 -> basePlaneCost
        | IL2M41 -> 2.0f * basePlaneCost
        | Mig3 -> (5.0f / 3.0f) * basePlaneCost
        | P40 -> basePlaneCost
        | Pe2s35 -> (7.5f / 3.0f) * basePlaneCost

    member this.BombCapacity =
        match this with
        | Bf109e7 
        | Bf109f2
        | Mc202 -> 500.0f<K>
        | Bf110e -> 1250.0f<K>
        | He111h6 -> 3600.0f<K>
        | Ju87 -> 1800.0f<K>
        | Ju88a4 -> 2800.0f<K>
        | Ju52 -> 0.0f<K>
        | I16 -> 200.0f<K>
        | IL2M41 -> 600.0f<K>
        | Mig3 -> 200.0f<K>
        | P40 -> 500.0f<K>
        | Pe2s35 -> 1000.0f<K>

    member this.Coalition =
        match this with
        | Bf109e7 
        | Bf109f2
        | Mc202
        | Bf110e
        | Ju87
        | He111h6
        | Ju88a4
        | Ju52 -> Axis
        | I16
        | IL2M41
        | Mig3
        | P40
        | Pe2s35 -> Allies

    member this.PlaneType =
        match this with
        | Bf109e7 
        | Bf109f2
        | Mc202
        | I16
        | P40
        | Mig3 -> Fighter
        | Bf110e
        | Ju87
        | IL2M41 -> Attacker
        | Ju88a4
        | He111h6
        | Pe2s35 -> Bomber
        | Ju52 -> Transport

    member this.PlaneName =
        match this with
        | Bf109e7 -> "bf109e7"
        | Bf109f2 -> "bf109f2"
        | Mc202 -> "mc202"
        | Bf110e -> "bf110e"
        | Ju87 -> "ju87"
        | Ju88a4 -> "ju88"
        | Ju52 -> "ju52"
        | He111h6 -> "he111h6"
        | I16 -> "i16"
        | IL2M41 -> "il2mod41"
        | Mig3 -> "mig3"
        | P40 -> "p40"
        | Pe2s35 -> "pe2s35"

    static member AllModels =
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

    static member RandomPlaneOfType(typ : PlaneType, coalition : CoalitionId) =
        PlaneModel.AllModels
        |> Seq.filter (fun model -> model.PlaneType = typ && model.Coalition = coalition)
        |> Array.ofSeq
        |> Array.shuffle (System.Random())
        |> Seq.tryHead

type PlaneType
with
    member this.Random(coalition) = PlaneModel.RandomPlaneOfType(this, coalition)
