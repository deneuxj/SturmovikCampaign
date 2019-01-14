module SturmovikMission.Blocks.StaticDefenses.Factory

open System.Numerics
open SturmovikMission.Blocks
open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.StaticDefenses.Types
open SturmovikMission.Blocks.Links
open SturmovikMission.Blocks.Predicates
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.WhileEnemyClose

type CanonInstance = CanonInstance of int

type StaticDefenseGroup = {
    CanonSet : Map<CanonInstance, Canon>
    EnemyClose : WhileEnemyClose
    Decorations : Mcu.McuBase list
    Api : Api
}
with
    interface McuUtil.IMcuGroup with
        member this.Content =
            [
                yield! this.Decorations
            ]
        member this.LcStrings = []
        member this.SubGroups =
            [
                yield! this.CanonSet |> Seq.map (fun kvp -> kvp.Value.All)
                yield this.Api.All
                yield this.EnemyClose.All
            ]

    static member Create(settings : CanonGenerationSettings, specialty : DefenseSpecialty, includeSearchLights : bool, random : System.Random, store, lcStore, boundary : Vector2 list, yori : float32, groupSize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue) =
        let center =
            let n = max 1 (List.length boundary)
            let k = 1.0f / float32 n
            let sum =
                boundary
                |> Seq.sum
            k * sum
        let cannonSet =
            let numSearchLights =
                if includeSearchLights then
                    max 1 (groupSize / 10)
                else
                    0
            seq {
                for i in 1 .. groupSize do
                    let isFlak =
                        match specialty with
                        | AntiTank -> false
                        | AntiAirMg -> i <= max (numSearchLights + 1) (groupSize / 4)
                        | AntiAirCanon -> i <= max (numSearchLights + 1) (groupSize / 2)
                    let canon =
                        Canon.Create(settings, specialty, random, store, boundary, yori, isFlak, country)
                        |> fun canon ->
                            if i <= numSearchLights then
                                canon.SwitchToSearchLight()
                            else
                                canon
                    yield canon
            }
            |> Seq.mapi (fun i x -> CanonInstance i, x)
            |> Map.ofSeq
        let positions =
            [
                let model =
                    match specialty with
                    | AntiAirMg | AntiAirCanon -> Vehicles.vehicles.AntiAirPosition
                    | AntiTank -> Vehicles.vehicles.AntiTankPosition
                let newBlock (pos : Mcu.Vec3) (ori : Mcu.Vec3) =
                    let block = newBlock 1 (int country) model.Model model.Script
                    let mcu =
                        block
                            .SetXPos(T.Float pos.X).SetYPos(T.Float pos.Y).SetZPos(T.Float pos.Z)
                            .SetXOri(T.Float ori.X).SetYOri(T.Float ori.Y).SetZOri(T.Float ori.Z)
                            .CreateMcu()
                    let subst = Mcu.substId <| store.GetIdMapper()
                    subst mcu
                    mcu
                for canon in cannonSet do
                    yield newBlock canon.Value.Cannon.Pos canon.Value.Cannon.Ori
            ]
        let enemyClose =
            // For ATs, show when an enemy ground vehicle is near, reduce scanning range.
            let wec = WhileEnemyClose.Create(true, true, store, center, coalition)
            match specialty with
            | AntiTank ->
                let otherCoalition = McuUtil.swapCoalition coalition
                for mcu in McuUtil.deepContentOf wec.All do
                    match mcu with
                    | :? Mcu.McuProximity as prox ->
                        prox.VehicleCoalitions <- [otherCoalition]
                        prox.Distance <- 3000
                    | _ -> ()
            | _ ->
                ()
            wec
        // Result
        let api = Api.Create(store, center)
        { CanonSet = cannonSet
          EnemyClose = enemyClose
          Decorations = positions
          Api = api
        }

    member this.CreateLinks() =
        let targetLinks =
            [
                let wec = this.EnemyClose
                for canon in this.CanonSet do
                    let canon = canon.Value
                    yield wec.WakeUp, canon.Show :> Mcu.McuBase
                    yield wec.Sleep, canon.Hide :> Mcu.McuBase
                yield this.Api.Start, upcast wec.StartMonitoring
                yield this.Api.Stop, upcast wec.StopMonitoring
            ]
        { Columns = []
          Objects = []
          Targets = targetLinks
          Events = []
        }
