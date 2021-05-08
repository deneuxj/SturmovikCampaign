module SturmovikMission.Blocks.StaticDefenses.Factory

open System.Numerics
open VectorExtension

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
    Coalition : Mcu.CoalitionValue
    CanonSet : Map<CanonInstance, Canon>
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
            ]

    interface ITriggeredByEnemy with
        member this.Coalition =
            this.Coalition
        member this.Pos =
            this.CanonSet
            |> Map.tryPick (fun _ c -> Some c)
            |> Option.map (fun canon -> Vector2.FromMcu canon.Cannon.Pos)
            |> Option.defaultValue Vector2.Zero
        member this.Sleep =
            this.Api.Stop
        member this.WakeUp =
            this.Api.Start

    static member Create(settings : CanonGenerationSettings, specialty : DefenseSpecialty, includeFlak : bool, includeSearchLights : bool, random : System.Random, store, boundary : Vector2 list, yori : float32, groupSize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue) =
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
                        includeFlak &&
                        match specialty with
                        | AntiTank | Artillery -> false
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
                    | Artillery -> Vehicles.vehicles.ArtilleryPosition
                let newBlock (pos : Mcu.Vec3) (ori : Mcu.Vec3) =
                    let block = newBlock 1 (int country) model.Model model.Script
                    let mcu =
                        block
                            .SetXPos(T.Float.N pos.X).SetYPos(T.Float.N pos.Y).SetZPos(T.Float.N pos.Z)
                            .SetXOri(T.Float.N ori.X).SetYOri(T.Float.N ori.Y).SetZOri(T.Float.N ori.Z)
                            .CreateMcu()
                    let subst = Mcu.substId <| store.GetIdMapper()
                    subst mcu
                    mcu
                for canon in cannonSet do
                    yield newBlock canon.Value.Cannon.Pos canon.Value.Cannon.Ori
            ]
        // Result
        let api = Api.Create(store, center)
        { Coalition = coalition
          CanonSet = cannonSet
          Decorations = positions
          Api = api
        }

    member this.CreateLinks() =
        let targetLinks =
            [
                for canon in this.CanonSet do
                    let canon = canon.Value
                    yield this.Api.Start, canon.Show :> Mcu.McuBase
                    yield this.Api.Stop, canon.Hide :> Mcu.McuBase
            ]
        { Columns = []
          Objects = []
          Targets = targetLinks
          Events = []
        }
