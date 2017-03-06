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

type LeadCanonInstance = LeadCanonInstance of int
type CanonInstance = CanonInstance of lead: int * rank: int

type StaticDefenseGroup = {
    LeadCanonSet : Map<LeadCanonInstance, LeadCanon>
    CanonSet : Map<CanonInstance, Canon>
    EnemyCloseSet : Map<WhileEnemyCloseInstance, WhileEnemyClose>
    CanonInGroup : Set<LeadCanonInstance * int * CanonInstance>
    LeadOfEnemyClose : Set<WhileEnemyCloseInstance * LeadCanonInstance>
    Decorations : Mcu.McuBase list
    Api : LeadCanon
}
with
    interface McuUtil.IMcuGroup with
        member this.Content = this.Decorations
        member this.LcStrings = []
        member this.SubGroups =
            [
                yield! this.LeadCanonSet |> Seq.map (fun kvp -> kvp.Value.All)
                yield! this.CanonSet |> Seq.map (fun kvp -> kvp.Value.All)
                yield! this.EnemyCloseSet |> Seq.map (fun kvp -> kvp.Value.All)
            ]

    static member Create(specialty : DefenseSpecialty, random : System.Random, store : NumericalIdentifiers.IdStore, boundary : Vector2 list, yori : float32, groupSize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue) =
        let center =
            let n = max 1 (List.length boundary)
            let k = 1.0f / float32 n
            let sum =
                boundary
                |> Seq.sum
            k * sum
        let leadAntiTankCanonSet =
            seq {
                yield LeadCanonInstance 1, LeadCanon.Create(specialty, random, store, boundary, yori, country)
            }
            |> Map.ofSeq
        let antiTankCanonSet =
            seq {
                for i in 1 .. groupSize do
                    yield CanonInstance(1, i), Canon.Create(specialty, random, store, boundary, yori, country)
            }
            |> Map.ofSeq
        let positions =
            [
                let model =
                    match specialty with
                    | AntiAir -> Vehicles.antiAirPosition
                    | AntiTank -> Vehicles.antiTankPosition
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
                for lead in leadAntiTankCanonSet do
                    let pos = lead.Value.Canon.Pos
                    let ori = lead.Value.Canon.Ori
                    yield newBlock pos ori
                for canon in antiTankCanonSet do
                    yield newBlock canon.Value.Canon.Pos canon.Value.Canon.Ori
            ]
        let enemyCloseSet =
            seq {
                // For ATs, show when an enemy ground vehicle is near, reduce scanning range.
                let wec = WhileEnemyClose.Create(store, center, coalition)
                match specialty with
                | AntiTank ->
                    let otherCoalition =
                        match coalition with
                        | Mcu.CoalitionValue.Allies -> Mcu.CoalitionValue.Axis
                        | Mcu.CoalitionValue.Axis -> Mcu.CoalitionValue.Allies
                        | _ -> failwithf "Unexpected coalition value %A" coalition
                    for mcu in McuUtil.deepContentOf wec.All do
                        match mcu with
                        | :? Mcu.McuProximity as prox ->
                            prox.VehicleCoalitions <- [otherCoalition]
                            prox.Distance <- 2000
                        | _ -> ()
                | _ ->
                    ()
                yield WhileEnemyCloseInstance 1, wec
            }
            |> Map.ofSeq
        let canonInGroup =
            seq {
                for i in 1 .. groupSize do
                    yield LeadCanonInstance 1, i, CanonInstance(1, i)
            }
            |> Set.ofSeq
        let leadOfEnemyClose =
            [ WhileEnemyCloseInstance 1, LeadCanonInstance 1 ]
            |> Set.ofList
        let api =
            leadAntiTankCanonSet.[LeadCanonInstance 1]
        { LeadCanonSet = leadAntiTankCanonSet
          CanonSet = antiTankCanonSet
          EnemyCloseSet = enemyCloseSet
          CanonInGroup = canonInGroup
          LeadOfEnemyClose = leadOfEnemyClose
          Decorations = positions
          Api = api
        }

    member this.CreateLinks() =
        let columns =
            [
                for lead, rank, canon in this.CanonInGroup do
                    yield this.CanonSet.[canon].Canon, this.LeadCanonSet.[lead].Canon, rank
            ]
        let objectLinks =
            [
                for wec, lead in this.LeadOfEnemyClose do
                    yield this.EnemyCloseSet.[wec].Proximity, this.LeadCanonSet.[lead].Canon :> Mcu.McuBase
            ]
        let targetLinks =
            [
                for wec, lead in this.LeadOfEnemyClose do
                    let wec = this.EnemyCloseSet.[wec]
                    let lead = this.LeadCanonSet.[lead]
                    yield wec.WakeUp, lead.Show :> Mcu.McuBase
                    yield wec.Sleep, lead.Hide :> Mcu.McuBase
                    yield lead.Start, wec.StartMonitoring :> Mcu.McuBase
            ]
        { Columns = columns
          Objects = objectLinks
          Targets = targetLinks
        }
