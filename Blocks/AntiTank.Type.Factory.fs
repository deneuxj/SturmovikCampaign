module SturmovikMission.Blocks.AntiTank.Factory

open System.Numerics
open SturmovikMission.Blocks
open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.AntiTank.Types
open SturmovikMission.Blocks.Links
open SturmovikMission.Blocks.Predicates
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil

type LeadCanonInstance = LeadCanonInstance of int
type AntiTankCanonInstance = AntiTankCanonInstance of lead: int * rank: int

type AntiTankGroup = {
    LeadAntiTankCanonSet : Map<LeadCanonInstance, AntiTank>
    AntiTankCanonSet : Map<AntiTankCanonInstance, AntiTankCanon>
    EnemyCloseSet : Map<WhileEnemyCloseInstance, WhileEnemyClose>
    CanonInGroup : Set<LeadCanonInstance * int * AntiTankCanonInstance>
    LeadOfEnemyClose : Set<WhileEnemyCloseInstance * LeadCanonInstance>
    Api : AntiTank
}
with
    interface McuUtil.IMcuGroup with
        member this.Content = []
        member this.LcStrings = []
        member this.SubGroups =
            [
                yield! this.LeadAntiTankCanonSet |> Seq.map (fun kvp -> kvp.Value.All)
                yield! this.AntiTankCanonSet |> Seq.map (fun kvp -> kvp.Value.All)
                yield! this.EnemyCloseSet |> Seq.map (fun kvp -> kvp.Value.All)
                yield this.Api.All
            ]

    static member Create(random : System.Random, store : NumericalIdentifiers.IdStore, boundary : Vector2 list, yori : float32, groupSize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue) =
        let center =
            let n = max 1 (List.length boundary)
            let k = 1.0f / float32 n
            let sum =
                boundary
                |> Seq.sum
            k * sum
        let leadAntiTankCanonSet =
            seq {
                yield LeadCanonInstance 1, AntiTank.Create(random, store, boundary, yori, country)
            }
            |> Map.ofSeq
        let antiTankCanonSet =
            seq {
                for i in 1 .. groupSize do
                    yield AntiTankCanonInstance(1, i), AntiTankCanon.Create(random, store, boundary, yori, country)
            }
            |> Map.ofSeq
        let enemyCloseSet =
            seq {
                yield WhileEnemyCloseInstance 1, WhileEnemyClose.Create(store, McuUtil.newVec3(float center.X, 0.0, float center.Y), coalition)
            }
            |> Map.ofSeq
        let canonInGroup =
            seq {
                for i in 1 .. groupSize do
                    yield LeadCanonInstance 1, i, AntiTankCanonInstance(1, i)
            }
            |> Set.ofSeq
        let leadOfEnemyClose =
            [ WhileEnemyCloseInstance 1, LeadCanonInstance 1]
            |> Set.ofList
        let api =
            leadAntiTankCanonSet.[LeadCanonInstance 1]
        { LeadAntiTankCanonSet = leadAntiTankCanonSet
          AntiTankCanonSet = antiTankCanonSet
          EnemyCloseSet = enemyCloseSet
          CanonInGroup = canonInGroup
          LeadOfEnemyClose = leadOfEnemyClose
          Api = api
        }

    member this.CreateLinks() =
        let columns =
            [
                for lead, rank, canon in this.CanonInGroup do
                    yield this.AntiTankCanonSet.[canon].Canon, this.LeadAntiTankCanonSet.[lead].Canon, rank
            ]
        let objectLinks =
            [
                for wec, lead in this.LeadOfEnemyClose do
                    yield this.EnemyCloseSet.[wec].Proximity, this.LeadAntiTankCanonSet.[lead].Canon :> Mcu.McuBase
            ]
        let targetLinks =
            [
                for wec, lead in this.LeadOfEnemyClose do
                    let wec = this.EnemyCloseSet.[wec]
                    let lead = this.LeadAntiTankCanonSet.[lead]
                    yield wec.WakeUp, lead.Show :> Mcu.McuBase
                    yield wec.Sleep, lead.Hide :> Mcu.McuBase
                    yield lead.Start, wec.StartMonitoring :> Mcu.McuBase
            ]
        { Columns = columns
          Objects = objectLinks
          Targets = targetLinks
        }


type AntiAirGroup = {
    Underlying : AntiTankGroup
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = (x.Underlying :> McuUtil.IMcuGroup).Content
        member x.LcStrings = (x.Underlying :> McuUtil.IMcuGroup).LcStrings
        member x.SubGroups = (x.Underlying :> McuUtil.IMcuGroup).SubGroups

    static member Create(random : System.Random, store : NumericalIdentifiers.IdStore, boundary : Vector2 list, yori : float32, groupSize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue) =
        let ret = { Underlying = AntiTankGroup.Create(random, store, boundary, yori, groupSize, country, coalition) }
        ret.Mutate(random)
        ret

    member private this.Mutate(random : System.Random) =
        let mutateVehicle(vehicle : Mcu.HasEntity) =
            let spec =
                match vehicle.Country with
                | Mcu.CountryValue.Russia ->
                    if random.Next() % 2 = 1 then
                        Vehicles.russianAntiAirCanon
                    else
                        Vehicles.russianFlak
                | _ ->
                    if random.Next() % 2 = 1 then
                        Vehicles.germanAntiAirCanon
                    else
                        Vehicles.germanFlak
            vehicle.Script <- spec.Script
            vehicle.Model <- spec.Model
        for kvp in this.Underlying.AntiTankCanonSet do
            let members = McuUtil.deepContentOf kvp.Value.All
            let vehicle = 
                McuUtil.getHasEntityByIndex kvp.Value.Canon.MisObjID members
            mutateVehicle vehicle
        for kvp in this.Underlying.LeadAntiTankCanonSet do
            let members = McuUtil.deepContentOf kvp.Value.All
            let vehicle = 
                McuUtil.getHasEntityByIndex kvp.Value.Canon.MisObjID members
            mutateVehicle vehicle
            
    member this.CreateLinks() =
        this.Underlying.CreateLinks()