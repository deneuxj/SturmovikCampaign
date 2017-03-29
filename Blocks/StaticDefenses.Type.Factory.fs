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

type CanonInstance = CanonInstance of int

type StaticDefenseGroup = {
    CanonSet : Map<CanonInstance, Canon>
    EnemyClose : WhileEnemyClose
    CheckZoneOverride : Mcu.McuTrigger
    Decorations : Mcu.McuBase list
    Icon : IconDisplay option
    Api : Api
}
with
    interface McuUtil.IMcuGroup with
        member this.Content = this.Decorations
        member this.LcStrings = []
        member this.SubGroups =
            [
                yield! this.CanonSet |> Seq.map (fun kvp -> kvp.Value.All)
                yield this.Api.All
                let overriden =
                    this.EnemyClose.All
                    |> McuUtil.deepContentOf
                    |> List.map (
                        function
                        | :? Mcu.McuProximity as mcu when mcu.Name = "EnemyClose" -> this.CheckZoneOverride :> Mcu.McuBase
                        | mcu -> mcu
                    )
                yield McuUtil.groupFromList overriden
//                yield this.EnemyClose.All
                match this.Icon with
                | Some icon -> yield icon.All
                | None -> ()
            ]

    static member Create(specialty : DefenseSpecialty, includeSearchLights : bool, random : System.Random, store, lcStore, boundary : Vector2 list, yori : float32, groupSize : int, country : Mcu.CountryValue, coalition : Mcu.CoalitionValue) =
        let center =
            let n = max 1 (List.length boundary)
            let k = 1.0f / float32 n
            let sum =
                boundary
                |> Seq.sum
            k * sum
        let antiTankCanonSet =
            seq {
                for i in 1 .. groupSize do
                    yield CanonInstance(i), Canon.Create(specialty, random, store, boundary, yori, country)
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
                for canon in antiTankCanonSet do
                    yield newBlock canon.Value.Canon.Pos canon.Value.Canon.Ori
            ]
        let enemyClose =
            // For ATs, show when an enemy ground vehicle is near, reduce scanning range.
            let wec = WhileEnemyClose.Create(true, store, center, coalition)
            match specialty with
            | AntiTank ->
                let otherCoalition = McuUtil.swapCoalition coalition
                for mcu in McuUtil.deepContentOf wec.All do
                    match mcu with
                    | :? Mcu.McuProximity as prox ->
                        prox.VehicleCoalitions <- [otherCoalition]
                        prox.Distance <- 2000
                    | _ -> ()
            | _ ->
                ()
            wec
        // Replace proximity trigger by checkzone. Canons don't move.
        let proximity =
            enemyClose.Proximity :?> Mcu.McuProximity
        let checkZone = newCheckZone proximity.Index proximity.Distance
        checkZone.PlaneCoalitions <- proximity.PlaneCoalitions
        checkZone.VehicleCoalitions <- proximity.VehicleCoalitions
        checkZone.Objects <- proximity.Objects
        checkZone.Targets <- proximity.Targets
        McuUtil.vecCopy proximity.Pos checkZone.Pos
        // Replace a few canons by searchlights during dark hours
        if includeSearchLights then
            let numSearchLights = max 1 (groupSize / 10)
            for kvp in antiTankCanonSet |> Seq.truncate numSearchLights do
                let canon = kvp.Value
                let entity = canon.Canon
                let vehicle = McuUtil.getHasEntityByIndex entity.MisObjID (McuUtil.deepContentOf canon.All)
                let model =
                    match country with
                    | Mcu.CountryValue.Germany -> Vehicles.germanSearchLight
                    | _ -> Vehicles.russianSearchLight
                vehicle.Model <- model.Model
                vehicle.Script <- model.Script
        // Icon
        let icon =
            if groupSize >= 3 then
                IconDisplay.Create(store, lcStore, center, "", McuUtil.swapCoalition coalition, Mcu.IconIdValue.AttackAntiAirPosition)
                |> Some
            else
                None
        // Result
        let api = Api.Create(store, center)
        { CanonSet = antiTankCanonSet
          EnemyClose = enemyClose
          Decorations = positions
          CheckZoneOverride = checkZone
          Icon = icon
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
                match this.Icon with
                | Some icon ->
                    yield wec.WakeUp, upcast icon.Show
                | None ->
                    ()
            ]
        { Columns = []
          Objects = []
          Targets = targetLinks
        }
