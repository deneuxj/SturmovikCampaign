﻿module SturmovikMission.Blocks.StaticDefenses.Factory

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
    Decorations : Mcu.McuBase list
    Icon : (IconDisplay * Mcu.McuCounter) option
    Api : Api
}
with
    interface McuUtil.IMcuGroup with
        member this.Content =
            [
                yield! this.Decorations
                match this.Icon with
                | Some(icon, counter) -> yield counter :> Mcu.McuBase
                | None -> ()
            ]
        member this.LcStrings = []
        member this.SubGroups =
            [
                yield! this.CanonSet |> Seq.map (fun kvp -> kvp.Value.All)
                yield this.Api.All
                yield this.EnemyClose.All
                match this.Icon with
                | Some(icon, counter) -> yield icon.All
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
            let numSearchLights =
                if includeSearchLights then
                    max 1 (groupSize / 10)
                else
                    0
            seq {
                for i in 1 .. groupSize do
                    let canon =
                        Canon.Create(specialty, random, store, boundary, yori, country)
                        |> fun canon ->
                            if i <= numSearchLights then
                                canon.SwitchToSearchLight()
                            else
                                canon
                    yield CanonInstance(i), canon
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
            let wec = WhileEnemyClose.Create(true, true, store, center, coalition)
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
        // Icon
        let icon =
            if groupSize >= 3 then
                let icon =
                    IconDisplay.Create(store, lcStore, center, "", McuUtil.swapCoalition coalition, Mcu.IconIdValue.AttackAntiAirPosition)
                match icon.Show with
                | :? Mcu.McuTimer as timer ->
                    timer.Time <- 300.0 // Delay showing icon by 5 minutes.
                | _ ->
                    ()
                // Show the icon only once. It's never hidden anyway, and reshow-ing it every time a plane approaches is unnecessary. Moreover, I suspect it causes stutters.
                let once = newCounter 1
                let subst = Mcu.substId <| store.GetIdMapper()
                subst once
                Mcu.addTargetLink once icon.Show.Index
                Some(icon, once)
            else
                None
        // Result
        let api = Api.Create(store, center)
        { CanonSet = antiTankCanonSet
          EnemyClose = enemyClose
          Decorations = positions
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
                | Some(_, once) ->
                    yield wec.WakeUp, upcast once
                | None ->
                    ()
            ]
        { Columns = []
          Objects = []
          Targets = targetLinks
        }
