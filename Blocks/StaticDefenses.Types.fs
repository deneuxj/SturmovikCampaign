module SturmovikMission.Blocks.StaticDefenses.Types

open System.Numerics
open Vector
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.BlocksMissionData

let getRandomPositionInArea(random : System.Random, area : Vector2 list, forward : Vector2) =
    let right = forward.Rotate(90.0f)
    let xs =
        area
        |> List.map (fun v -> Vector2.Dot(v, forward))
    let xmin = List.min xs
    let xmax = List.max xs
    let xL = xmax - xmin
    let x0 = xmin
    let ys =
        area
        |> List.map (fun v -> Vector2.Dot(v, right))
    let ymin = List.min ys
    let ymax = List.max ys
    let y0 = ymin
    let yL = ymax - ymin
    Seq.initInfinite (fun _ ->
        let x = x0 + float32 (random.NextDouble()) * xL
        let y = y0 + float32 (random.NextDouble()) * yL
        x * forward + y * right
    )
    |> Seq.find(fun v -> v.IsInConvexPolygon(area))

type DefenseSpecialty =
    | AntiTank
    | AntiAir
with
    member this.GroupName =
        match this with
        | AntiTank -> "AntiTank"
        | AntiAir -> "AntiAir"

    member this.SetModel(thing : Mcu.HasEntity, ?random : System.Random) =
        match this with
        | AntiTank ->
            match thing.Country with
            | Mcu.CountryValue.Germany ->
                thing.Model <- vehicles.GermanAntiTankCanon.Model
                thing.Script <- vehicles.GermanAntiTankCanon.Script
            | Mcu.CountryValue.Russia ->
                thing.Model <- vehicles.RussianAntiTankCanon.Model
                thing.Script <- vehicles.RussianAntiTankCanon.Script
            | _ ->
                ()
        | AntiAir ->
            let random = defaultArg random (System.Random())
            match thing.Country with
            | Mcu.CountryValue.Germany ->
                if random.Next(2) = 1 then
                    thing.Model <- vehicles.GermanAntiAirCanon.Model
                    thing.Script <- vehicles.GermanAntiAirCanon.Script
                else
                    thing.Model <- vehicles.GermanFlak.Model
                    thing.Script <- vehicles.GermanFlak.Script
            | Mcu.CountryValue.Russia ->
                if random.Next(2) = 1 then
                    thing.Model <- vehicles.RussianAntiAirCanon.Model
                    thing.Script <- vehicles.RussianAntiAirCanon.Script
                else
                    thing.Model <- vehicles.RussianFlak.Model
                    thing.Script <- vehicles.RussianFlak.Script
            | _ ->
                ()

type Canon = {
    Canon : Mcu.McuEntity
    Show : Mcu.McuTrigger
    Hide : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(specialty : DefenseSpecialty, random : System.Random, store : NumericalIdentifiers.IdStore, boundary : Vector2 list, yori : float32 , country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.GetGroup(specialty.GroupName)
        let db = db.CreateMcuList()
        for mcu in db do
            subst mcu
        // Get key nodes
        let canon = getVehicleByName db "CANON"
        let show = getTriggerByName db "SHOW"
        let hide = getTriggerByName db "HIDE"
        // Set country and positions
        canon.Country <- country
        specialty.SetModel(canon, random)
        let angle = float32 System.Math.PI * (yori / 180.0f)
        let forward = Vector2(cos angle, sin angle)
        let newPos = getRandomPositionInArea(random, boundary, forward)
        let refPos = Vector2.FromMcu(canon.Pos)
        let translation = newPos - refPos
        for mcu in db do
            // rotate around old position of canon, then translate to new position
            let pos = Vector2.FromMcu(mcu.Pos)
            let v = pos - refPos
            let final = v.Rotate(yori) + refPos + translation
            final.AssignTo(mcu.Pos)
            mcu.Ori.Y <- mcu.Ori.Y + float yori
        // Result
        { Canon = McuUtil.getEntityByIndex canon.LinkTrId db
          Show = show
          Hide = hide
          All = McuUtil.groupFromList db
        }

    member this.SwitchToSearchLight() =
        let mcus = McuUtil.deepContentOf this.All
        let attackAirOrder =
            getTriggerByName mcus "AttackAirTargets"
        let searchOrder =
            let db = blocksData.GetGroup("Palette").CreateMcuList()
            getTriggerByName db "ForceCompleteLow"
        searchOrder.Targets <- attackAirOrder.Targets
        searchOrder.Objects <- attackAirOrder.Objects
        vecCopy attackAirOrder.Pos searchOrder.Pos
        searchOrder.Index <- attackAirOrder.Index
        let canon = getHasEntityByIndex this.Canon.MisObjID mcus
        match canon.Country with
        | Mcu.CountryValue.Germany ->
            canon.Model <- vehicles.GermanSearchLight.Model
            canon.Script <- vehicles.GermanSearchLight.Script
        | _ ->
            canon.Model <- vehicles.RussianSearchLight.Model
            canon.Script <- vehicles.RussianSearchLight.Script
        let mcus =
            mcus
            |> List.map (fun mcu ->
                if mcu.Index = searchOrder.Index then
                    searchOrder :> Mcu.McuBase
                else
                    mcu)
        { this with All = McuUtil.groupFromList mcus }

type Api = {
    Start : Mcu.McuTrigger
    Stop : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2) =
        let subst = Mcu.substId <| store.GetIdMapper()
        let start = newTimer 1
        let stop = newTimer 2
        subst start
        subst stop
        pos.AssignTo(start.Pos)
        (pos + Vector2(100.0f, 0.0f)).AssignTo(stop.Pos)
        { Start = start
          Stop = stop
          All = McuUtil.groupFromList [ start; stop ]
        }