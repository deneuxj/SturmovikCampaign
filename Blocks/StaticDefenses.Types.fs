module SturmovikMission.Blocks.StaticDefenses.Types

open System.Numerics
open VectorExtension
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
    | AntiAirMg
    | AntiAirCanon
with
    member this.GroupName =
        match this with
        | AntiTank -> "AntiTank"
        | AntiAirMg | AntiAirCanon -> "AntiAir"

    member this.SetModel(thing : Mcu.HasEntity, isFlak : bool) =
        match this with
        | AntiTank ->
            match thing.Country with
            | Some Mcu.CountryValue.Germany ->
                vehicles.GermanAntiTankCanon.AssignTo(thing)
            | Some Mcu.CountryValue.Russia ->
                vehicles.RussianAntiTankCanon.AssignTo(thing)
            | _ ->
                ()
        | AntiAirCanon ->
            match thing.Country with
            | Some Mcu.CountryValue.Germany ->
                if isFlak then
                    vehicles.GermanFlak.AssignTo(thing)
                else
                    vehicles.GermanAntiAirCanon.AssignTo(thing)
            | Some Mcu.CountryValue.Russia ->
                if isFlak then
                    vehicles.RussianFlak.AssignTo(thing)
                else
                    vehicles.RussianAntiAirCanon.AssignTo(thing)
            | _ ->
                ()
        | AntiAirMg ->
            match thing.Country with
            | Some Mcu.CountryValue.Germany ->
                if isFlak then
                    vehicles.GermanFlak.AssignTo(thing)
                else
                    vehicles.GermanAntiAirMachineGun.AssignTo(thing)
            | Some Mcu.CountryValue.Russia ->
                if isFlak then
                    vehicles.RussianFlak.AssignTo(thing)
                else
                    vehicles.RussianAntiAirMachineGun.AssignTo(thing)
            | _ ->
                ()

[<Literal>]
let CannonObjectName = "CANNON"
[<Literal>]
let LightMachineGunAAName = "MGAA-L"
[<Literal>]
let HeavyMachineGunAAName = "MGAA-H"

type CanonGenerationSettings =
    { SkillLevel : int
      RepairDelaySeconds : float // Non positive value means "no repair"
      RepairDelayDelta : float  // Maximum deviation from delay
    }
with
    static member Default = { SkillLevel = 2; RepairDelaySeconds = -1.0; RepairDelayDelta = 0.0 }
    static member Strong = { SkillLevel = 3; RepairDelaySeconds = 180.0; RepairDelayDelta = 120.0 }

type Canon = {
    Cannon : Mcu.McuEntity
    Show : Mcu.McuTrigger
    Hide : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(settings : CanonGenerationSettings, specialty : DefenseSpecialty, random : System.Random, store : NumericalIdentifiers.IdStore, boundary : Vector2 list, yori : float32 , isFlak : bool, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.GetGroup(specialty.GroupName)
        let db = db.CreateMcuList()
        for mcu in db do
            subst mcu
        // Get key nodes
        let cannon = getVehicleByName db "CANNON"
        let show = getTriggerByName db "SHOW"
        let hide = getTriggerByName db "HIDE"
        let repairDelay = getTriggerByName db "RepairDelay" :?> Mcu.McuTimer
        let attackOrder =
            try
                getTriggerByName db "AttackAirTargets"
                |> Some
            with _ -> None
        // Set country and positions
        cannon.Country <- Some country
        specialty.SetModel(cannon, isFlak)
        let angle = float32 System.Math.PI * (yori / 180.0f)
        let forward = Vector2(cos angle, sin angle)
        let newPos = getRandomPositionInArea(random, boundary, forward)
        let refPos = Vector2.FromMcu(cannon.Pos)
        let translation = newPos - refPos
        for mcu in db do
            // rotate around old position of canon, then translate to new position
            let pos = Vector2.FromMcu(mcu.Pos)
            let v = pos - refPos
            let final = v.Rotate(yori) + refPos + translation
            final.AssignTo(mcu.Pos)
            mcu.Ori.Y <- mcu.Ori.Y + float yori
        // Set object name, used for identifying damages from the log
        let name =
            match specialty with
            | AntiTank | AntiAirCanon -> CannonObjectName
            | AntiAirMg when isFlak -> CannonObjectName
            | AntiAirMg when country = Mcu.CountryValue.Russia -> HeavyMachineGunAAName
            | AntiAirMg -> LightMachineGunAAName
        cannon.Name <- name
        // Set attack radius according to gun type
        let range =
            match specialty with
            | _ when isFlak -> 9000
            | AntiTank | AntiAirCanon -> 2000
            | AntiAirMg -> 1500
        match attackOrder with
        | Some (:? Mcu.McuAttackArea as attackOrder) ->
            attackOrder.AttackArea <- range
        | _ ->
            ()
        // Settings
        let skillLevel =
            // Limit skill level to normal for flak
            if isFlak then
                min settings.SkillLevel 2
            else
                settings.SkillLevel
        cannon.AILevel <- Some skillLevel
        let delay =
            let x = random.NextDouble()
            let x  = 2.0 * (x - 0.5)
            let x = x * x * x
            let offset = settings.RepairDelayDelta * x
            settings.RepairDelaySeconds + offset
        repairDelay.Time <- max 0.0 delay
        if settings.RepairDelaySeconds > 0.0 then
            let killed = getTriggerByName db "Killed"
            Mcu.addTargetLink killed repairDelay.Index
        // Result
        { Cannon = McuUtil.getEntityByIndex cannon.LinkTrId db
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
        let canon = getHasEntityByIndex this.Cannon.MisObjID mcus
        match canon.Country with
        | Some Mcu.CountryValue.Germany ->
            vehicles.GermanSearchLight.AssignTo(canon)
        | _ ->
            vehicles.RussianSearchLight.AssignTo(canon)
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