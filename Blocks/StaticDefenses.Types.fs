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
                thing.Model <- germanAntiTankCanon.Model
                thing.Script <- germanAntiTankCanon.Script
            | Mcu.CountryValue.Russia ->
                thing.Model <- russianAntiTankCanon.Model
                thing.Script <- russianAntiTankCanon.Script
            | _ ->
                ()
        | AntiAir ->
            let random = defaultArg random (System.Random())
            match thing.Country with
            | Mcu.CountryValue.Germany ->
                if random.Next(2) = 1 then
                    thing.Model <- germanAntiAirCanon.Model
                    thing.Script <- germanAntiAirCanon.Script
                else
                    thing.Model <- germanFlak.Model
                    thing.Script <- germanFlak.Script
            | Mcu.CountryValue.Russia ->
                if random.Next(2) = 1 then
                    thing.Model <- russianAntiAirCanon.Model
                    thing.Script <- russianAntiAirCanon.Script
                else
                    thing.Model <- russianFlak.Model
                    thing.Script <- russianFlak.Script
            | _ ->
                ()

type LeadCanon = {
    Canon : Mcu.McuEntity
    Show : Mcu.McuTrigger
    Hide : Mcu.McuTrigger
    Start : Mcu.McuTrigger
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
        let lead = getVehicleByName db T.Blocks.``AT leader``
        let mg =
            try
                getVehicleByName db "MG"
                |> Some
            with
            | _ -> None
        let show = getTriggerByName db "SHOW"
        let hide = getTriggerByName db "HIDE"
        let start = getTriggerByName db "START"
        // Set country and positions
        lead.Country <- country
        specialty.SetModel(lead, random)
        let angle = float32 System.Math.PI * (yori / 180.0f)
        let forward = Vector2(cos angle, sin angle)
        let newPos = getRandomPositionInArea(random, boundary, forward)
        let refPos = Vector2(float32 lead.Pos.X, float32 lead.Pos.Z)
        let translation = newPos - refPos
        for mcu in db do
            // rotate around old position of leader, then translate to new position
            let pos = Vector2(float32 mcu.Pos.X, float32 mcu.Pos.Z)
            let v = pos - refPos
            let final = v.Rotate(yori) + refPos + translation
            mcu.Pos.X <- float final.X
            mcu.Pos.Z <- float final.Y
            mcu.Ori.Y <- mcu.Ori.Y + float yori
        // Adjust machinegun, if any.
        match mg with
        | Some mg ->
            mg.Country <- country
            // Move the machine gun to a random position
            let pos = getRandomPositionInArea(random, boundary, forward)
            mg.Pos.X <- float pos.X
            mg.Pos.Z <- float pos.Y
            mg.Ori.Y <- float yori
        | None ->
            ()
        // Result
        { Canon = McuUtil.getEntityByIndex lead.LinkTrId db
          Show = show
          Hide = hide
          Start = start
          All = McuUtil.groupFromList db
        }


type Canon = {
    Canon : Mcu.McuEntity
    All : McuUtil.IMcuGroup
}
with
    static member Create(specialty : DefenseSpecialty, random : System.Random, store : NumericalIdentifiers.IdStore, boundary : Vector2 list, yori : float32, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = blocksData.GetGroup(specialty.GroupName).CreateMcuList()
        for mcu in db do
            subst mcu
        // Get key nodes
        let canon = getVehicleByName db T.Blocks.``AT leader``
        // Set country and positions
        canon.Country <- country
        specialty.SetModel(canon, random)
        let forward = Vector2.UnitX.Rotate(yori)
        let pos = getRandomPositionInArea(random, boundary, forward)
        canon.Pos.X <- float pos.X
        canon.Pos.Z <- float pos.Y
        canon.Ori.Y <- float yori
        // Result
        let entity = McuUtil.getEntityByIndex canon.LinkTrId db
        { Canon = entity
          All = McuUtil.groupFromList [canon ; entity]
        }
