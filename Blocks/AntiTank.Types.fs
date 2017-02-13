module SturmovikMission.Blocks.AntiTank.Types

open System.Numerics
open Vector
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types

let blockMission = "Blocks.Mission"
//let blockMission = @"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Blocks.Mission"

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


type AntiTank = {
    Canon : Mcu.McuEntity
    Show : Mcu.McuTrigger
    Hide : Mcu.McuTrigger
    Start : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(random : System.Random, store : NumericalIdentifiers.IdStore, boundary : Vector2 list, yori : float32 , country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = T.GroupData(Parsing.Stream.FromFile blockMission).GetGroup("AntiTank").CreateMcuList()
        for mcu in db do
            subst mcu
        // Get key nodes
        let lead = getVehicleByName db T.Blocks.``AT leader``
        let mg = getVehicleByName db "MG"
        let show = getTriggerByName db "SHOW"
        let hide = getTriggerByName db "HIDE"
        let start = getTriggerByName db "START"
        // Set country and positions
        lead.Country <- country
        match country with
        | Mcu.CountryValue.Germany ->
            lead.Model <- germanAntiTankCanon.Model
            lead.Script <- germanAntiTankCanon.Script
        | Mcu.CountryValue.Russia ->
            lead.Model <- russianAntiTankCanon.Model
            lead.Script <- russianAntiTankCanon.Script
        | _ ->
            ()
        let forward = Vector2.UnitX.Rotate(yori)
        let pos = getRandomPositionInArea(random, boundary, forward)
        lead.Pos.X <- float pos.X
        lead.Pos.Z <- float pos.Y
        lead.Ori.Y <- float yori
        mg.Country <- country
        let pos = getRandomPositionInArea(random, boundary, forward)
        mg.Pos.X <- float pos.X
        mg.Pos.Z <- float pos.Y
        mg.Ori.Y <- float yori
        // Result
        { Canon = McuUtil.getEntityByIndex lead.LinkTrId db
          Show = show
          Hide = hide
          Start = start
          All = McuUtil.groupFromList db
        }


type AntiTankCanon = {
    Canon : Mcu.McuEntity
    All : McuUtil.IMcuGroup
}
with
    static member Create(random : System.Random, store : NumericalIdentifiers.IdStore, boundary : Vector2 list, yori : float32, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = T.GroupData(Parsing.Stream.FromFile blockMission).GetGroup("AntiTank").CreateMcuList()
        for mcu in db do
            subst mcu
        // Get key nodes
        let canon = getVehicleByName db T.Blocks.``AT leader``
        // Set country and positions
        canon.Country <- country
        match country with
        | Mcu.CountryValue.Germany ->
            canon.Model <- germanAntiTankCanon.Model
            canon.Script <- germanAntiTankCanon.Script
        | Mcu.CountryValue.Russia ->
            canon.Model <- russianAntiTankCanon.Model
            canon.Script <- russianAntiTankCanon.Script
        | _ ->
            ()
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
