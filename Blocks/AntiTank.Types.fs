module SturmovikMission.Blocks.AntiTank.Types

open System.Numerics
open Vector
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types


let getRandomPositionInArea(random : System.Random, area : T.MCU_TR_InfluenceArea) =    
    let forward = Vector2.FromYOri(area)
    let right = forward.Rotate(90.0f)
    let vertices =
        area.Boundary.Value
        |> List.map Vector2.FromPair
    let xs =
        vertices
        |> List.map (fun v -> Vector2.Dot(v, forward))
    let xmin = List.min xs
    let xmax = List.max xs
    let xL = xmax - xmin
    let x0 = xmin
    let ys =
        vertices
        |> List.map (fun v -> Vector2.Dot(v, right))
    let ymin = List.min ys
    let ymax = List.max ys
    let y0 = ymin
    let yL = ymax - ymin
    Seq.initInfinite (fun _ ->
        let x = x0 + float32 (random.NextDouble()) * xL
        let y = y0 + float32 (random.NextDouble()) * yL
        Vector2(x, y)
    )
    |> Seq.find(fun v -> v.IsInConvexPolygon(vertices))


type AntiTank = {
    Canon : Mcu.McuEntity
    Show : Mcu.McuTrigger
    Hide : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(random : System.Random, store : NumericalIdentifiers.IdStore, boundary : T.MCU_TR_InfluenceArea, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = T.GroupData(Parsing.Stream.FromFile "Blocks.Mission").GetGroup("AntiTank").CreateMcuList()
        for mcu in db do
            subst mcu
        // Get key nodes
        let lead = getVehicleByName db T.Blocks.``AT leader``
        let mg = getVehicleByName db "MG"
        let show = getTriggerByName db "SHOW"
        let hide = getTriggerByName db "HIDE"
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
        let pos = getRandomPositionInArea(random, boundary)
        lead.Pos.X <- float pos.X
        lead.Pos.Z <- float pos.Y
        mg.Country <- country
        let pos = getRandomPositionInArea(random, boundary)
        mg.Pos.X <- float pos.X
        mg.Pos.Z <- float pos.Y
        // Result
        { Canon = McuUtil.getEntityByIndex lead.LinkTrId db
          Show = show
          Hide = hide
          All = McuUtil.groupFromList db
        }


type AntiTankCanon = {
    Canon : Mcu.McuEntity
    All : McuUtil.IMcuGroup
}
with
    static member Create(random : System.Random, store : NumericalIdentifiers.IdStore, boundary : T.MCU_TR_InfluenceArea, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let db = T.GroupData(Parsing.Stream.FromFile "Blocks.Mission").GetGroup("AntiTank").CreateMcuList()
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
        let pos = getRandomPositionInArea(random, boundary)
        canon.Pos.X <- float pos.X
        canon.Pos.Z <- float pos.Y
        // Result
        let entity = McuUtil.getEntityByIndex canon.LinkTrId db
        { Canon = entity
          All = McuUtil.groupFromList [canon ; entity]
        }
