module SturmovikMission.Blocks.BlocksMissionData

open SturmovikMission.DataProvider
open System.Numerics
open System.IO
open Vector

type T = SturmovikMissionTypes.Provider<"../data/Sample.Mission", "../data/Blocks/Blocks.Mission;../data/Blocks/Vehicles.mission">

let blocksData =
    let blockMission =
        let path = Path.GetDirectoryName(System.Reflection.Assembly.GetCallingAssembly().Location)
        //let path = System.Environment.CurrentDirectory
        Path.Combine(path, "Blocks.Mission")
    T.GroupData(Parsing.Stream.FromFile blockMission)

// Utility functions to create MCU programmatically.
let newTimer idx =
    T.MCU_Timer(T.String "", T.Integer idx, T.String "", T.VectorOfIntegers[], T.Integer 100, T.VectorOfIntegers[], T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0)
        .CreateMcu() :?> Mcu.McuTimer

let newCounter idx =
    T.MCU_Counter(T.Integer 1, T.String "", T.Boolean false, T.Integer idx, T.String "", T.VectorOfIntegers[], T.VectorOfIntegers[], T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0)
        .CreateMcu() :?> Mcu.McuCounter

let newCheckZone idx distance =
    let mcu =
        T.MCU_CheckZone(T.Boolean true, T.Boolean true, T.String "", T.Integer idx, T.String "", T.VectorOfIntegers[], T.VectorOfIntegers[], T.VectorOfIntegers[], T.VectorOfIntegers[], T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Integer distance)
            .CreateMcu() :?> Mcu.McuProximity
    // McuProximity setters remove empty plane and vehicle coalitions fields erroneously created by T.MCU_CheckZone
    mcu.PlaneCoalitions <- []
    mcu.VehicleCoalitions <- []
    mcu

let newWaypoint idx (pos : Vector2) (yori : float32) (radius : int) (speed : int) (priority : int) =
    T.MCU_Waypoint(
        T.Integer radius,
        T.String "",
        T.Integer idx,
        T.String "",
        T.VectorOfIntegers[],
        T.Integer priority,
        T.Integer speed,
        T.VectorOfIntegers[],
        T.Float 0.0,
        T.Float(float pos.X),
        T.Float(float yori),
        T.Float 0.0,
        T.Float 0.0,
        T.Float(float pos.Y)).CreateMcu() :?> Mcu.McuWaypoint

let newEntity idx =
    T.MCU_TR_Entity(
        T.String "",
        T.Boolean true,
        T.Integer idx,
        T.Integer -1,
        T.String "",
        T.VectorOfIntegers [],
        T.VectorOfIntegers [],
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0
    ).CreateMcu() :?> Mcu.McuEntity

let newMissionBegin idx =
    T.MCU_TR_MissionBegin(
        T.String "",
        T.Boolean true,
        T.Integer idx,
        T.String "",
        T.VectorOfIntegers[],
        T.VectorOfIntegers[],
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0).CreateMcu() :?> Mcu.McuTrigger

let newAirfieldPlane(modFilter, payloadFilter, mods, payload, skinFilter, name, number) =
    T.Airfield.Planes.Plane(
        T.Integer 2,
        T.Boolean false,
        T.Integer 0,
        T.String modFilter,
        T.String payloadFilter,
        T.String skinFilter,
        T.Integer 0,
        T.Integer 0,
        T.Boolean true,
        T.Float 1.0,
        T.Boolean true,
        T.String "",
        T.String name,
        T.Integer number,
        T.Integer payload,
        T.Integer 0,
        T.Boolean false,
        T.Integer 60,
        T.String "",
        T.Integer 0,
        T.String "",
        T.Integer -1,
        T.Integer 0,
        T.Boolean true,
        T.Integer mods
    ).SetRenewable(T.Boolean false)

let newBlock idx country model script =
    T.Block(
        T.Integer country,
        T.Integer 50,
        T.Boolean true,
        T.Block.Damaged(),
        T.Boolean true,
        T.String "",
        T.Integer 7000,
        T.Integer idx,
        T.Integer 0,
        T.String "",
        T.String "",
        T.String "",
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0,
        T.Float 0.0
    ).SetModel(T.String model).SetScript(T.String script)

let newBlockMcu (store : NumericalIdentifiers.IdStore) country model script durability =
    let block = (newBlock 1 country model script).SetDurability(T.Integer durability)
    let subst = Mcu.substId <| store.GetIdMapper()
    let block = block.CreateMcu()
    subst block
    block

let newBlockWithEntityMcu (store : NumericalIdentifiers.IdStore) country model script durability =
    let block = (newBlock 1 country model script).SetDurability(T.Integer durability).SetLinkTrId(T.Integer 2)
    let entity = newEntity 2
    entity.MisObjID <- 1
    let subst = Mcu.substId <| store.GetIdMapper()
    let block = block.CreateMcu()
    subst block
    subst entity
    block, entity

let newObjective idx lcDesc lcName =
    T.MCU_TR_MissionObjective(T.Integer 0, T.Boolean true, T.Integer 0, T.Integer idx, T.Integer lcDesc, T.Integer lcName, T.VectorOfIntegers[], T.Boolean true, T.VectorOfIntegers[], T.Integer 0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0)

let runwayOfAirfieldSpawn (airfield : T.Airfield) =
    let chart = airfield.TryGetChart()
    match chart with
    | None ->
        None
    | Some chart ->
        let yori = airfield.GetYOri().Value |> float32
        let points = chart.GetPoints()
        let pos, direction =
            points
            |> List.pairwise
            |> List.pick(fun (p1, p2) ->
                if p1.GetType().Value = 2 && p2.GetType().Value = 2 then
                    let mkVec(p : T.Airfield.Chart.Point) =
                        Vector2(float32 <| p.GetX().Value, float32 <| p.GetY().Value)
                    Some(mkVec(p1).Rotate(yori) + Vector2.FromPos(airfield), (mkVec(p2) - mkVec(p1)).Rotate(yori))
                else
                    None)
        Some(pos, direction.YOri)

module CommonMethods =
    let inline createMcu(x : ^T) =
        (^T : (member CreateMcu : unit -> Mcu.McuBase) x)

    let inline setDamaged (damaged : ^D) (x : ^T) =
        (^T : (member SetDamaged : ^D -> ^T) x, damaged)

    let inline setDurability (durability : ^D) (x : ^T) =
        (^T : (member SetDurability : ^D -> ^T) x, durability)

    let inline setIndex (index : ^D) (x : ^T) =
        (^T : (member SetIndex : ^D -> ^T) x, index)

    let inline getModel(x : ^T) : ^R =
        (^T : (member GetModel : unit -> ^R) x)

    let inline getScript(x : ^T) : ^R =
        (^T : (member GetScript : unit -> ^R) x)

    let inline getYOri(x : ^T) : ^R =
        (^T : (member GetYOri : unit -> ^R) x)

    let inline valueOf(v : ^V) : ^R =
        (^V : (member Value : ^R) v)

    let inline setLinkTrId (index : ^D) (x : ^T) =
        (^T : (member SetLinkTrId : ^D -> ^T) x, index)
