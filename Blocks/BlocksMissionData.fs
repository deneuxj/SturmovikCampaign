module SturmovikMission.Blocks.BlocksMissionData

open SturmovikMission.DataProvider
open System.Numerics

type T = SturmovikMissionTypes.Provider<"../data/Sample.Mission", "../data/Blocks/Blocks.Mission">

let blockMission = "Blocks.Mission"
let blocksData = T.GroupData(Parsing.Stream.FromFile blockMission)

// Utility functions to create MCU programmatically.
let newTimer idx =
    T.MCU_Timer(T.String "", T.Integer idx, T.String "", T.VectorOfIntegers[], T.Integer 100, T.VectorOfIntegers[], T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0)
        .CreateMcu() :?> Mcu.McuTimer

let newCounter idx =
    T.MCU_Counter(T.Integer 1, T.String "", T.Boolean false, T.Integer idx, T.String "", T.VectorOfIntegers[], T.VectorOfIntegers[], T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0, T.Float 0.0)
        .CreateMcu() :?> Mcu.McuCounter

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
        T.Boolean true,
        T.Integer 60,
        T.String "",
        T.Integer 0,
        T.String "",
        T.Integer -1,
        T.Integer 0,
        T.Boolean true,
        T.Integer mods
    )

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
        T.Integer -1,
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

let newBlockWithEntityMcu (store : NumericalIdentifiers.IdStore) country model script =
    let block = (newBlock 1 country model script).SetLinkTrId(T.Integer 2)
    let entity = newEntity 2
    entity.MisObjID <- 1
    let subst = Mcu.substId <| store.GetIdMapper()
    let block = block.CreateMcu()
    subst block
    subst entity
    block, entity

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

    let inline valueOf(v : ^V) : ^R =
        (^V : (member Value : ^R) v)

    let inline setLinkTrId (index : ^D) (x : ^T) =
        (^T : (member SetLinkTrId : ^D -> ^T) x, index)
