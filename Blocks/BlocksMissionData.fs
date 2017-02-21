module SturmovikMission.Blocks.BlocksMissionData

open SturmovikMission.DataProvider

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
