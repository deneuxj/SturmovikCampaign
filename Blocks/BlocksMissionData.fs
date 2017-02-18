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
