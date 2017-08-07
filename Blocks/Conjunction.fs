module SturmovikMission.Blocks.Conjunction

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData

/// <summary>
/// Conjunction of two conditions A and B
/// </summary>
type Conjunction = {
    SetA : Mcu.McuTrigger
    ClearA : Mcu.McuTrigger
    SetB : Mcu.McuTrigger
    ClearB : Mcu.McuTrigger
    AllTrue : Mcu.McuTrigger
    SomeFalse : Mcu.McuTrigger
    All : IMcuGroup
}
with
    /// <summary>
    /// Create conjunction logic
    /// </summary>
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Conjunction").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let setA = getTriggerByName group T.Blocks.SetA
        let clearA = getTriggerByName group T.Blocks.ClearA
        let setB = getTriggerByName group T.Blocks.SetB
        let clearB = getTriggerByName group T.Blocks.ClearB
        let allTrue = getTriggerByName group T.Blocks.AllTrue
        let someFalse = getTriggerByName group T.Blocks.SomeFalse
        // Position of all nodes
        let refPoint = Vector2.FromMcu setA.Pos
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // result
        { SetA = setA
          ClearA = clearA
          SetB = setB
          ClearB = clearB
          AllTrue = allTrue
          SomeFalse = someFalse
          All = groupFromList group
        }
