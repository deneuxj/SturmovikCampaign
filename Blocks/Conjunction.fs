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
        let setA = getTriggerByName group "SetA"
        let clearA = getTriggerByName group "ClearA"
        let setB = getTriggerByName group "SetB"
        let clearB = getTriggerByName group "ClearB"
        let allTrue = getTriggerByName group "AllTrue"
        let someFalse = getTriggerByName group "SomeFalse"
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

    /// Clear A and B at mission start.
    member this.MakeInitiallyFalse(store : NumericalIdentifiers.IdStore) =
        let initially = newMissionBegin 1
        (Vector2.FromMcu(this.ClearA.Pos) - Vector2(0.0f, 100.0f)).AssignTo(initially.Pos)
        let subst = Mcu.substId <| store.GetIdMapper()
        subst initially
        Mcu.addTargetLink initially this.ClearA.Index
        Mcu.addTargetLink initially this.ClearB.Index
        { this with
            All =
                { new IMcuGroup with
                      member x.Content: Mcu.McuBase list = upcast initially :: this.All.Content
                      member x.LcStrings: (int * string) list = this.All.LcStrings
                      member x.SubGroups: IMcuGroup list = this.All.SubGroups
                }
        }

    /// Clear A and B on a given trigger
    member this.ClearABOn(trigger : Mcu.McuTrigger) =
        Mcu.addTargetLink trigger this.ClearA.Index
        Mcu.addTargetLink trigger this.ClearB.Index
        this
