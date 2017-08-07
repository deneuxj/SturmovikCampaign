module SturmovikMission.Blocks.Effect

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.BlocksMissionData

/// <summary>
/// Single-shot proximity detection of planes.
/// </summary>
type EffectControl = {
    Start : Mcu.McuTrigger
    Stop : Mcu.McuTrigger
    All : IMcuGroup
}
with
    /// <summary>
    /// Create logic to start/stop effects (land fires, smoke...)
    /// </summary>
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("StartStopEffect").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.StartEffect
        let stop = getTriggerByName group T.Blocks.StopEffect
        // Position of all nodes
        let refPoint = Vector2.FromMcu start.Pos
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // result
        { Start = start
          Stop = stop
          All = groupFromList group
        }


