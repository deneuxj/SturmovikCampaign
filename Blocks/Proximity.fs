module SturmovikMission.Blocks.Proximity

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
type Proximity = {
    Proximity : Mcu.McuProximity
    Start : Mcu.McuTimer
    Out : Mcu.McuCounter
    All : IMcuGroup
}
with
    /// <summary>
    /// Create logic detecting incoming planes of a certain coalition.
    /// </summary>
    static member Create(store : NumericalIdentifiers.IdStore, coalition : Mcu.CoalitionValue, range : int, pos : Vector2) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Proximity").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.Start :?> Mcu.McuTimer
        let out = getTriggerByName group T.Blocks.Out :?> Mcu.McuCounter
        let proximity = getTriggerByName group T.Blocks.Proximity :?> Mcu.McuProximity
        // Position of all nodes
        let refPoint = Vector2.FromMcu proximity.Pos
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // Set coalition to react to
        proximity.PlaneCoalitions <- [coalition]
        // result
        { Start = start
          Out = out
          Proximity = proximity
          All = groupFromList group
        }
