module SturmovikMission.Blocks.ResourcePool

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData

/// A node to limit instantiation of heavy instances (typically planes with AIs) to a maximum concurrent number.
type ResourcePool = {
    Grab : Mcu.McuTrigger
    Release : Mcu.McuTrigger
    AllGrabbed : Mcu.McuTrigger
    AvailableAgain : Mcu.McuTrigger
    /// Convenience activate node triggered by AvailableAgain
    Open : Mcu.McuTrigger
    /// Convenience deactivate node triggered by AllGrabbed
    Close : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, size : int, position : Vector2) : ResourcePool =
        if size < 1 then
            invalidArg "size" "Must be at least 1"
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = SturmovikMission.Blocks.BlocksMissionData.blocksData.GetGroup("ResourceLock").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let grab = getTriggerByName group "GRAB"
        let release = getTriggerByName group "RELEASE"
        let allGrabbed = getTriggerByName group "ALL GRABBED"
        let availableAgain = getTriggerByName group "AVAILABLE AGAIN"
        let xOpen = getTriggerByName group "OPEN"
        let xClose = getTriggerByName group "CLOSE"
        let numLocked = getTriggerByName group "NumLocked" :?> Mcu.McuCounter
        // Position of all nodes
        let refPoint = Vector2.FromMcu grab.Pos
        let dv = position - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // Pool size
        numLocked.Count <- size
        // result
        { Grab = grab
          Release = release
          AllGrabbed = allGrabbed
          AvailableAgain = availableAgain
          Open = xOpen
          Close = xClose
          All = groupFromList group
        }
