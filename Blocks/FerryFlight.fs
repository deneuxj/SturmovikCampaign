module SturmovikMission.Blocks.FerryFlight

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData

/// Logic for transfering a plane from one airfield to another.
// Includes a prio node for chaining multiple flights.
type FerryFlight = {
    TryStart : Mcu.McuTrigger
    Do : Mcu.McuTrigger
    Pass : Mcu.McuTrigger
    Next : Mcu.McuTrigger
    Enable : Mcu.McuTrigger
    Spawned : Mcu.McuTrigger
    Landed : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    Plane : Mcu.HasEntity
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, startPos : Vector2, destinationPos : Vector2, ori : float32, landingOri : float32, count : int, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("PlaneTransfer").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group "Try"
        let enable = getTriggerByName group "Enable"
        let doNode = getTriggerByName group "Do"
        let pass = getTriggerByName group "Pass"
        let next = getTriggerByName group "Next"
        let destination = getTriggerByName group "Destination"
        let cmdLand = getTriggerByName group "Land"
        let plane = getVehicleByName group "TransferPlane"
        let killed = getTriggerByName group "Killed"
        let arrived = getTriggerByName group "JustLanded"
        let counter = getTriggerByName group "StartCount" :?> Mcu.McuCounter
        let spawned = getTriggerByName group "Spawned"
        // Position of all nodes
        let refPoint = Vector2.FromMcu plane.Pos
        let dPos = startPos - refPoint
        for mcu in group do
            ((Vector2.FromMcu mcu.Pos - refPoint).Rotate(ori) + dPos + refPoint).AssignTo(mcu.Pos)
            mcu.Ori.Y <- mcu.Ori.Y + float ori
        // Plane properties
        plane.Pos.Y <- destination.Pos.Y
        plane.Country <- Some country
        // Position of destination and land order
        destinationPos.AssignTo destination.Pos
        destinationPos.AssignTo cmdLand.Pos
        cmdLand.Ori.Y <- float landingOri
        // Restart count
        counter.Count <- count
        // Return
        { TryStart = start
          Do = doNode
          Pass = pass
          Next = next
          Enable = enable
          Landed = arrived
          Killed = killed
          Spawned = spawned
          Plane = plane
          All = McuUtil.groupFromList group
        }
