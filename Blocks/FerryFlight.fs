module SturmovikMission.Blocks.FerryFlight

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData

type FerryFlight = {
    Start : Mcu.McuTrigger
    Landed : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    Plane : Mcu.HasEntity
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, startPos : Vector2, destinationPos : Vector2, ori : float32, landingOri : float32, count : int) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("PlaneTransfer").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.Start
        let destination = getTriggerByName group T.Blocks.Destination
        let cmdLand = getTriggerByName group T.Blocks.Land
        let plane = getVehicleByName group T.Blocks.TransferPlane
        let killed = getTriggerByName group T.Blocks.Killed
        let arrived = getTriggerByName group T.Blocks.Landed
        let counter = getTriggerByName group T.Blocks.StartCount :?> Mcu.McuCounter
        // Position of all nodes
        let refPoint = Vector2.FromMcu plane.Pos
        let dPos = startPos - refPoint
        for mcu in group do
            ((Vector2.FromMcu mcu.Pos - refPoint).Rotate(ori) + dPos + refPoint).AssignTo(mcu.Pos)
            mcu.Ori.Y <- mcu.Ori.Y + float ori
        // Position of destination and land order
        destinationPos.AssignTo destination.Pos
        destinationPos.AssignTo cmdLand.Pos
        cmdLand.Ori.Y <- float landingOri
        // Restart count
        counter.Count <- count
        // Return
        { Start = start
          Landed = arrived
          Killed = killed
          Plane = plane
          All = McuUtil.groupFromList group
        }
