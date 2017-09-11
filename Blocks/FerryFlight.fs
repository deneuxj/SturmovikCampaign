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
        let start = getTriggerByName group T.Blocks.Try
        let enable = getTriggerByName group T.Blocks.Enable
        let doNode = getTriggerByName group T.Blocks.Do
        let pass = getTriggerByName group T.Blocks.Pass
        let next = getTriggerByName group T.Blocks.Next
        let destination = getTriggerByName group T.Blocks.Destination
        let cmdLand = getTriggerByName group T.Blocks.Land
        let plane = getVehicleByName group T.Blocks.TransferPlane
        let killed = getTriggerByName group T.Blocks.Killed
        let arrived = getTriggerByName group T.Blocks.Landed
        let counter = getTriggerByName group T.Blocks.StartCount :?> Mcu.McuCounter
        let setCountry =
            let name =
                match country with
                | Mcu.CountryValue.Germany -> T.Blocks.SetGerman
                | Mcu.CountryValue.Russia -> T.Blocks.SetRussian
                | _ -> failwithf "unknown country value %d" (int country)
            getTriggerByName group name
        let spawned = getTriggerByName group T.Blocks.Spawned
        // When spawned -> set country
        // The aircraft is initially neutral so that it does not trigger the CZ that checks for occupancy of the spawn area.
        Mcu.addTargetLink spawned setCountry.Index
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
