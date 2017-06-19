module SturmovikMission.Blocks.TransportFlight

open System.Numerics
open Vector
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData

type TransportFlight = {
    Start : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, takeOffPos : Vector2, takeOffDir : float32, destinationPos : Vector2, landingDir : float32, country) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Transport").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.RequestSpawn
        let takeOff = getTriggerByName group T.Blocks.TakeOff
        let destination = getTriggerByName group T.Blocks.Destination
        let cmdLand = getTriggerByName group T.Blocks.Land
        let plane1 = getVehicleByName group T.Blocks.Transport1
        let plane2 = getVehicleByName group T.Blocks.Transport2
        // Position of all nodes
        let refPoint = Vector2.FromMcu takeOff.Pos
        let dPos = takeOffPos - refPoint
        let dRot = takeOffDir - float32 takeOff.Ori.Y
        for mcu in group do
            ((Vector2.FromMcu mcu.Pos - refPoint).Rotate(dRot) + dPos).AssignTo(mcu.Pos)
            mcu.Ori.Y <- mcu.Ori.Y + float dRot
        // Position of destination and land order
        destinationPos.AssignTo destination.Pos
        destinationPos.AssignTo cmdLand.Pos
        cmdLand.Ori.Y <- float landingDir
        // Countries
        plane1.Country <- country
        plane2.Country <- country
        // result
        { Start = start
          All = McuUtil.groupFromList group
        }