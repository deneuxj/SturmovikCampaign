module SturmovikMission.Blocks.TransportFlight

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.IconDisplay
open SturmovikMission.Blocks.EventReporting

type TransportFlight = {
    Start : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    Arrived : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, takeOffPos : Vector2, takeOffDir : float32, destinationPos : Vector2, landingDir : float32, country, eventName) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Transport").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.RequestSpawn
        let destination = getTriggerByName group T.Blocks.Destination
        let cmdLand = getTriggerByName group T.Blocks.Land
        let plane1 = getVehicleByName group T.Blocks.Transport1
        let killed = getTriggerByName group T.Blocks.Killed
        let arrived = getTriggerByName group T.Blocks.Landed
        // Position of all nodes
        let refPoint = Vector2.FromMcu plane1.Pos
        let dPos = takeOffPos - refPoint
        let dRot = takeOffDir - float32 plane1.Ori.Y
        for mcu in group do
            ((Vector2.FromMcu mcu.Pos - refPoint).Rotate(dRot) + dPos + refPoint).AssignTo(mcu.Pos)
            mcu.Ori.Y <- mcu.Ori.Y + float dRot
        // Position of destination and land order
        destinationPos.AssignTo destination.Pos
        destinationPos.AssignTo cmdLand.Pos
        cmdLand.Ori.Y <- float landingDir
        // Countries
        plane1.Country <- country
        // Icons
        let coalition =
            match country with
            | Mcu.CountryValue.Germany -> Mcu.CoalitionValue.Axis
            | Mcu.CountryValue.Russia -> Mcu.CoalitionValue.Allies
            | _ -> failwith "Unsupported country value"
        let iconAttack, iconCover = IconDisplay.CreatePair(store, lcStore, 0.5f * (takeOffPos + destinationPos), "Transport", coalition, Mcu.IconIdValue.CoverBombersFlight)
        // Events
        let startEventName = sprintf "%s-D-0" eventName
        let startEvent = EventReporting.Create(store, country, takeOffPos, startEventName)
        let arrivedEventName = sprintf "%s-A-0" eventName
        let arrivedEvent = EventReporting.Create(store, country, takeOffPos + Vector2(0.0f, 100.0f), arrivedEventName)
        let destroyedEventName = sprintf "%s-K-0" eventName
        let destroyedEvent = EventReporting.Create(store, country, takeOffPos + Vector2(0.0f, 200.0f), destroyedEventName)
        // Connections to icons
        for icon in [ iconAttack; iconCover] do
            Mcu.addTargetLink start icon.Show.Index
            Mcu.addTargetLink killed icon.Hide.Index
            Mcu.addTargetLink arrived icon.Hide.Index
        // Connections to events
        Mcu.addTargetLink start startEvent.Trigger.Index
        Mcu.addTargetLink arrived arrivedEvent.Trigger.Index
        Mcu.addTargetLink killed destroyedEvent.Trigger.Index
        // result
        let all =
            { new IMcuGroup with
                  member x.Content = group
                  member x.LcStrings = []
                  member x.SubGroups = [ iconAttack.All; iconCover.All; startEvent.All; arrivedEvent.All ]
            }
        { Start = start
          Killed = killed
          Arrived = arrived
          All = all
        }