module SturmovikMission.Blocks.EventReporting

open SturmovikMission.DataProvider
open System.Numerics
open BlocksMissionData
open SturmovikMission.DataProvider.McuUtil
open VectorExtension

type EventReporting = {
    Name : string
    Trigger : Mcu.McuTrigger
    Disable : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    /// Create a group that reports a given event, e.g. arrival of a convoy at destination.
    /// This event can later be retrieved from the mission log. The mechanism that is used is destruction of a fake block, which can be identified by its name
    static member Create(store : NumericalIdentifiers.IdStore, country: Mcu.CountryValue, pos : Vector2, eventName : string) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("EventLogging").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let notifier = getVehicleByName group "Notification"
        let entity = getEntityByIndex notifier.LinkTrId group 
        let getByName = getTriggerByName group
        let leaderArrived = getByName "Trigger"
        let destroyed = getByName "Disable"
        let isAlive = getByName "IsEnabled"
        // Position of all nodes
        let refPoint = Vector2(float32 notifier.Pos.X, float32 notifier.Pos.Z)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // Move notification plates where they won't be noticed by players
        Vector2.Zero.AssignTo notifier.Pos
        Vector2.Zero.AssignTo entity.Pos
        // Result
        entity.Name <- eventName
        notifier.Name <- eventName
        notifier.Country <- Some country
        { Name = eventName
          Trigger = leaderArrived
          Disable = destroyed
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = group
                  member x.LcStrings = []
                  member x.SubGroups = []
            }
        }
