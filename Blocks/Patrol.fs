module SturmovikMission.Blocks.Patrol

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.WhileEnemyClose

/// A single fighter that flies a rectangular pattern for one hour, then respawns.
type Patrol = {
    Start : Mcu.McuTrigger
    Plane : Mcu.HasEntity
    Spawned : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    WhileEnemyClose : WhileEnemyClose
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, pos : Vector2, planeAlt : float32, coalition : Mcu.CoalitionValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Patrol").CreateMcuList()
        for mcu in group do
            subst mcu
        let wec = WhileEnemyClose.Create(true, false, store, pos, coalition)
        // Get key nodes
        let start = getTriggerByName group T.Blocks.DelayedStart
        let plane = getVehicleByName group T.Blocks.Plane
        let killed = getTriggerByName group T.Blocks.Killed
        let spawned = getTriggerByName group T.Blocks.Spawned
        // Connection with wec
        let wakeup = getTriggerByName group T.Blocks.WakeUp
        let sleep = getTriggerByName group T.Blocks.Sleep
        let startMonitoring = getTriggerByName group T.Blocks.StartMonitoring
        Mcu.addTargetLink wec.WakeUp wakeup.Index
        Mcu.addTargetLink wec.Sleep sleep.Index
        Mcu.addTargetLink startMonitoring wec.StartMonitoring.Index
        Mcu.addObjectLink wec.Proximity plane.LinkTrId
        // Position of all nodes
        let refPoint = Vector2.FromMcu(plane.Pos)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
            mcu.Pos.Y <- float planeAlt
        // result
        { Start = start
          Plane = plane
          Killed = killed
          Spawned = spawned
          WhileEnemyClose = wec
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = group
                  member x.LcStrings = []
                  member x.SubGroups = [ wec.All ]
            }
        }
