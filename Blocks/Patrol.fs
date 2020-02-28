module SturmovikMission.Blocks.Patrol

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.WhileEnemyClose

/// A pair of fighters that fly a circular pattern for one hour, then respawns.
type Patrol = {
    Start : Mcu.McuTrigger
    Plane : Mcu.HasEntity
    Spawned : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    Completed : Mcu.McuTrigger
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
        let start = getTriggerByName group "DelayedStart"
        let plane = getVehicleByName group "Plane"
        let killed = getTriggerByName group "Killed"
        let spawned = getTriggerByName group "Spawned"
        let setGerman = getTriggerByName group "SetGerman"
        let setRussian = getTriggerByName group "SetRussian"
        let completed = getTriggerByName group "Completed"
        // Connection with wec
        let wakeup = getTriggerByName group "WakeUp"
        let sleep = getTriggerByName group "Sleep"
        let startMonitoring = getTriggerByName group "StartMonitoring"
        Mcu.addTargetLink wec.WakeUp wakeup.Index
        Mcu.addTargetLink wec.Sleep sleep.Index
        Mcu.addTargetLink startMonitoring wec.StartMonitoring.Index
        Mcu.addObjectLink wec.Proximity plane.LinkTrId
        // Set country upon spawning
        match coalition with
        | Mcu.CoalitionValue.Axis -> Mcu.addTargetLink spawned setGerman.Index
        | Mcu.CoalitionValue.Allies -> Mcu.addTargetLink spawned setRussian.Index
        | _ -> failwith "Unsupported coalition value"
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
          Completed = completed
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = group
                  member x.LcStrings = []
                  member x.SubGroups = [ wec.All ]
            }
        }
