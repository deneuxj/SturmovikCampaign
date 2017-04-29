module SturmovikMission.Blocks.Patrol

open System.Numerics
open Vector
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.BlocksMissionData

/// A single fighter that flies a rectangular pattern for one hour, then respawns.
type Patrol = {
    Start : Mcu.McuTrigger
    Plane : Mcu.HasEntity
    P1 : Mcu.McuWaypoint
    P2 : Mcu.McuWaypoint
    P3 : Mcu.McuWaypoint
    P4 : Mcu.McuWaypoint
    Spawned : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, planeAlt : float32, p14 : Vector2, p23 : Vector2, breadth : float32) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("Patrol").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.DelayedStart
        let plane = getVehicleByName group T.Blocks.Plane
        let p1 = getWaypointByName group T.Blocks.P1
        let p2 = getWaypointByName group T.Blocks.P2
        let p3 = getWaypointByName group T.Blocks.P3
        let p4 = getWaypointByName group T.Blocks.P4
        let killed = getTriggerByName group T.Blocks.Killed
        let spawned = getTriggerByName group T.Blocks.Spawned
        // Position of all nodes
        let refPoint = Vector2.FromMcu(plane.Pos)
        let dv = p14 - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
            mcu.Pos.Y <- float planeAlt
        // Position waypoints
        let dir =
            let x = p23 - p14
            x / x.Length()
        let side = 0.5f * breadth * dir.Rotate(90.0f)
        (p14 + side).AssignTo(p1.Pos)
        (p14 - side).AssignTo(p4.Pos)
        (p23 + side).AssignTo(p2.Pos)
        (p23 - side).AssignTo(p3.Pos)
        // result
        { Start = start
          Plane = plane
          P1 = p1
          P2 = p2
          P3 = p3
          P4 = p4
          Killed = killed
          Spawned = spawned
          All = McuUtil.groupFromList group
        }
