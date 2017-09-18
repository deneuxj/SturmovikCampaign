module SturmovikMission.Blocks.Battlefield

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData

/// A respawning tank on a battlefield
type RespawningTank = {
    Start : Mcu.McuTrigger
    Tank : Mcu.HasEntity
    Destination : Mcu.McuWaypoint
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, startPos : Vector2, destinationPos : Vector2) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("RespawningTank").CreateMcuList()
        for mcu in group do
            subst mcu
        // Key nodes
        let start = McuUtil.getTriggerByName group T.Blocks.Start
        let tank = McuUtil.getVehicleByName group T.Blocks.HeavyTank
        let destination = McuUtil.getWaypointByName group T.Blocks.Destination
        // Position all nodes
        let refPos = Vector2.FromMcu tank.Pos
        let dv = startPos - refPos
        let dr = (destinationPos - startPos).YOri
        for mcu in group do
            let rel = Vector2.FromMcu(mcu.Pos) - refPos
            (rel.Rotate(dr) + startPos).AssignTo mcu.Pos
            mcu.Ori.Y <- mcu.Ori.Y + float dr
        // Return
        { Start = start
          Tank = tank
          Destination = destination
          All = McuUtil.groupFromList group
        }

/// A respawning canon on a battlefield
type RespawningCanon = {
    Start : Mcu.McuTrigger
    Canon : Mcu.HasEntity
    Wall : Mcu.HasEntity
    Target : Mcu.McuAttackArea
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, startPos : Vector2, targetPos : Vector2) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("RespawningArtillery").CreateMcuList()
        for mcu in group do
            subst mcu
        // Key nodes
        let start = McuUtil.getTriggerByName group T.Blocks.Start
        let canon = McuUtil.getVehicleByName group T.Blocks.Gun
        let wall = McuUtil.getVehicleByName group T.Blocks.Wall
        let target = McuUtil.getTriggerByName group T.Blocks.AttackArea :?> Mcu.McuAttackArea
        // Position all nodes
        let refPos = Vector2.FromMcu canon.Pos
        let dv = startPos - refPos
        let dr = (targetPos - startPos).YOri
        for mcu in group do
            let rel = Vector2.FromMcu(mcu.Pos) - refPos
            (rel.Rotate(dr) + startPos).AssignTo mcu.Pos
            mcu.Ori.Y <- mcu.Ori.Y + float dr
        // Return
        { Start = start
          Canon = canon
          Wall = wall
          Target = target
          All = McuUtil.groupFromList group
        }
