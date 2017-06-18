module SturmovikMission.Blocks.GroundAttack

open System.Numerics
open Vector
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.BlocksMissionData

type OptionalLandOrder =
    | Land of Vector2 * float32
    | NoLanding

/// A single attacker that flies to an objective, attacks ground targets there and then flies to an exit point.
type Attacker = {
    Start : Mcu.McuTrigger
    Plane : Mcu.HasEntity
    Ingress : Mcu.McuWaypoint
    AttackArea : Mcu.McuTrigger
    Egress : Mcu.McuWaypoint
    Exit : Mcu.McuWaypoint
    Spawned : Mcu.McuTrigger
    Killed : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, pos : Vector2, planeAlt : float32, target : Vector2, landOrder : OptionalLandOrder) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("GroundAttack").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let start = getTriggerByName group T.Blocks.DelayedStart
        let plane = getVehicleByName group T.Blocks.Plane
        let ingress = getWaypointByName group T.Blocks.Ingress
        let egress = getWaypointByName group T.Blocks.Egress
        let exit = getWaypointByName group T.Blocks.Exit
        let killed = getTriggerByName group T.Blocks.Killed
        let spawned = getTriggerByName group T.Blocks.Spawned
        let attack = getTriggerByName group T.Blocks.AttackGroundTargets
        let respawnDelay = getTriggerByName group T.Blocks.RespawnDelay :?> Mcu.McuTimer
        let attackDuration = getTriggerByName group T.Blocks.AttackDuration :?> Mcu.McuTimer
        let landNode = getTriggerByName group T.Blocks.Land
        let landDelay = getTriggerByName group T.Blocks.LandDelay :?> Mcu.McuTimer
        // Respawn timing, depends on travel time
        respawnDelay.Time <-
            attackDuration.Time + 3.6 * 2.0 * (float <| (target - pos).Length()) / float ingress.Speed
        // Position of all nodes
        let refPoint = Vector2.FromMcu(plane.Pos)
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
            mcu.Pos.Y <- float planeAlt
        // Position waypoints
        let dir =
            let x = target - pos
            x / x.Length()
        target.AssignTo attack.Pos
        let ingressPos = target - 10000.0f * dir
        let egressPos = ingressPos - 10000.0f * dir.Rotate(90.0f)
        let exitPos =
            let rtbDir =
                let x = pos - egressPos
                x / x.Length()
            pos - 3000.0f * rtbDir
        ingressPos.AssignTo ingress.Pos
        egressPos.AssignTo egress.Pos
        exitPos.AssignTo exit.Pos
        match landOrder with
        | Land(pos, yori) ->
            pos.AssignTo landNode.Pos
            landNode.Ori.Y <- float yori
            landDelay.Time <- 5.0 * 60.0
            Mcu.addTargetLink exit landNode.Index
        | NoLanding ->
            ()
        // result
        { Start = start
          Plane = plane
          Ingress = ingress
          Egress = egress
          Exit = exit
          AttackArea = attack
          Killed = killed
          Spawned = spawned
          All = McuUtil.groupFromList group
        }

