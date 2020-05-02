module SturmovikMission.Blocks.GroundAttack

open System.Numerics
open VectorExtension
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
    Stop : Mcu.McuTrigger
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
        let start = getTriggerByName group "START"
        let stop = getTriggerByName group "STOP"
        let plane = getVehicleByName group "Plane"
        let ingress = getWaypointByName group "Ingress"
        let egress = getWaypointByName group "Egress"
        let exit = getWaypointByName group "Exit"
        let killed = getTriggerByName group "Killed"
        let spawned = getTriggerByName group "Spawned"
        let attack = getTriggerByName group "AttackGroundTargets"
        let respawnDelay = getTriggerByName group "RespawnDelay" :?> Mcu.McuTimer
        let attackDuration = getTriggerByName group "AttackDuration" :?> Mcu.McuTimer
        let landNode = getTriggerByName group "Land"
        let landDelay = getTriggerByName group "LandDelay" :?> Mcu.McuTimer
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
          Stop = stop
          Plane = plane
          Ingress = ingress
          Egress = egress
          Exit = exit
          AttackArea = attack
          Killed = killed
          Spawned = spawned
          All = McuUtil.groupFromList group
        }

    member this.StartDelay
        with get() =
            match this.Start with
            | :? Mcu.McuTimer as timer -> timer.Time
            | _ -> failwith "MCU START in GroundAttack must be a timer"
        and set(t) =
            match this.Start with
            | :? Mcu.McuTimer as timer -> timer.Time <- t
            | _ -> failwith "MCU START in GroundAttack must be a timer"

type DirectedPoint =
    { Pos : Vector2
      Direction : float32
    }

type StartType =
    | AirStart of {| Altitude : int |}
    | GroundStart of {| EnginesRunning : bool; TakeOffPoint : DirectedPoint |}

type AttackArea = {
    Center : Vector2
    Ingress : DirectedPoint
    AttackStart : DirectedPoint
    AttackAltitude : int
    Egress : DirectedPoint
}
with member this.DirectedPoint =
    { Pos = this.Center
      Direction = (this.Center - this.Ingress.Pos).YOri
    }

type AttackerGroupParams = {
    StartType : StartType
    StartPos : DirectedPoint
    CruiseAltitude : int
    CruiseSpeed : int
    RendezVous : DirectedPoint option
    MidPos : DirectedPoint
    PrimaryObjective : AttackArea
    SecondaryObjective : AttackArea option
    Return : DirectedPoint
    Final : DirectedPoint
    LandAt : Vector2
    NumPlanes : int
}

/// Move and rotate a group to a new location
let relocateGroup (newCenter : DirectedPoint) (group : #Mcu.McuBase seq * Mcu.McuBase) =
    let mcus, refMcu = group
    let refPos = Vector2.FromMcu refMcu.Pos
    let rotation = newCenter.Direction - float32 refMcu.Ori.Y
    for mcu in mcus do
        let r = (Vector2.FromMcu mcu.Pos) - refPos
        let r = newCenter.Pos + r.Rotate(rotation)
        r.AssignTo(mcu.Pos)

let setAltitude (altitude : int) (group : #Mcu.McuBase seq * Mcu.McuBase) =
    let mcus, _ = group
    for mcu in mcus do
        mcu.Pos.Y <- float altitude

type AttackerGroup = {
    Start : Mcu.McuTrigger
    EscortReady : Mcu.McuTrigger
    ReleaseEscort : Mcu.McuTrigger
    AllKilled : Mcu.McuTrigger
    LeadPlane : Mcu.HasEntity
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, mkPlanes : int * DirectedPoint * Vector2 -> Mcu.McuBase list, parameters : AttackerGroupParams) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("GroundAttack").CreateMcuList()
        for mcu in group do
            subst mcu

        // Nodes of interest
        let wpRDV = getWaypointByName group "RDV1"
        let wp2 = getWaypointByName group "Waypoint2"
        let wpIngress1 = getWaypointByName group "Ingress1"
        let wpStartAttack1 = getWaypointByName group "StartAttack1"
        let wpEgress1 = getWaypointByName group "Egress1"

        // groups of related nodes, each group centered around some key position
        let extractGroup groupName refName =
            group |> filterByPath [groupName],
            group |> filterByName refName |> Seq.head
        let centralGroup = extractGroup "GroundAttack" "Waypoint2"
        let primaryObjGroup = extractGroup "PrimaryObjective" "AttackArea"
        let secondaryObjGroup = extractGroup "SecondaryObjective" "AttackArea2"
        let meetEscortGroup = extractGroup "MeetEscort" "RDV1"
        let flightStartGroup = extractGroup "FlightStart" "TakeOff"
        let planeGroup = extractGroup "Plane" "ATTACKER"
        let returnGroup = extractGroup "Return" "Return"
        let landGroup = extractGroup "Land" "Land"

        // General relocation
        relocateGroup parameters.MidPos centralGroup
        relocateGroup parameters.PrimaryObjective.DirectedPoint primaryObjGroup
        parameters.SecondaryObjective |> Option.iter (fun p -> relocateGroup p.DirectedPoint secondaryObjGroup)
        parameters.RendezVous |> Option.iter (fun p -> relocateGroup p meetEscortGroup)
        match parameters.StartType with
        | AirStart x ->
            relocateGroup parameters.StartPos planeGroup
            relocateGroup parameters.StartPos flightStartGroup
            setAltitude x.Altitude planeGroup
        | GroundStart x ->
            relocateGroup parameters.StartPos planeGroup
            relocateGroup x.TakeOffPoint flightStartGroup
        relocateGroup parameters.StartPos planeGroup
        relocateGroup parameters.Return returnGroup
        relocateGroup { Pos = parameters.LandAt; Direction = parameters.Return.Direction } landGroup

        // Altitude
        for group in [centralGroup; meetEscortGroup; flightStartGroup; returnGroup] do
            setAltitude parameters.CruiseAltitude group
        setAltitude parameters.PrimaryObjective.AttackAltitude primaryObjGroup
        parameters.SecondaryObjective |> Option.iter (fun p -> setAltitude p.AttackAltitude secondaryObjGroup)

        // Detailed relocation
        // TODO

        // Set speeds

        // Cut off secondary objective logic if inactive
        // TODO

        // Remove placeholder plane
        // TODO

        // Set plane wing
        // TODO

        failwith "TODO"
