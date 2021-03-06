﻿module SturmovikMission.Blocks.GroundAttack

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.McuInstantiation

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
    static member Create(store : NumericalIdentifiers.IdStore, lcStore, pos : Vector2, planeAlt : float32, cruiseSpeed : int, target : Vector2, landOrder : OptionalLandOrder) =
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
            let takeOffAndLandingTime = 900.0 // 15min
            takeOffAndLandingTime + attackDuration.Time + 3.6 * 2.0 * (float <| (target - pos).Length()) / float cruiseSpeed
        // Speeds
        ingress.Speed <- cruiseSpeed
        egress.Speed <- cruiseSpeed
        exit.Speed <- cruiseSpeed
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

type StartType =
    | AirStart of {| Altitude : int |}
    | GroundStart of {| EnginesRunning : bool; TakeOffPoint : DirectedPoint |}

type AttackAreaParams = {
    Center : Vector2
    Ingress : DirectedPoint
    AttackStart : DirectedPoint
    AttackAltitude : int
    Egress : DirectedPoint
}
with
    member this.DirectedPoint =
        { Pos = this.Center
          Direction = (this.Center - this.Ingress.Pos).YOri
        }

type AttackArea = {
    /// OBJ, IN
    Ingress : Mcu.McuWaypoint
    /// OBJ, OUT
    StartAttack : Mcu.McuWaypoint
    /// OBJ
    AttackArea : Mcu.McuAttackArea
    /// IN: trigger this when a wing is unable to drop more bombs
    AttackDone : Mcu.McuCounter
    /// OBJ, OUT
    Egress : Mcu.McuWaypoint
    /// OBJ
    FormationNone : Mcu.McuTrigger
    /// OBJ
    FormationSafe : Mcu.McuTrigger
    /// OUT
    AfterAttack : Mcu.McuTrigger
    /// OUT
    Canceled : Mcu.McuTrigger
    /// IN
    Start : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, config : AttackAreaParams) =
        // Instantiate
        let mcus, subst = getFreshGroup blocks2Data store "GroundAreaAttack"

        // nodes of interest
        let ingress = getWaypointByName mcus "INGRESS"
        let startAttack = getWaypointByName mcus "START_ATTACK"
        let attackArea = getTriggerByName mcus "ATTACK_AREA" :?> Mcu.McuAttackArea
        let attackDone = getTriggerByName mcus "ATTACK_DONE" :?> Mcu.McuCounter
        let egress = getWaypointByName mcus "EGRESS"
        let formationNone = getTriggerByName mcus "FORMATION_NONE"
        let formationSafe = getTriggerByName mcus "FORMATION_SAFE"
        let start = getTriggerByName mcus "START"
        let afterAttack = getTriggerByName mcus "AFTER_ATTACK"
        let canceled = getTriggerByName mcus "ATTACK_CANCELED"
        // general node relocation
        relocateGroup config.DirectedPoint (mcus, attackArea)
        setAltitude config.AttackAltitude mcus

        // specific node relocation
        config.Ingress.AssignTo ingress
        config.AttackStart.AssignTo startAttack
        config.Egress.AssignTo egress

        {
            Ingress = ingress
            StartAttack = startAttack
            AttackArea = attackArea
            AttackDone = attackDone
            Egress = egress
            FormationNone = formationNone
            FormationSafe = formationSafe
            Start = start
            AfterAttack = afterAttack
            Canceled = canceled
            All = McuUtil.groupFromList mcus
        }

    member this.ConnectTo(entity : Mcu.McuEntity) =
        for node in this.All.Content do
            match node with
            | :? Mcu.McuWaypoint as wp -> Mcu.addObjectLink wp entity.Index
            | _ -> ()
        for trigger in [this.FormationNone; this.FormationSafe; upcast this.AttackArea] do
            Mcu.addObjectLink trigger entity.Index

type FlightSectionConfig = {
    Altitude : int
    Speed : int
    Prio : int
    Path : Vector2 list
}
with
    static member Create(wps : T.MCU_Waypoint list) =
        let alt = List.tryHead wps |> Option.map (fun wp -> wp.GetYPos().Value |> int) |> Option.defaultValue 2000
        let speed = List.tryHead wps |> Option.map (fun wp -> wp.GetSpeed().Value |> int) |> Option.defaultValue 300
        {
            Altitude = alt
            Speed = speed
            Prio = 1
            Path = wps |> List.map (Vector2.FromPos)
        }

    /// Create list of McuWaypoint, ordered as in Path
    member this.CreateWaypoints(store, objIdx) =
        if this.Path.Length > 9999 then
            failwith "Too many waypoints in the path"
        let wps =
            [
                for i, v in List.indexed this.Path do
                    let wp =
                        T.MCU_Waypoint.Default
                            .SetName(T.String.N (sprintf "WP %04d" i))
                            .SetArea(T.Integer.N 1000)
                            .SetSpeed(T.Integer.N this.Speed)
                            .SetYPos(T.Float.N (float this.Altitude))
                            .SetXPos(T.Float.N (float v.X))
                            .SetZPos(T.Float.N (float v.Y))
                            .SetPriority(T.Integer.N this.Prio)
                            .SetIndex(T.Integer.N (i + 1))
                    yield wp.CreateMcu()
            ]
        for wp1, wp2 in Seq.pairwise wps do
            Mcu.addTargetLink (wp1 :?> Mcu.McuTrigger) wp2.Index
        cloneFresh store wps
        |> List.map (fun x ->
            let wp = x :?> Mcu.McuWaypoint
            wp.Objects <- [objIdx]
            wp)
        |> List.sortBy (fun x -> x.Name)

let waypointsToGroup (wps : Mcu.McuWaypoint seq) =
    wps
    |> Seq.map (fun x -> x :> Mcu.McuBase)
    |> List.ofSeq
    |> McuUtil.groupFromList

type MeetingPointConfig = {
    Location : DirectedPoint
    Altitude : int
    MaxWaitDuration : float32
}

type MeetingPoint = {
    /// IN
    Start : Mcu.McuTrigger
    /// OBJ
    MeetAt : Mcu.McuWaypoint
    /// IN
    OtherArrived : Mcu.McuTrigger
    /// OBJ
    FormationDense : Mcu.McuTrigger
    /// OUT
    Proceed : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store, config : MeetingPointConfig) =
        // Instantiation
        let mcus, subst = getFreshGroup blocks2Data store "MeetingPoint"

        // Nodes of interest
        let start = getTriggerByName mcus "START"
        let meetAt = getWaypointByName mcus "RDV"
        let otherArrived = getTriggerByName mcus "ESCORT_READY"
        let timeout = getTriggerByName mcus "MaxWait" :?> Mcu.McuTimer
        let proceed = getTriggerByName mcus "PROCEED"
        let formation = getTriggerByName mcus "FORMATION_DENSE"
        // Relocation
        relocateGroup config.Location (mcus, meetAt)
        setAltitude config.Altitude mcus
        // Result
        timeout.Time <- float config.MaxWaitDuration
        {
            Start = start
            MeetAt = meetAt
            OtherArrived = otherArrived
            FormationDense = formation
            Proceed = proceed
            All = McuUtil.groupFromList mcus
        }

    member this.ConnectTo(entity : Mcu.McuEntity) =
        for node in this.All.Content do
            match node with
            | :? Mcu.McuWaypoint as wp -> Mcu.addObjectLink wp entity.Index
            | _ -> ()
        Mcu.addObjectLink this.FormationDense entity.Index


type AttackerGroupConfig = {
    StartType : StartType
    StartPos : DirectedPoint
    ToRendezVous : FlightSectionConfig option
    RendezVous : MeetingPointConfig option
    ToPrimaryObjective : FlightSectionConfig
    PrimaryObjective : AttackAreaParams
    ToSecondaryObjective : FlightSectionConfig option
    SecondaryObjective : AttackAreaParams option
    ToReturn : FlightSectionConfig
    LandAt : DirectedPoint
    NumPlanes : int
}

type AttackerGroup(store : NumericalIdentifiers.IdStore, config : AttackerGroupConfig) =
    inherit PlaneWingReplacement()

    // Instantiate
    let group, subst = getFreshGroup blocks2Data store "GroundAttack"

    // Subgroups
    let attack1 = AttackArea.Create(store, config.PrimaryObjective)
    do attack1.All.PushGroupName(store, "Primary objective")
    let attack2 = config.SecondaryObjective |> Option.map (fun o -> AttackArea.Create(store, o))
    do attack2 |> Option.iter (fun attack2 -> attack2.All.PushGroupName(store, "Secondary objective"))
    let meeting = config.RendezVous |> Option.map (fun rdv -> MeetingPoint.Create(store, rdv))
    do
        meeting
        |> Option.iter (fun meeting ->
            meeting.OtherArrived.Name <- "EscortReady"
            meeting.All.PushGroupName(store, "RendezVous"))

    // Nodes of interest
    let wp1 = getWaypointByName group "Waypoint1"
    let start = getTriggerByName group "START"
    let escortReady = getTriggerByName group "ESCORT_READY"
    let releaseEscort = getTriggerByName group "RELEASE_ESCORT"
    let escortStdBy = getTriggerByName group "ESCORT_STDBY"
    let intoSecondary = getTriggerByName group "INTO_SECONDARY" :?> Mcu.McuTimer
    let unableToAttack = getTriggerByName group "GROUP_UNABLE_TO_ATT" :?> Mcu.McuCounter
    let greenFlare = getTriggerByName group "GreenFlare"
    let allKilled = getTriggerByName group "ALL_KILLED" :?> Mcu.McuCounter
    let planeVehicle = getVehicleByName group "ATTACKER"
    let plane = getEntityByIndex planeVehicle.LinkTrId group
    let takeOff = getTriggerByName group "TakeOff"
    let chooseSecOrReturn = getTriggerByName group "ChooseSecOrReturn"
    let final = getTriggerByName group "Final"

    // groups of related nodes, each group centered around some key position
    let extractGroup = extractGroup group
    let flightStartGroup = extractGroup "FlightStart" "TakeOff"
    let planeGroup = extractGroup "Plane" "ATTACKER"
    let returnGroup = extractGroup "Return" "Return"
    let flyBack = getTriggerByName (fst returnGroup) "Return"
    let landGroup = extractGroup "Land" "Land"

    // General relocation
    do
        try
            let rest =
                let bag =
                    [flightStartGroup; planeGroup; returnGroup; landGroup]
                    |> Seq.map fst
                    |> Seq.collect (Seq.map (fun mcu -> mcu.Index))
                    |> Set
                let mcus =
                    group
                    |> List.filter (fun mcu -> not(bag.Contains(mcu.Index)))
                let refPoint =
                    getTriggerByName mcus "START"
                mcus, refPoint
            relocateGroup { config.StartPos with Pos = config.StartPos.Pos + Vector2(0.0f, 300.0f) } rest
        with _ -> ()
        match config.StartType with
        | AirStart x ->
            relocateGroup config.StartPos planeGroup
            relocateGroup config.StartPos flightStartGroup
            setAltitude x.Altitude (fst planeGroup)
        | GroundStart x ->
            relocateGroup config.StartPos planeGroup
            relocateGroup x.TakeOffPoint flightStartGroup
        relocateGroup config.StartPos planeGroup
        relocateGroup config.LandAt landGroup

    // Flight sections
    let toRdv = config.ToRendezVous |> Option.map(fun x -> x.CreateWaypoints(store, plane.Index))
    let toPrimary = config.ToPrimaryObjective.CreateWaypoints(store, plane.Index)
    let toSecondary = config.ToSecondaryObjective |> Option.map (fun x -> x.CreateWaypoints(store, plane.Index))
    let toBase = config.ToReturn.CreateWaypoints(store, plane.Index)
    let intoPrimary = List.last toPrimary
    // Set up connections between subgroups
    let cx = Mcu.addTargetLink
    //  Take-off or fly to first waypoint
    do
        match config.StartType with
        | AirStart _ -> cx start wp1.Index
        | GroundStart _ -> cx start takeOff.Index

    //  Escort meet-up
    do
        match meeting with
        | Some meeting ->
            match toRdv with
            | Some (wp :: _ as toRdv) ->
                cx wp1 wp.Index
                cx (List.last toRdv) meeting.MeetAt.Index
            | _ ->
                cx wp1 meeting.MeetAt.Index
            cx start meeting.Start.Index
            cx attack1.StartAttack escortStdBy.Index
            cx attack1.AfterAttack releaseEscort.Index
            cx attack1.Canceled releaseEscort.Index
            cx escortReady meeting.OtherArrived.Index
            cx meeting.Proceed intoPrimary.Index
        | None ->
            cx wp1 intoPrimary.Index
        //  Primary objective
        cx start attack1.Start.Index
        cx attack1.AfterAttack greenFlare.Index
        cx unableToAttack attack1.AttackDone.Index
        cx intoPrimary attack1.Ingress.Index
        match toSecondary with
        | Some (wp :: _ as toSecondary) ->
            cx attack1.Egress wp.Index
            cx (List.last toSecondary) chooseSecOrReturn.Index
        | _ ->
            cx attack1.Egress chooseSecOrReturn.Index
        //  Secondary objective
        match attack2 with
        | Some attack2 ->
            cx start attack2.Start.Index
            cx attack2.AfterAttack greenFlare.Index
            cx unableToAttack attack2.AttackDone.Index
            cx attack2.Egress (List.head toBase).Index
            cx intoSecondary attack2.Ingress.Index
        | None ->
            // Break connection between waypoint and timer
            chooseSecOrReturn.Targets <- chooseSecOrReturn.Targets |> List.filter ((<>) intoSecondary.Index)
        //  Flight back to base
        match toBase with
        | [] ->
            cx flyBack final.Index
        | wp :: _ ->
            cx flyBack wp.Index
            cx (List.last toBase) final.Index

        //  Release escort if group dies
        cx allKilled releaseEscort.Index

    // Set plane wing
    do
        allKilled.Count <- config.NumPlanes
        unableToAttack.Count <- config.NumPlanes

        attack1.ConnectTo plane
        attack2 |> Option.iter (fun attack -> attack.ConnectTo plane)
        meeting |> Option.iter (fun meeting -> meeting.ConnectTo plane)

    let isDead = getTriggerByName group "Dead"
    let isBingoBombs = getTriggerByName group "BingoBombs"
    let isDone = getTriggerByName group "UnableToAttack"
    do
        cx isDead allKilled.Index
        cx isDone unableToAttack.Index
    let wing =
        let items : Mcu.McuBase list =
            [planeVehicle; plane; isDead; isBingoBombs; isDone]
        let newGroup() = cloneFresh store items
        [|
            let offset =
                match config.StartType with
                | AirStart _ -> Vector2(-50.0f, 50.0f).Rotate(config.StartPos.Direction)
                | GroundStart _ -> Vector2(0.0f, 35.0f).Rotate(config.StartPos.Direction)
            for i in 2..config.NumPlanes do
                let group = newGroup()
                let dead = getTriggerByName group "Dead"
                let unable = getTriggerByName group "UnableToAttack"
                let vehicle = getVehicleByName group "ATTACKER"
                let entity = getEntityByIndex vehicle.LinkTrId group
                for mcu in group do
                    let pos = Vector2.FromMcu(mcu.Pos)
                    let newPos = pos + (float32 (i - 1)) * offset
                    newPos.AssignTo mcu.Pos
                vehicle.Name <- sprintf "ATTACKER %d" i
                vehicle.NumberInFormation.Value.Number <- i - 1
                cx entity plane.Index
                cx dead allKilled.Index
                cx unable unableToAttack.Index
                gatherInNamedGroup store (sprintf "Wingman %d" (i - 1)) group
                yield vehicle, group
        |]

    let wingPlanes =
        wing
        |> Array.map fst
        |> Array.append [| planeVehicle |]

    let replaceables =
        seq {
            for plane in wingPlanes do
                yield plane.Index
        }
        |> Set

    let getReplacement(mcu : Mcu.McuBase) : Mcu.McuBase =
        if replaceables.Contains(mcu.Index) then
            wingPlanes
            |> Array.find (fun plane -> plane.Index = mcu.Index)
            :> Mcu.McuBase
        else
            mcu

    let group =
        [
            yield! group
            for _, group in wing do
                yield! group

            match toRdv with
            | Some wps ->
                for wp in wps do yield upcast wp
            | None -> ()

            for wp in toPrimary do yield upcast wp

            match toSecondary with
            | Some wps ->
                for wp in wps do yield upcast wp
            | None -> ()

            for wp in toBase do yield upcast wp
        ]

    /// IN
    member this.Start = start

    /// OUT
    member this.ReleaseEscort = releaseEscort

    /// OUT
    member this.AllKilled = allKilled

    /// OUT
    member this.EscortStandBy = escortStdBy

    /// IN
    member this.OtherAtRDV = meeting |> Option.map (fun m -> m.OtherArrived)

    /// OUT
    member this.ProceedAfterRDV = meeting |> Option.map (fun m -> m.Proceed)

    member this.LeadPlane = wingPlanes.[0]
 
    member this.PrimaryAttackArea = attack1

    member this.SecondaryAttackArea = attack2

    member this.MeetWithEscort = meeting

    member this.All = 
        { new McuUtil.IMcuGroup with
            member this.Content = group
            member this.LcStrings = []
            member this.SubGroups = [
                yield attack1.All
                match attack2 with
                | Some attack2 -> yield attack2.All
                | None -> ()
                match meeting with
                | Some meeting -> yield meeting.All
                | None -> ()
            ]
        }
        |> mcuGroupWithReplaceables getReplacement

    override this.Planes = wingPlanes