/// An AI group that takes off, flies to a rendez-vous point, meets other planes, escort them to an
/// area, covers the area, then flies home

module SturmovikMission.Blocks.Escort

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.McuInstantiation
open SturmovikMission.Blocks.GroundAttack

type CoverArea = {
    Center : DirectedPoint
    Altitude : int
    Duration : float32
}

type EscortConfig = {
    StartType : StartType
    StartPos : DirectedPoint
    FlightToRendezVous : FlightSectionConfig
    MeetingPoint : MeetingPointConfig
    Area : CoverArea
    FlightToReturn : FlightSectionConfig
    LandAt : DirectedPoint
    NumPlanes : int
}

type EscortGroup(store, config : EscortConfig) =
    inherit PlaneWingReplacement()

    // Instantiate
    let mcus, subst = getFreshGroup blocks2Data store "Escort"

    // nodes of interest
    let start = getTriggerByName mcus "START"
    let coverArea = getTriggerByName mcus "COVER_AREA"
    let setFree = getTriggerByName mcus "SET_FREE"
    let allUnable = getTriggerByName mcus "ALL_UNABLE" :?> Mcu.McuCounter
    let rdv = getWaypointByName mcus "RDV"
    let leadPlane = getVehicleByName mcus "ESCORT"
    let leadPlaneEntity = getEntityByIndex leadPlane.LinkTrId mcus
    let retWp1 = getWaypointByName mcus "Waypoint2"
    let retWp2 = getWaypointByName mcus "Waypoint3"
    let coverAreaDuration = getTriggerByName mcus "CoverDuration" :?> Mcu.McuTimer
    let wp1 = getWaypointByName mcus "Waypoint1"
    let takeOff = getTriggerByName mcus "TakeOff"
    let follow = getTriggerByName mcus "FOLLOW_ESCORTED"
    let escortCmd = getTriggerByName mcus "ESCORT_CMD"

    // groups of related nodes
    let extractGroup = extractGroup mcus
    let coverTargetGroup = extractGroup "CoverTarget" "CoverArea"
    let returnGroup = extractGroup "FlyBack" "Waypoint2"
    let landGroup = extractGroup "Land" "Land"
    let takeOffGroup = extractGroup "TakeOff" "TakeOff"
    let planeGroup = extractGroup "Plane" "ESCORT"

    let rtbPoint =
        { Direction = 0.0f
          Pos = List.head config.FlightToReturn.Path
        }
    let rtbPoint2 =
        { Direction = 0.0f
          Pos = List.head (List.tail config.FlightToReturn.Path)
        }

    // general node relocation
    do
        relocateGroup config.MeetingPoint.Location (mcus, rdv)
        setAltitude config.MeetingPoint.Altitude mcus
        match config.StartType with
        | AirStart x ->
            relocateGroup config.StartPos planeGroup
            relocateGroup config.StartPos takeOffGroup
            setAltitude x.Altitude (fst planeGroup)
        | GroundStart x ->
            relocateGroup config.StartPos planeGroup
            relocateGroup x.TakeOffPoint takeOffGroup
        relocateGroup config.StartPos planeGroup
        relocateGroup rtbPoint returnGroup
        relocateGroup config.LandAt landGroup
        relocateGroup config.Area.Center coverTargetGroup
        setAltitude config.Area.Altitude (fst coverTargetGroup)

    // waypoints to rendez-vous
    let toRdv = config.FlightToRendezVous.CreateWaypoints(store, leadPlaneEntity.Index)
    do gatherInNamedGroup store "To RDV" toRdv

    // waypoints to home base
    let flightToReturn =
        { config.FlightToReturn with
            Path = config.FlightToReturn.Path |> List.skip 2
        }
    let toBase = flightToReturn.CreateWaypoints(store, leadPlaneEntity.Index)
    do gatherInNamedGroup store "Back to base" toBase

    // Detailed node relocation
    do
        rtbPoint.Pos.AssignTo retWp1.Pos
        retWp1.Pos.Y <- float config.FlightToReturn.Altitude
        rtbPoint2.Pos.AssignTo retWp2.Pos
        retWp2.Pos.Y <- float config.FlightToReturn.Altitude
        coverAreaDuration.Time <- float config.Area.Duration

    // Logic connection
    let cx = Mcu.addTargetLink
    //  Take-off or fly to first waypoint
    let onTookOff = getTriggerByName (fst takeOffGroup) "TookOff"
    do
        match config.StartType with
        | AirStart _ -> cx start onTookOff.Index
        | GroundStart _ ->
            cx start takeOff.Index
            let planeTookOff = getTriggerByName (fst planeGroup) "TookOff"
            cx planeTookOff onTookOff.Index
    //  Escort meet-up
    do
        cx wp1 (List.head toRdv).Index
        cx (List.last toRdv) rdv.Index
    //  Return to base
    let finalWp = getWaypointByName (fst landGroup) "Final"
    do
        cx retWp2 (List.head toBase).Index
        cx (List.last toBase) finalWp.Index

    // Set wing
    let planeGroup = fst planeGroup
    do allUnable.Count <- config.NumPlanes

    let unable = getTriggerByName planeGroup "Unable"
    do cx unable allUnable.Index

    let wing =
        let newGroup() = cloneFresh store planeGroup
        [|
            let offset =
                match config.StartType with
                | AirStart _ -> Vector2(-50.0f, 50.0f).Rotate(config.StartPos.Direction)
                | GroundStart _ -> Vector2(0.0f, 35.0f).Rotate(config.StartPos.Direction)
            for i in 2..config.NumPlanes do
                let group = newGroup()
                let vehicle = getVehicleByName group "ESCORT"
                let unable = getTriggerByName group "Unable"
                let entity = getEntityByIndex vehicle.LinkTrId group
                for mcu in group do
                    let pos = Vector2.FromMcu(mcu.Pos)
                    let newPos = pos + (float32 (i - 1)) * offset
                    newPos.AssignTo mcu.Pos
                vehicle.Name <- sprintf "ESCORT %d" i
                vehicle.NumberInFormation.Value.Number <- i - 1
                cx entity leadPlaneEntity.Index
                cx unable allUnable.Index
                gatherInNamedGroup store (sprintf "Wingman %d" (i - 1)) group
                yield vehicle, group
        |]

    let wingPlanes =
        wing
        |> Array.map fst
        |> Array.append [| leadPlane |]

    let replaceables =
        seq {
            for plane in wingPlanes do
                yield plane.Index
        }
        |> Set

    let getReplacements() : Mcu.McuBase list=
        [
            for plane in wingPlanes do
                yield plane
        ]

    let group =
        [
            yield! mcus
            for _, group in wing do
                yield! group
            for wp in toRdv do yield upcast wp
            for wp in toBase do yield upcast wp
        ]

    /// IN
    member this.Start = start

    /// IN: Triggered by attackers during ingress
    member this.CoverArea = coverArea

    /// IN: Triggered by attackers after first attack
    member this.ReleaseEscort = setFree

    /// IN: Triggered by attackers after successful RDV
    member this.FollowEscorted = follow

    /// OUT
    member this.AtRDV = rdv

    /// OUT
    member this.Unable = allUnable

    /// OBJ: Set object to lead plane of group to escort
    member this.EscortCmd = escortCmd

    member this.LeadPlane = wingPlanes.[0]

    member this.All =
        groupFromList group
        |> mcuGroupWithReplaceables(replaceables, getReplacements)

    override this.Planes = wingPlanes


let connectEscortWithPlanes (escort : EscortGroup) (planes : AttackerGroup) =
    Mcu.addObjectLink escort.EscortCmd planes.LeadPlane.LinkTrId
    let cx = Mcu.addTargetLink
    cx planes.EscortStandBy escort.CoverArea.Index
    cx planes.ReleaseEscort escort.ReleaseEscort.Index
    match planes.MeetWithEscort with
    | Some meet ->
        cx meet.Proceed escort.FollowEscorted.Index
        cx escort.AtRDV meet.OtherArrived.Index
    | None ->
        failwith "Attack group lacks a rendezvous point with the escort"