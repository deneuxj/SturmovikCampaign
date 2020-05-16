module Escort

open SturmovikMission.Blocks.Escort
open SturmovikMission.Blocks.GroundAttack
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.McuInstantiation

open System.Numerics
open VectorExtension

open GroundAttack

let mkConfigFromGroup (group : T.GroupData) =
    let prefixData = extractPath group

    let toMeeting, rdv, toRTB =
        let rec procToMeeting (path : T.MCU_Waypoint list, toMeeting) =
            match path with
            | [] -> failwith "Failed to find meeting point"
            | wp :: rest ->
                if wp.GetName().Value.ToLowerInvariant() = "rdv" then
                    procMeeting (rest, toMeeting, wp)
                else
                    procToMeeting (rest, wp :: toMeeting)

        and procMeeting (path, toMeeting, rdv) =
            match path with
            | [] | [_] -> failwith "Failed to find path back to base"
            | wp1 :: wp2 :: rest ->
                procRTB (rest, toMeeting, rdv, [wp2; wp1])

        and procRTB (path, toMeeting, rdv, rtb) =
            match path with
            | [] -> List.rev toMeeting, rdv, List.rev rtb
            | wp :: rest -> procRTB (rest, toMeeting, rdv, wp :: rtb)
        procToMeeting (prefixData.Path, [])

    let flightToMeeting : FlightSectionConfig =
        match toMeeting with
        | wp :: _ ->
            {
                Altitude = int(wp.GetYPos().Value)
                Speed = int(wp.GetSpeed().Value)
                Prio = 1
                Path = toMeeting |> List.map (Vector2.FromPos)
            }
        | [] ->
            failwith "Path to RDV must have at least one waypoint"

    let meeting : MeetingPointConfig =
        let duration = rdv.GetDesc().Value |> System.Int32.TryParse |> function true, x -> x | _ -> 15
        {
            Location = DirectedPoint.FromMCU rdv
            Altitude = int(rdv.GetYPos().Value)
            MaxWaitDuration = (float32 duration) * 60.0f
        }

    let area : CoverArea =
        let cmd =
            group.ListOfMCU_CMD_AttackArea
            |> Seq.tryExactlyOne
        match cmd with
        | Some cmd ->
            let duration = cmd.GetDesc().Value |> System.Int32.TryParse |> function true, x -> x | _ -> 15
            {
                Center = DirectedPoint.FromMCU(cmd)
                Altitude = int(cmd.GetYPos().Value)
                Duration = (float32 duration) * 60.0f
            }
        | None ->
            failwith "No unique attack area identifying the area to patrol at the end of the escort"

    let returnFlight : FlightSectionConfig =
        match toRTB with
        | wp :: _ ->
            {
                Altitude = int(wp.GetYPos().Value)
                Speed = int(wp.GetSpeed().Value)
                Prio = 1
                Path = toRTB |> List.map (Vector2.FromPos)
            }
        | [] ->
            failwith "Path back to base must have at least one waypoint"

    let landAt =
        let cmd = group.ListOfMCU_CMD_Land |> Seq.tryExactlyOne
        match cmd with
        | Some cmd ->
            DirectedPoint.FromMCU cmd
        | None ->
            failwith "No unique land command identifying where to land"

    let numPlanes =
        prefixData.Plane.GetDesc().Value
        |> System.Int32.TryParse
        |> function true, x -> x | _ -> 3

    {
        StartType = prefixData.StartType
        StartPos = prefixData.StartPos
        FlightToRendezVous = flightToMeeting
        MeetingPoint = meeting
        Area = area
        FlightToReturn = returnFlight
        LandAt = landAt
        NumPlanes = numPlanes
    }, prefixData.Plane
