module GroundAttack

open SturmovikMission.Blocks.GroundAttack
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.McuInstantiation

open System.Numerics
open VectorExtension

let infoWp (wp : T.MCU_Waypoint) =
    sprintf "waypoint '%s' (ID %d)" (wp.GetName().Value) (wp.GetIndex().Value)

type PathPrefixData = {
    Plane : T.Plane
    StartPos : DirectedPoint
    StartType : StartType
    Path : T.MCU_Waypoint list
}

let extractPath (group : T.GroupData) =
    let plane =
        let x = group.ListOfPlane |> List.ofSeq
        match x with
        | [subject] -> subject
        | [] -> failwith "Group does not have a plane"
        | _ -> failwith "Group has too many planes"
    let startPos = DirectedPoint.FromMCU(plane)
    let (|OnGround|InAir|Unknown|) =
        function
        | 0 -> InAir
        | 1 -> OnGround true
        | 2 -> OnGround false
        | _ -> Unknown
    let startType =
        match plane.GetStartInAir().Value with
        | InAir ->
            AirStart {| Altitude = plane.GetYPos().Value |> int |}
        | OnGround running ->
            let takeoff =
                let x = group.ListOfMCU_CMD_TakeOff |> List.ofSeq
                match x with
                | [x] -> x
                | [] -> failwith "Missing Command:Take off MCU"
                | _ -> failwith "Too many Command:Take off MCUs, there must be exactly one"
            GroundStart {| EnginesRunning = running; TakeOffPoint = DirectedPoint.FromMCU(takeoff) |}
        | x -> failwithf "Invalid air/ground start value %d" x
    let waypoints =
        group.ListOfMCU_Waypoint
        |> List.ofSeq
    if List.isEmpty waypoints then
        failwith "There must be at least one Trigger:Waypoint MCU. Make sure the waypoint has the right altitude (e.g 3000m), speed (e.g. 300kph) and radius (e.g. 1000m)"
    let path =
        let isWaypoint =
            let items =
                waypoints
                |> Seq.map (fun wp -> wp.GetIndex().Value)
                |> Set
            items.Contains
        let last =
            waypoints
            |> List.tryFind (fun wp ->
                wp.GetTargets().Value
                |> Seq.exists (isWaypoint)
                |> not)
            |> Option.defaultWith (fun _ -> failwith "Could not find last waypoint. The waypoints must be connected in a linear fashion, from first to last, without branches or loops.")
        let rec work (current : T.MCU_Waypoint) =
            [
                yield current
                let prev =
                    waypoints
                    |> List.filter (fun wp -> wp.GetTargets().Value |> Seq.exists ((=) (current.GetIndex().Value)))
                match prev with
                | [] -> ()
                | [prev] ->
                    yield! work prev
                | _ -> failwithf "Bad %s with multiple predecessors" (infoWp current)
            ]
        work last
        |> List.rev
    {
        Plane = plane
        StartPos = startPos
        StartType = startType
        Path = path
    }

let mkConfigFromGroup (omitEscortInstructions : bool) (group : T.GroupData) =
    let prefixData = extractPath group
    let plane = prefixData.Plane
    let startPos = prefixData.StartPos
    let startType = prefixData.StartType
    let path = prefixData.Path

    let rec start (path : T.MCU_Waypoint list, flyOut) =
        match path with
        | startWp :: rest ->
            let name = startWp.GetName().Value
            match name with
            | "RDV" -> rdv(path, flyOut)
            | "Attack" -> attack1(path, flyOut, None, [])
            | _ -> start (rest, startWp :: flyOut)
        | [] ->
            failwith "Failed to find primary objective"

    and rdv(path, flyOut) =
        match path with
        | [] ->
            toAttack1([], flyOut, None, [])
        | rdv :: rest ->
            toAttack1(rest, flyOut, Some rdv, [])

    and toAttack1(path, flyOut, rdv, toAttack) =
        match path with
        | [] ->
            attack1(path, flyOut, rdv, toAttack)
        | wp :: rest ->
            match wp.GetName().Value with
            | "Attack" -> attack1(path, flyOut, rdv, toAttack)
            | _ -> toAttack1(rest, flyOut, rdv, wp :: toAttack)

    and attack1(path, flyOut, rdv, toAttack1) =
        match path with
        | ingress :: attack :: egress :: rest ->
            toAttack2OrFinal(rest, flyOut, rdv, toAttack1, (ingress, attack, egress), [])
        | _ -> failwith "No primary obj"

    and toAttack2OrFinal(path, flyOut, rdv, toAttack1, attack1, acc) =
        match path with
        | wp :: rest ->
            match wp.GetName().Value with
            | "Attack" -> attack2(path, flyOut, rdv, toAttack1, attack1, acc)
            | _ -> toAttack2OrFinal(rest, flyOut, rdv, toAttack1, attack1, wp :: acc)
        | [] ->
            final(flyOut, rdv, toAttack1, attack1, [], None, acc)

    and attack2(path, flyOut, rdv, toAttack1, attack1, toAttack2) =
        match path with
        | ingress :: attack :: egress :: rest ->
            toFinal(rest, flyOut, rdv, toAttack1, attack1, toAttack2, (ingress, attack, egress), [])
        | _ -> failwith "Bad secondary obj"

    and toFinal(path, flyOut, rdv, toAttack1, attack1, toAttack2, attack2, acc) =
        match path with
        | [] ->
            final(flyOut, rdv, toAttack1, attack1, toAttack2, Some attack2, acc)
        | wp :: rest ->
            toFinal(rest, flyOut, rdv, toAttack1, attack1, toAttack2, attack2, wp :: acc)

    and final(flyOut, rdv, toAttack1, attack1, toAttack2, attack2, toFinal) =
        let flyOut, toAttack1 =
            match rdv with
            | Some _ ->
                flyOut |> List.rev |> FlightSectionConfig.Create |> Some,
                toAttack1 |> List.rev |> FlightSectionConfig.Create
            | None ->
                None,
                (toAttack1 @ flyOut) |> List.rev |> FlightSectionConfig.Create
        let rdv =
            rdv
            |> Option.map (fun rdv ->
                let duration =
                    rdv.GetDesc().Value
                    |> System.Int32.TryParse
                    |> function true, x -> Some x | _ -> None
                    |> Option.defaultValue 10
                    |> ((*) 60)
                    |> float32
                {
                    Location = DirectedPoint.FromMCU rdv
                    Altitude = rdv.GetYPos().Value |> int
                    MaxWaitDuration = duration
                }
            )
        let mkAttack(ingress : T.MCU_Waypoint, attack : T.MCU_Waypoint, egress : T.MCU_Waypoint) =
            let attackArea =
                group.ListOfMCU_CMD_AttackArea
                |> Seq.find (fun area -> attack.GetTargets().Value |> Seq.exists ((=) (area.GetIndex().Value)))
            {
                Center = Vector2.FromPos(attackArea)
                Ingress = DirectedPoint.FromMCU(ingress)
                AttackStart = DirectedPoint.FromMCU(attack)
                Egress = DirectedPoint.FromMCU(egress)
                AttackAltitude = attackArea.GetYPos().Value |> int
            }
        let toAttack2, toFinal =
            match attack2 with
            | Some _ ->
                toAttack2 |> List.rev |> FlightSectionConfig.Create |> Some,
                toFinal |> List.rev |> FlightSectionConfig.Create
            | None ->
                None,
                (toFinal @ toAttack2) |> List.rev |> FlightSectionConfig.Create
        let numPlanes =
            prefixData.Plane.GetDesc().Value
            |> System.Int32.TryParse
            |> function true, x -> Some x | _ -> None
            |> Option.defaultValue 1
        let landing =
            try
                group.ListOfMCU_CMD_Land
                |> Seq.exactlyOne
            with _ -> failwith "Must have exactly one landing command"
        {
            StartType = prefixData.StartType
            StartPos = prefixData.StartPos
            ToRendezVous = flyOut
            RendezVous = rdv
            ToPrimaryObjective = toAttack1
            PrimaryObjective = mkAttack attack1
            ToSecondaryObjective = toAttack2
            SecondaryObjective = attack2 |> Option.map mkAttack
            ToReturn = toFinal
            LandAt = DirectedPoint.FromMCU landing
            NumPlanes = numPlanes
        }

    let config = start(prefixData.Path, [])

    let instructions =
        [
            yield "IN: Connect external trigger to START"
            if config.RendezVous.IsSome && not omitEscortInstructions then
                yield "IN: Connect external trigger to ESCORT_READY"
                yield "OUT: Connect RDV to external trigger (to tell attackers at rendez-vous point)"
                yield "OUT: Connect ESCORT_STDBY to external trigger (e.g. to tell escort to stop escorting during dives)"
                yield "OUT: Connect RELEASE_ESCORT to external trigger (e.g. to release escort)"
            yield "IN: Connect external trigger to ATTACK_DONE of primary objective (e.g. xx objects destroyed)"
            if config.SecondaryObjective.IsSome then
                yield "IN: Connect external trigger to ATTACK_DONE of secondary objective"
        ]

    config, plane, instructions