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
                | [] -> failwith "Missing take off MCU"
                | _ -> failwith "Too many take off MCUs"
            GroundStart {| EnginesRunning = running; TakeOffPoint = DirectedPoint.FromMCU(takeoff) |}
        | x -> failwithf "Invalid air/ground start value %d" x
    let waypoints =
        group.ListOfMCU_Waypoint
        |> List.ofSeq
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
            |> Option.defaultWith (fun _ -> failwith "Could not find last waypoint")
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

let mkConfigFromGroup (group : T.GroupData) =
    let prefixData = extractPath group
    let plane = prefixData.Plane
    let startPos = prefixData.StartPos
    let startType = prefixData.StartType
    let path = prefixData.Path

    let (|Start|_|) =
        function
        | (startWp : T.MCU_Waypoint) :: rest ->
            Some({| CruiseAltitude = startWp.GetYOri().Value |> int; CruiseSpeed = startWp.GetSpeed().Value |}, rest)
        | [] -> None

    let (|OptRendezVous|) =
        function
        | ((rdv : T.MCU_Waypoint) :: rest) as path ->
            if rdv.GetName().Value.ToLowerInvariant().Contains("rdv") then
                let duration =
                    rdv.GetDesc().Value
                    |> System.Int32.TryParse
                    |> function true, x -> Some x | _ -> None
                    |> Option.defaultValue 10
                    |> ((*) 60)
                    |> float32
                OptRendezVous(
                    Some {Location = DirectedPoint.FromMCU rdv; Altitude = rdv.GetYPos().Value |> int; MaxWaitDuration = duration},
                    rest)
            else
                OptRendezVous(None, path)
        | [] -> OptRendezVous(None, [])

    let (|MidPos|_|) =
        function
        | (wp : T.MCU_Waypoint) :: rest -> Some(DirectedPoint.FromMCU wp, rest)
        | [] -> None

    let (|Attack|_|) =
        function
        | (ingress : T.MCU_Waypoint) :: attack :: egress :: rest ->
            let attackArea =
                group.ListOfMCU_CMD_AttackArea
                |> Seq.tryFind (fun area -> attack.GetTargets().Value |> Seq.exists ((=) (area.GetIndex().Value)))
            attackArea
            |> Option.map (fun attackArea ->
                {
                    Center = Vector2.FromPos(attackArea)
                    Ingress = DirectedPoint.FromMCU(ingress)
                    AttackStart = DirectedPoint.FromMCU(attack)
                    Egress = DirectedPoint.FromMCU(egress)
                    AttackAltitude = attackArea.GetYPos().Value |> int
                },
                rest
            )
        | _ -> None

    let (|Closing|) =
        function
        | [wpReturn : T.MCU_Waypoint; wpLand] ->
            let landing =
                let landing =
                    group.ListOfMCU_CMD_Land
                    |> List.ofSeq
                match landing with
                | [] -> failwith "Missing landing command"
                | [x] -> x
                | _ -> failwith "Too many landing commands"
            Closing {|
                    Return = DirectedPoint.FromMCU(wpReturn)
                    Final = DirectedPoint.FromMCU(wpLand)
                    LandAt = DirectedPoint.FromMCU(landing)
            |}
        | [] | [_] ->
            failwith "Missing 'RTB' or 'Final' waypoint"
        | _ ->
            failwith "Extra unprocessed waypoints"

    let numPlanes =
        plane.GetDesc().Value
        |> System.Int32.TryParse
        |> function true, x -> Some x | _ -> None
        |> Option.defaultValue 1

    let config =
        match path with
        | Start(startData, OptRendezVous(rdvOpt, MidPos(mid, Attack(primary, MidPos(intoReturn, rest))))) ->
            let secondary, rest =
                match rest with
                | Attack(secondary, rest) -> Some secondary, rest
                | rest -> None, rest
            match rest with
            | Closing closing ->
                {
                    StartType = startType
                    StartPos = startPos
                    CruiseAltitude = startData.CruiseAltitude
                    CruiseSpeed = startData.CruiseSpeed
                    RendezVous = rdvOpt
                    MidPos = mid
                    PrimaryObjective = primary
                    SecondaryObjective = secondary
                    IntoReturn = intoReturn
                    Return = closing.Return
                    Final = closing.Final
                    LandAt = closing.LandAt
                    NumPlanes = numPlanes
                }
        | Start(_, OptRendezVous(_, MidPos(_, []))) ->
            failwith "Missing primary objective"
        | Start(_, OptRendezVous(_, MidPos(_, wp :: _))) ->
            failwithf "Invalid primary objective starting with %s" (infoWp wp)
        | Start(_, OptRendezVous(_, [])) ->
            failwith "Missing waypoint to primary objective"
        | _ -> failwith "Failed to analyse flight path"

    config, plane