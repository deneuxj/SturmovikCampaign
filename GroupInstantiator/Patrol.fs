﻿module Patrol

open SturmovikMission.Blocks.Patrol
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.McuInstantiation

open System.Numerics
open VectorExtension

open GroundAttack

let mkConfigFromGroup (group : T.GroupData) =
    let prefixData = extractPath group

    let (|Start|_|) =
        function
        | (startWp : T.MCU_Waypoint) :: rest ->
            Some(
                {|
                    CruiseAltitude = startWp.GetYOri().Value |> int
                    CruiseSpeed = startWp.GetSpeed().Value
                |}, rest)
        | [] -> None

    let (|Waypoint|_|) =
        function
        | (wp : T.MCU_Waypoint) :: rest ->
            Some(
                {|
                    Pos = DirectedPoint.FromMCU wp
                    Alt = int(wp.GetYPos().Value)
                |}, rest)
        | [] -> None

    let (|PatrolArea|_|) =
        function
        | (wp : T.MCU_Waypoint) :: rest ->
            let duration =
                wp.GetDesc().Value
                |> System.Int32.TryParse
                |> function true, x -> x | false, _ -> 20
            Some (
                {|
                    Center = DirectedPoint.FromMCU wp
                    Alt = int(wp.GetYPos().Value)
                    Speed = wp.GetSpeed().Value
                    MaxRange = wp.GetArea().Value
                    Duration = float32 (duration * 60)
                |}, rest)
        | [] -> None

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
        | [] | [_] | [_;_] ->
            failwith "Missing 'intoReturn', 'return' or 'land' waypoint"
        | _ ->
            failwith "Extra unprocessed waypoints"

    let numPlanes =
        prefixData.Plane.GetDesc().Value
        |> System.Int32.TryParse
        |> function true, x -> Some x | _ -> None
        |> Option.defaultValue 1

    let config =
        match prefixData.Path with
        | Start(start, Waypoint(wp2, PatrolArea(patrol, Waypoint(rtb, Closing closing)))) ->
            {
                StartType = prefixData.StartType
                StartPos = prefixData.StartPos
                CruiseAltitude = start.CruiseAltitude
                CruiseSpeed = start.CruiseSpeed
                MidPoint = wp2.Pos
                PatrolCenter = patrol.Center
                PatrolDuration = patrol.Duration
                PatrolAltitude = patrol.Alt
                PatrolSpeed = patrol.Speed
                MaxRange = patrol.MaxRange
                Return = closing.Return
                Final = closing.Final
                LandAt = closing.LandAt
                NumPlanes = numPlanes
            } : PatrolGroupConfig
        | _ -> failwith "Failed to analyse flight path"

    config, prefixData.Plane