module SturmovikMission.Blocks.Strategy

open SturmovikMission.DataProvider
open Types
open Data

let getSomePath() =
    let db = T.GroupData(Parsing.Stream.FromFile "Strategy.Mission").CreateMcuList()
    let group = McuUtil.filterByPath ["Roads"] db  |> List.ofSeq
    let starts =
        McuUtil.filterByName "Start" group
        |> Seq.choose (function :? Mcu.McuWaypoint as waypoint -> Some waypoint | _ -> None)
    if Seq.isEmpty starts then
        []
    else
        let start = Seq.head starts
        let rec work (current : Mcu.McuWaypoint) path =
            if current.Name = "End" then
                current :: path
            else
                let targets =
                    current.Targets
                    |> List.choose(fun idx ->
                        try
                            match McuUtil.getCommandByIndex idx group with
                            | :? Mcu.McuWaypoint as next -> Some next
                            | _ -> None
                        with
                        | _ -> None)
                match targets with
                | [] -> current :: path
                | x :: _ -> work x (current :: path)
        let path =
            work start []
            |> List.rev
            |> List.map (fun wp ->
                { Pos = wp.Pos
                  Ori = wp.Ori
                  Speed = wp.Speed
                  Priority = wp.Priority
                  Radius = wp.Radius
                }
            )
        path