// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "scripts/load-references.fsx"

open System.Collections.Generic
open SturmovikMissionTypes
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Ast
open SturmovikMission.DataProvider.Mcu

type T = Provider< @"C:\Users\johann\Documents\Visual Studio 2013\Projects\sturmovikmission\data\Sample.Mission", @"C:\Users\johann\Documents\Visual Studio 2013\Projects\sturmovikmission\data\vluki-rescue\vluki-rescue.Mission" >

type vluki = T.``vluki-rescue``
let vlukidyn = T.GroupData(Stream.FromFile @"C:\Users\johann\Documents\Visual Studio 2013\Projects\sturmovikmission\data\vluki-rescue\vluki-rescue.Mission")

type TankGroupContent = {
    Tank1 : T.Vehicle
    Tank2 : T.Vehicle
    Tank3 : T.Vehicle
    Waypoints : T.MCU_Waypoint[]
    OffRoadColumn : T.MCU_CMD_Formation
    OnRoadColumn : T.MCU_CMD_Formation
}
with
    static member Default =
        { Tank1 = vluki.``Panzer gr1``
          Tank2 = vluki.``Panzer gr1_2``
          Tank3 = vluki.``Panzer gr1_3``
          Waypoints = vlukidyn.ListOfMCU_Waypoint |> Array.ofList
        }