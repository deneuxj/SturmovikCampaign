// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Scripts/load-project-debug.fsx"
open Campaign
open SturmovikMission.DataProvider.Parsing

let s = Stream.FromFile @"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission"
let data = T.GroupData(s)
let areas = Area.ExtractAreas(data.ListOfMCU_TR_InfluenceArea)
let paths = Path.ExtractPaths(data.ListOfMCU_Waypoint, areas)

// Define your library scripting code here

