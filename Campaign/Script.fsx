// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Scripts/load-project-debug.fsx"
open Campaign
open SturmovikMission.DataProvider.Parsing

let s = Stream.FromFile @"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission"
let data = T.GroupData(s)
let areas = Area.ExtractAreas(data.GetGroup("Zones").ListOfMCU_TR_InfluenceArea)
let roads = Path.ExtractPaths(data.GetGroup("Roads").ListOfMCU_Waypoint, areas)
let rail = Path.ExtractPaths(data.GetGroup("Trains").ListOfMCU_Waypoint, areas)
let defenses = data.GetGroup("Defenses")
let aaas = defenses.ListOfMCU_TR_InfluenceArea |> List.filter(fun spawn -> spawn.Name.Value = "AAA")
let spawnsAAA = SpawnArea.ExtractCentralSpawnAreas(aaas, areas)
let ats = defenses.ListOfMCU_TR_InfluenceArea |> List.filter(fun spawn -> spawn.Name.Value = "AT")
let spawnsAT = SpawnArea.ExtractFrontLineSpawnAreas(ats, areas, roads)
let afs = data.GetGroup("Airfield spawns").ListOfAirfield
let planes = data.GetGroup("Parked planes").ListOfPlane
let storages = data.GetGroup("Airfield storage").ListOfBlock
let airfields = Airfield.ExtractAirfields(afs, planes, storages)

// Define your library scripting code here

