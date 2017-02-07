// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "Scripts/load-project-debug.fsx"
open Campaign.WorldDescription
open SturmovikMission.DataProvider.Parsing

let s = Stream.FromFile @"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission"
let data = T.GroupData(s)
let regions =
    let regions = Region.ExtractRegions(data.GetGroup("Regions").ListOfMCU_TR_InfluenceArea)
    let ammoStorages = data.GetGroup("Ammo").ListOfBlock
    regions
    |> List.map (fun area -> area.AddStorage ammoStorages)
let roads = Path.ExtractPaths(data.GetGroup("Roads").ListOfMCU_Waypoint, regions)
let rail = Path.ExtractPaths(data.GetGroup("Trains").ListOfMCU_Waypoint, regions)
let defenses = data.GetGroup("Defenses")
let aaas = defenses.ListOfMCU_TR_InfluenceArea |> List.filter(fun spawn -> spawn.Name.Value = "AAA")
let spawnsAAA = DefenseArea.ExtractCentralDefenseAreas(aaas, regions)
let ats = defenses.ListOfMCU_TR_InfluenceArea |> List.filter(fun spawn -> spawn.Name.Value = "AT")
let spawnsAT = DefenseArea.ExtractFrontLineDefenseAreas(ats, regions, roads)
let afs = data.GetGroup("Airfield spawns").ListOfAirfield
let planes = data.GetGroup("Parked planes").ListOfPlane
let afStorages = data.GetGroup("Airfield storage").ListOfBlock
let airfields = Airfield.ExtractAirfields(afs, planes, afStorages)
