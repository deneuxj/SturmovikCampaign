// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "../DataProvider/bin/Debug/DataProvider.dll"
#r "../Blocks/bin/Debug/SturmovikMission.Blocks.exe"
#r "../Campaign/bin/Debug/Campaign.dll"

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.MissionGeneration
open System.IO

File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission", "StrategySmall1.mission")
File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Blocks.mission", "Blocks.mission")
File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Vehicles.mission", "Vehicles.mission")

let strategyFile = @"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission"
let world = World.Create(strategyFile)
let state = WorldState.Create(world, strategyFile)
writeGroupFile world state @"C:\Users\johann\Documents\campaign.group"