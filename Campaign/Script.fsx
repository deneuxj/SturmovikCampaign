// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r "../DataProvider/bin/Debug/DataProvider.dll"
#r "../Blocks/bin/Debug/SturmovikMission.Blocks.exe"
#r "../Campaign/bin/Debug/Campaign.dll"

open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.MissionGeneration
open Campaign.AutoOrder
open System.IO

try
    File.Delete("StrategySmall1.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission", "StrategySmall1.mission")
    File.Delete("Blocks.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Blocks.mission", "Blocks.mission")
    File.Delete("Vehicles.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Vehicles.mission", "Vehicles.mission")
with
    | exc -> printfn "Error copying files: '%s'" exc.Message

let strategyFile = "StrategySmall1.mission"
let world, blocks, options = World.Create(strategyFile)
let state = WorldState.Create(world, strategyFile)
let axisConvoyOrders =
    createAllConvoyOrders(Some Axis, world, state)
    |> prioritizeConvoys 4 world state
let alliesConvoyOrders =
    createAllConvoyOrders (Some Allies, world, state)
    |> prioritizeConvoys 4 world state
let allConvoyOrders = axisConvoyOrders @ alliesConvoyOrders
writeMissionFile options blocks world state allConvoyOrders @"C:\Users\johann\Documents\AutoMoscow\AutoGenMission.Mission"