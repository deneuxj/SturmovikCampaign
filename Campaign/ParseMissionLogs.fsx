// Parse the mission log and produce a new world state.

#I @"bin\Debug"

#r "Campaign.dll"
#r "ploggy"

#load "Configuration.fsx" 

open Configuration

let data, states = Campaign.Run.MissionLogParsing.stage1 config
let entries, shipped, resups, damages, tookOff, landed, columnLeft, columnArrived = data

Campaign.Run.MissionLogParsing.stage2 config states