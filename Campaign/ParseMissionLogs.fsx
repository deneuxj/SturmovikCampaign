// Parse the mission log and produce a new world state.

#I @"bin\Debug"

#r "Campaign.dll"
#r "ploggy"

#load "Configuration.fsx" 

open Configuration

let data, states = Campaign.Run.MissionLogParsing.stage1 config
let entries, shipped, staticDamages, vehicleDamages, tookOff, landed, columnLeft = data
let oldState, newState = states
let axisAAR, alliesAAR = Campaign.Run.MissionLogParsing.buildAfterActionReports(config, oldState, newState, tookOff, landed, staticDamages @ vehicleDamages)
Campaign.Run.MissionLogParsing.stage2 config (oldState, newState, axisAAR, alliesAAR)
