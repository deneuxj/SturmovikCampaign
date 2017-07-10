// Parse the mission log and produce a new world state.

#I @"bin\Debug"

#r "Campaign.dll"
#r "ploggy"

#load "Configuration.fsx" 

open Configuration

let missionResults = Campaign.Run.MissionLogParsing.stage1 config
let newProduction, battleResults, ((oldState, newState) as states) = Campaign.Run.MissionLogParsing.updateState(config, missionResults)
let axisAAR, alliesAAR = Campaign.Run.MissionLogParsing.buildAfterActionReports(config, oldState, newState, missionResults.TakeOffs, missionResults.Landings, missionResults.StaticDamages @ missionResults.VehicleDamages, newProduction)