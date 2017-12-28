// Parse the mission log and produce a new world state.

#I @"bin\Debug"

#r "Campaign.dll"
#r "ploggy"

#load "Configuration.fsx" 

open Configuration

let entries = Campaign.Run.MissionLogParsing.stage0(config)

let missionResults = Campaign.Run.MissionLogParsing.stage1(config, entries)

Campaign.Run.MissionLogParsing.backupFiles config
let date = Campaign.Run.WeatherComputation.getNextDateFromState config
let weather = Campaign.Run.WeatherComputation.run(config, date)

let newProduction, battleResults, ((oldState, newState) as states) = Campaign.Run.MissionLogParsing.updateState(config, missionResults)
let axisAAR, alliesAAR = Campaign.Run.MissionLogParsing.buildAfterActionReports(config, oldState, newState, missionResults.TakeOffs, missionResults.Landings, missionResults.StaticDamages @ missionResults.VehicleDamages, newProduction)

Campaign.Run.MissionLogParsing.stage2 config (oldState, newState, axisAAR, alliesAAR, battleResults)