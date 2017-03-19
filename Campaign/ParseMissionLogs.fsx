// Parse the mission log and produce a new world state.

#I @"bin\Debug"

#r "Campaign.dll"

#load "Configuration.fsx" 

open Configuration

Campaign.Run.MissionLogParsing.run config