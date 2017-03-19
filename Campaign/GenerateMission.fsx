// Generate a mission given a world state
#I "bin/Debug"

#r "Campaign.dll"

#load "Configuration.fsx"

open Configuration

Campaign.Run.MissionFileGeneration.run config