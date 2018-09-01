// Generate a mission given a world state
#I "../bin/Debug"

#r "Campaign.dll"

#load "Configuration.fsx"

open Configuration

Async.RunSynchronously(Campaign.Run.MissionFileGeneration.run Configuration.config)