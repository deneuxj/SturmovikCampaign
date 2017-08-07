// Create the world description data and the initial state

#I "bin/Debug"
#r "Campaign.dll"

#load "Configuration.fsx" 

open Configuration

let startDate = Campaign.Run.Init.createWorld config
Campaign.Run.WeatherComputation.run(config, startDate)
Campaign.Run.Init.createState config
Campaign.Run.OrderDecision.run config
