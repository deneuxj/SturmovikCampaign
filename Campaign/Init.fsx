// Create the world description data and the initial state

#I "bin/Debug"
#r "Campaign.dll"

#load "Configuration.fsx" 

open Configuration

Campaign.Run.Init.createWorld config
Campaign.Run.Init.createState config
