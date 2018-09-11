#I @"..\bin\Debug"

#r "Campaign.dll"
#r "System.Numerics.Vectors.dll"
#r "DataProvider.dll"
#r "ploggy"
#r "FsPickler"

#load "Configuration.fsx" 

open System
open System.IO
open MBrace.FsPickler
open Campaign.NewWorldState
open Campaign.ResultExtraction
open Campaign.PlaneModel
open Campaign.BasicTypes
open Campaign.WorldState
open Campaign.WorldDescription

let serializer = FsPickler.CreateXmlSerializer(indent = true)

let state = serializer.Deserialize<WorldState>(File.OpenText(Path.Combine(Configuration.config.OutputDir, "state.xml")))
let world = serializer.Deserialize<World>(File.OpenText(Path.Combine(Configuration.config.OutputDir, "world.xml")))
let wg = world.FastAccess

for regState, region in List.zip state.Regions world.Regions do
    let percent = 100.0f * regState.Supplies / region.GetStorageCapacity(world.SubBlockSpecs)
    printfn "%20s ... %5.0f of %0.0f is %3.0f%%" (string(regState.RegionId)) regState.Supplies (region.GetStorageCapacity(world.SubBlockSpecs)) percent
