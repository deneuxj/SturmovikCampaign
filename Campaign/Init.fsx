#r "../DataProvider/bin/Debug/DataProvider.dll"
#r "../Blocks/bin/Debug/SturmovikMission.Blocks.dll"
#r "../Campaign/bin/Debug/Campaign.dll"
#r "System.Numerics.Vectors"
#r "../Blocks/packages/FsPickler.3.2.0/lib/net45/FsPickler.dll"

#load "Configuration.fsx" 

open SturmovikMission.Blocks.BlocksMissionData
open Campaign.WorldDescription
open Campaign.WorldState
open System.IO

try
    File.Delete("StrategySmall1.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\StrategySmall1.mission", "StrategySmall1.mission")
    File.Delete("Blocks.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Blocks.mission", "Blocks.mission")
    File.Delete("Vehicles.mission")
    File.Copy(@"C:\Users\johann\Documents\SturmovikMission-git\data\Blocks\Vehicles.mission", "Vehicles.mission")
with
    | exc -> printfn "Error copying files: '%s'" exc.Message

let random =
    match Configuration.Seed with
    | Some n ->
        System.Random(n)
    | None ->
        System.Random()

let world0, blocks, bridges, options = World.Create(Configuration.StrategyFile)
let world = { world0 with WeatherDaysOffset = (float Configuration.WeatherDayMaxOffset) * (random.NextDouble() - 0.5) }
let state = WorldState.Create(world, Configuration.StrategyFile)

open MBrace.FsPickler
let serializer = FsPickler.CreateXmlSerializer(indent = true)
do
    let outputDir = Configuration.OutputDir
    use worldFile = File.CreateText(Path.Combine(outputDir, "world.xml"))
    serializer.Serialize(worldFile, world)
    use stateFile = File.CreateText(Path.Combine(outputDir, "state.xml"))
    serializer.Serialize(stateFile, state)
    use blocksFile = File.CreateText(Path.Combine(outputDir, "blocks.xml"))
    serializer.Serialize(blocksFile, blocks)
    use bridgesFile = File.CreateText(Path.Combine(outputDir, "bridges.xml"))
    serializer.Serialize(bridgesFile, bridges)
    use optionsFile = File.CreateText(Path.Combine(outputDir, "options.xml"))
    serializer.Serialize(optionsFile, options)
