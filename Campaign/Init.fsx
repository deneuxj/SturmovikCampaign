// Create the world description data and the initial state

#I "../Campaign/bin/Debug"

#r "DataProvider.dll"
#r "SturmovikMission.Blocks.dll"
#r "Campaign.dll"
#r "FsPickler.dll"
#r "System.Numerics.Vectors"

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

let world0, (blocks : T.Block list), (bridges : T.Bridge list), (options : T.Options) = World.Create(Configuration.StrategyFile)
let world = { world0 with WeatherDaysOffset = (float Configuration.WeatherDayMaxOffset) * (random.NextDouble() - 0.5) }

let capacity =
    world.Regions
    |> Seq.map (fun region -> region.RegionId, region.Storage |> Seq.sumBy (fun sto -> getSupplyCapacityPerBuilding sto.Model))
    |> Seq.map (fun (region, capacity) -> region, capacity / canonCost)
    |> Map.ofSeq

let production =
    world.Regions
    |> Seq.map (fun region -> region.RegionId, region.Production |> Seq.sumBy (fun sto -> getProductionPerBuilding sto.Model))
    |> Seq.map (fun (region, production) -> region, production)
    |> Map.ofSeq

let antiAirUsage =
    world.AntiAirDefenses
    |> Seq.map (fun def -> def.Home.Home, getAntiAirCanonsForArea def)
    |> Seq.groupBy fst
    |> Seq.map (fun (region, canons) -> region, canons |> Seq.sumBy snd)
    |> Map.ofSeq

let antiTankUsage =
    world.AntiTankDefenses
    |> Seq.map (fun def -> def.Home.Home, getAntiTankCanonsForArea def)
    |> Seq.groupBy fst
    |> Seq.map (fun (region, canons) -> region, canons |> Seq.sumBy snd)
    |> Map.ofSeq

for region in world.Regions do
    let (RegionId regionName) = region.RegionId
    let aa = Map.tryFind region.RegionId antiAirUsage |> fun x -> defaultArg x 0
    let at = Map.tryFind region.RegionId antiTankUsage |> fun x -> defaultArg x 0
    let cap = Map.tryFind region.RegionId capacity |> fun x -> defaultArg x 0.0f
    let prod = Map.tryFind region.RegionId production |> fun x -> defaultArg x 0.0f<E/H>
    printfn "%20s | %6.1f - %3d | %4d - %5.1f | %4d - %5.1f" regionName prod (int cap) aa (100.0f * float32 aa / cap) at (100.0f * float32 at / cap)

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
