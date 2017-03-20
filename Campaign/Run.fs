module Campaign.Run

type Configuration = {
    StrategyFile : string
    Seed : int option
    WeatherDayMaxOffset : int
    MaxConvoys : int
    MaxInvasionsInPlanning : int
    MaxInvasions : int
    MaxReinforcements : int
    MissionName : string
    MissionLength : int
    ConvoyInterval : int
    OutputDir : string
    ServerDataDir : string
    ServerBinDir : string
    ServerSdsFile : string
    ScriptPath : string
    Briefing : string
}

module Init =
    open SturmovikMission.Blocks.BlocksMissionData
    open Campaign.WorldDescription
    open Campaign.WorldState
    open System.IO
    open MBrace.FsPickler

    let run(config : Configuration) =
        let random =
            match config.Seed with
            | Some n ->
                System.Random(n)
            | None ->
                System.Random()

        let world0, (blocks : T.Block list), (bridges : T.Bridge list), (options : T.Options) = World.Create(Path.Combine(config.ScriptPath, config.StrategyFile))
        let world = { world0 with WeatherDaysOffset = (float config.WeatherDayMaxOffset) * (random.NextDouble() - 0.5) }

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

        let state = WorldState.Create(world, Path.Combine(config.ScriptPath, config.StrategyFile))

        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let outputDir = config.OutputDir
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


module MissionFileGeneration =
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.MissionGeneration
    open Campaign.AutoOrder
    open Campaign.Orders
    open SturmovikMission.Blocks.BlocksMissionData
    open System.IO
    open MBrace.FsPickler
    open System.Diagnostics

    let run(config : Configuration) =
        let random =
            match config.Seed with
            | Some n ->
                System.Random(n)
            | None ->
                System.Random()

        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, blocks, bridges, options, state =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, "world.xml"))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, "state.xml"))
                use blocksFile = File.OpenText(Path.Combine(config.OutputDir, "blocks.xml"))
                use bridgesFile = File.OpenText(Path.Combine(config.OutputDir, "bridges.xml"))
                use optionsFile = File.OpenText(Path.Combine(config.OutputDir, "options.xml"))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<T.Block list>(blocksFile),
                serializer.Deserialize<T.Bridge list>(bridgesFile),
                serializer.Deserialize<T.Options>(optionsFile),
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Did you run Init.fsx? Reason was: '%s'" e.Message

        let dt = (1.0f<H>/60.0f) * float32 config.MissionLength
        let mkOrders coalition =
            let convoyOrders =
                createAllConvoyOrders coalition (world, state)
                |> prioritizeConvoys config.MaxConvoys dt world state
            let invasions =
                createGroundInvasionOrders(coalition, world, state)
                |> prioritizeGroundInvasionOrders(world, state)
                |> List.truncate config.MaxInvasionsInPlanning
            let reinforcements =
                prioritizedReinforcementOrders(world, state) coalition invasions
            let reinforcements, invasions = filterIncompatible(reinforcements, invasions)
            convoyOrders, reinforcements |> List.truncate config.MaxReinforcements, invasions |> List.truncate config.MaxInvasions |> List.map (fun (x, _, _) -> x)
        let adjustIndexes(convoys : ResupplyOrder list, reinforcements : ColumnMovement list, invasions : ColumnMovement list) =
            let convoys =
                convoys
                |> List.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = i + 1 } })
            let n = List.length convoys
            let reinforcements =
                reinforcements
                |> List.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = i + 1 + n } })
            let n = n + List.length reinforcements
            let invasions =
                invasions
                |> List.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = i + 1 + n } })
            convoys, reinforcements, invasions
        let mkAllOrders coalition =
            let convoys, reinforcements, invasions =
                mkOrders coalition
                |> adjustIndexes
            { Resupply = convoys
              Reinforcements = reinforcements
              Invasions = invasions
            }
        let allAxisOrders = mkAllOrders Axis
        let allAlliesOrders = mkAllOrders Allies

        let author = "coconut"
        let briefing = config.Briefing.Replace("\r\n", "\n").Replace("\n", "<br>")

        do
            let outputDir = config.OutputDir
            use axisOrderFiles = File.CreateText(Path.Combine(outputDir, "axisOrders.xml"))
            serializer.Serialize(axisOrderFiles, allAxisOrders)
            use alliesOrderFiles = File.CreateText(Path.Combine(outputDir, "alliesOrders.xml"))
            serializer.Serialize(alliesOrderFiles, allAlliesOrders)

        let missionName = config.MissionName
        writeMissionFile random author config.MissionName briefing config.MissionLength config.ConvoyInterval options blocks bridges world state allAxisOrders allAlliesOrders (Path.Combine(config.OutputDir, missionName + ".Mission"))

        let mpDir = Path.Combine(config.ServerDataDir, "Multiplayer")
        let swallow f = try f() with | _ -> ()
        swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".Mission")))
        swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".eng")))
        swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".msnbin")))
        File.Copy(Path.Combine(config.OutputDir, missionName + ".Mission"), Path.Combine(mpDir, missionName + ".Mission"))
        File.Copy(Path.Combine(config.OutputDir, missionName + ".eng"), Path.Combine(mpDir, missionName + ".eng"))
        let p = ProcessStartInfo("MissionResaver.exe", sprintf "-d %s -f %s" config.ServerDataDir (Path.Combine(mpDir, missionName + ".Mission")))
        p.WorkingDirectory <- Path.Combine(config.ServerBinDir, "resaver")
        p.UseShellExecute <- true
        let proc = Process.Start(p)
        proc.WaitForExit()
        swallow (fun () -> File.Delete (Path.Combine(mpDir, missionName + ".Mission")))
        printfn "Resaver exited with code %d" proc.ExitCode

module MissionLogParsing =
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.ResultExtraction
    open Campaign.NewWorldState
    open Campaign.Orders
    open MBrace.FsPickler
    open System.IO
    open ploggy
    open NLog

    let run(config : Configuration) =
        let missionLogsDir = Path.Combine(config.ServerDataDir, "logs")

        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state, axisOrders, alliesOrders =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, "world.xml"))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, "state.xml"))
                use axisOrdersFile = File.OpenText(Path.Combine(config.OutputDir, "axisOrders.xml"))
                use alliesOrdersFile = File.OpenText(Path.Combine(config.OutputDir, "alliesOrders.xml"))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile),
                serializer.Deserialize<OrderPackage>(axisOrdersFile),
                serializer.Deserialize<OrderPackage>(alliesOrdersFile)
            with
            | e -> failwithf "Failed to read world and state data. Did you run Init.fsx? Reason was: '%s'" e.Message

        do
            let config = Config.LoggingConfiguration()
            LogManager.Configuration <- config
            Plogger.Init()

        let entries =
            seq {
                for file in Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt") do
                    for line in File.ReadAllLines(file) do
                        yield LogEntry.Parse(line)
            }
            |> Seq.filter (
                function
                | :? MissionStartEntry
                | :? ObjectSpawnedEntry
                | :? TakeOffEntry
                | :? MissionObjectiveEntry
                | :? DamageEntry
                | :? KillEntry
                | :? LandingEntry
                | :? RoundEndEntry
                | :? MissionEndEntry -> true
                | _ -> false
            )
            |> Seq.sortBy (fun x -> x.Timestamp)
            |> Seq.skipWhile (
                function
                | :? MissionStartEntry as entry ->
                    entry.MissionTime <> state.Date
                | _ -> true
            )
            |> Seq.takeWhile (
                function
                | :? MissionStartEntry as entry ->
                    entry.MissionTime = state.Date
                | _ -> true
            )
            |> Seq.cache

        let shipments =
            let shipmentAxis = extractSuppliesShipped state axisOrders.Resupply entries |> List.ofSeq
            let shipmentAllies = extractSuppliesShipped state alliesOrders.Resupply entries |> List.ofSeq
            shipmentAxis @ shipmentAllies
        let resups =
            let resupsAxis = extractResupplies world state axisOrders.Resupply entries |> List.ofSeq
            let resupsAllies = extractResupplies world state alliesOrders.Resupply entries |> List.ofSeq
            resupsAxis @ resupsAllies
        let staticDamages = extractStaticDamages world state entries |> List.ofSeq
        let takeOffs, landings =
            let both =
                extractTakeOffs world entries
                |> List.ofSeq
            both |> List.choose (function Choice1Of2 x -> Some x | _ -> None),
            both |> List.choose (function Choice2Of2 x -> Some x | _ -> None)
        let movements = axisOrders.Reinforcements @ axisOrders.Invasions @ alliesOrders.Reinforcements @ alliesOrders.Invasions
        let columnDepartures = extractColumnDepartures movements entries |> List.ofSeq
        let columnArrivals = extractColumnArrivals world state movements entries |> List.ofSeq
        let dt = (1.0f<H>/60.0f) * float32 config.MissionLength

        let state2 = newState dt world state movements shipments resups staticDamages takeOffs landings columnDepartures columnArrivals

        let outputDir = config.OutputDir

        let backupFile name =
            let backupName =
                sprintf "%s-%s-%s.xml" name (state.Date.ToShortDateString()) (state.Date.ToShortTimeString())
                |> fun x -> x.Replace(":", "-")
            let backupDest = Path.Combine(outputDir, backupName)
            if File.Exists backupDest then
                File.Delete(backupDest)
            File.Copy(Path.Combine(outputDir, sprintf "%s.xml" name), backupDest)
    
        do
            backupFile "state"
            backupFile "axisOrders"
            backupFile "alliesOrders"
            use stateFile = File.CreateText(Path.Combine(outputDir, "state.xml"))
            serializer.Serialize(stateFile, state2)
