module Campaign.Run

open System.IO

open Campaign.BasicTypes

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
    MaxSimultaneousConvoys : int
    OutputDir : string
    ServerDataDir : string
    ServerBinDir : string
    ServerSdsFile : string
    ScriptPath : string
    Briefing : string
    ThinkTime : int
}

let backupFile (date : System.DateTime) outputDir name =
    let inFile = Path.Combine(outputDir, sprintf "%s.xml" name)
    if File.Exists inFile then
        let backupName =
            let dateString =
                date.ToString("yyyy-MM-dd_HH-mm-ss")
            sprintf "%s_%s.xml" name dateString
        let backupDest = Path.Combine(outputDir, backupName)
        if File.Exists backupDest then
            File.Delete(backupDest)
        File.Copy(inFile, backupDest)

module Init =
    open SturmovikMission.Blocks.BlocksMissionData
    open Campaign.WorldDescription
    open Campaign.WorldState
    open System.IO
    open MBrace.FsPickler

    let createWorld(config : Configuration) =
        let random =
            match config.Seed with
            | Some n ->
                System.Random(n)
            | None ->
                System.Random()

        let world0 = World.Create(Path.Combine(config.ScriptPath, config.StrategyFile))
        let world = { world0 with WeatherDaysOffset = (float config.WeatherDayMaxOffset) * (random.NextDouble() - 0.5) }

        let capacity =
            world.Regions
            |> Seq.map (fun region -> region.RegionId, region.Storage |> Seq.sumBy (fun sto -> sto.Storage))
            |> Seq.map (fun (region, capacity) -> region, capacity / canonCost)
            |> Map.ofSeq

        let production =
            world.Regions
            |> Seq.map (fun region -> region.RegionId, region.Production |> Seq.sumBy (fun prod -> prod.Production))
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

        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let outputDir = config.OutputDir
        use worldFile = File.CreateText(Path.Combine(outputDir, "world.xml"))
        serializer.Serialize(worldFile, world)

    let createState(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        use worldFile = File.OpenText(Path.Combine(config.OutputDir, "world.xml"))
        let world = serializer.Deserialize<World>(worldFile)

        let state = WorldState.Create(world, Path.Combine(config.ScriptPath, config.StrategyFile))

        let outputDir = config.OutputDir
        use stateFile = File.CreateText(Path.Combine(outputDir, "state.xml"))
        serializer.Serialize(stateFile, state)

module PlayChess =
    open MBrace.FsPickler
    open System.IO
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.MinMax
    open System.Diagnostics
    open System.Threading

    let run(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, "world.xml"))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, "state.xml"))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Did you run Init.fsx? Reason was: '%s'" e.Message
        seq {
            let board, neighboursOf = BoardState.Create(world, state)
            yield "Initially"
            yield board.DisplayString
            yield sprintf "%f" board.Score.Value
            let minMax cancel n = minMax cancel n neighboursOf
            let rec timeBound cancel prev n board =
                if n >= 100 then
                    prev
                else
                    printfn "Max depth: %d" n
                    let res = minMax cancel n board
                    if cancel.IsCancellationRequested then
                        prev
                    else
                        timeBound cancel res (n + 1) board
            let minMax board =
                use cancellation = new CancellationTokenSource()
                cancellation.CancelAfter(config.ThinkTime * 1000)
                timeBound cancellation.Token ({ Axis = []; Allies = [] }, Ongoing 0.0f) 1 board
            yield! play minMax board
        }

module WeatherComputation =
    open MBrace.FsPickler
    open System.IO
    open Campaign.WorldDescription
    open Campaign.WorldState

    let run(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, "world.xml"))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, "state.xml"))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Did you run Init.fsx? Reason was: '%s'" e.Message
        let random =
            match config.Seed with
            | Some n ->
                System.Random(n)
            | None ->
                System.Random()
        let daysOffset = System.TimeSpan(int64(world.WeatherDaysOffset * 3600.0 * 24.0  * 1.0e7))
        let weather = Weather.getWeather random (state.Date + daysOffset)

        backupFile state.Date config.OutputDir "weather"
        use weatherFile = File.CreateText(Path.Combine(config.OutputDir, "weather.xml"))
        serializer.Serialize(weatherFile, weather)

module OrderDecision =
    open System.IO
    open MBrace.FsPickler
    open Campaign.AutoOrder
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.Orders
    open Campaign.AiPlanes
    open Campaign.Util

    let run(config : Configuration) =
        let serializer = FsPickler.CreateXmlSerializer(indent = true)
        let world, state =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, "world.xml"))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, "state.xml"))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Did you run Init.fsx? Reason was: '%s'" e.Message

        let adjustConvoyIndexes(convoys : ResupplyOrder list) =
            convoys
            
        let mkConvoys coalition =
            createAllConvoyOrders coalition (world, state)
            |> prioritizeConvoys world state
            |> List.truncate config.MaxConvoys
            |> List.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = i + 1 } })
        let columnOrders =
            decideColumnMovements world state config.ThinkTime
            |> List.mapi (fun i order -> { order with OrderId = { order.OrderId with Index = i + 1 } })
        let axisConvoys = mkConvoys Axis
        let alliesConvoys = mkConvoys Allies
        let axisColumns =
            columnOrders
            |> List.filter (fun order -> order.OrderId.Coalition = Axis)
        let alliesColumns =
            columnOrders
            |> List.filter (fun order -> order.OrderId.Coalition = Allies)
        let mkPatrols coalition =
            let random = System.Random()
            mkAllPatrols world state coalition
            |> Array.ofSeq
            |> Array.shuffle random
            |> Array.truncate 4
            |> List.ofArray
        let axisPatrols = mkPatrols Axis
        let alliesPatrols = mkPatrols Allies
        let outputDir = config.OutputDir
        backupFile state.Date outputDir "axisOrders.xml"
        backupFile state.Date outputDir "alliesOrders.xml"
        use axisOrderFiles = File.CreateText(Path.Combine(outputDir, "axisOrders.xml"))
        serializer.Serialize(axisOrderFiles, { Resupply = axisConvoys; Columns = axisColumns; Patrols = axisPatrols } )
        use alliesOrderFiles = File.CreateText(Path.Combine(outputDir, "alliesOrders.xml"))
        serializer.Serialize(alliesOrderFiles, { Resupply = alliesConvoys; Columns = alliesColumns; Patrols = alliesPatrols } )

module MissionFileGeneration =
    open Campaign.WorldDescription
    open Campaign.WorldState
    open Campaign.MissionGeneration
    open Campaign.AutoOrder
    open Campaign.Orders
    open Campaign.Weather
    open Campaign.Util
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
        let world, state =
            try
                use worldFile = File.OpenText(Path.Combine(config.OutputDir, "world.xml"))
                use stateFile = File.OpenText(Path.Combine(config.OutputDir, "state.xml"))
                serializer.Deserialize<World>(worldFile),
                serializer.Deserialize<WorldState>(stateFile)
            with
            | e -> failwithf "Failed to read world and state data. Did you run Init.fsx? Reason was: '%s'" e.Message

        let weather =
            try
                use weatherFile = File.OpenText(Path.Combine(config.OutputDir, "weather.xml"))
                serializer.Deserialize<WeatherState>(weatherFile)
            with
            | e -> failwithf "Failed to read weather data. Reason was: '%s'" e.Message

        let allAxisOrders, allAlliesOrders =
            try
                use axisOrdersFile = File.OpenText(Path.Combine(config.OutputDir, "axisOrders.xml"))
                use alliesOrdersFile = File.OpenText(Path.Combine(config.OutputDir, "alliesOrders.xml"))
                serializer.Deserialize<OrderPackage>(axisOrdersFile),
                serializer.Deserialize<OrderPackage>(alliesOrdersFile)
            with
            | e -> failwithf "Failed to read orders. Reason was: '%s'" e.Message
        let dt = (1.0f<H>/60.0f) * float32 config.MissionLength

        let daysOffset = System.TimeSpan(int64(world.WeatherDaysOffset * 3600.0 * 24.0  * 1.0e7))
        let weather = Weather.getWeather random (state.Date + daysOffset)

        let author = "coconut"
        let timeAndDate =
            sprintf "%s<br><br>" (state.Date.ToString("d MMM yyyy HH:mm"))
        let weatherDescription =
            let cover =
                if weather.CloudDensity < 0.2 then
                    "clear"
                elif weather.CloudDensity < 0.4 then
                    "light"
                elif weather.CloudDensity < 0.6 then
                    "medium"
                elif weather.CloudDensity < 0.8 then
                    "heavy"
                else
                    "overcast"
            let windDirection =
                weather.Wind.Direction
            let windOrigin =
                let descs =
                    [ 1.0, "south"
                      3.0, "south-west"
                      5.0, "west"
                      7.0, "north-west"
                      9.0, "north"
                      11.0, "north-east"
                      15.0, "east"
                      15.0, "south-east" ]
                descs
                |> List.tryFind(fun (k, _) -> windDirection < k * 22.5)
                |> Option.map snd
                |> Option.defaultVal "south"
            sprintf "<b>Weather<b><br>Temperature: %2.0fC, Cloud cover: %s, Wind %3.1f m/s from %s<br><br>" weather.Temperature cover weather.Wind.Speed windOrigin
        let briefing = timeAndDate + weatherDescription + config.Briefing.Replace("\r\n", "\n").Replace("\n", "<br>")

        let missionName = config.MissionName
        writeMissionFile random weather author config.MissionName briefing config.MissionLength config.ConvoyInterval config.MaxSimultaneousConvoys (Path.Combine(config.ScriptPath, config.StrategyFile)) world state allAxisOrders allAlliesOrders (Path.Combine(config.OutputDir, missionName + ".Mission"))

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
        proc.ExitCode

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
    open System.Text.RegularExpressions

    let stage1(config : Configuration) =
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
            let missionHasPassed30Min (entry : LogEntry) =
                entry.Timestamp > System.TimeSpan(0, 30, 0)
            seq {
                let unordered = Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt")
                let r = Regex(@"(missionReport\(.*\))\[([0-9]+)\]\.txt")
                let ordered =
                    unordered
                    |> Seq.choose(fun path ->
                        let m = r.Match(Path.GetFileName(path))
                        if not m.Success then
                            None
                        else
                            Some(path, (m.Groups.[1].ToString(), System.Int32.Parse(m.Groups.[2].ToString()))))
                    |> Seq.sortBy snd
                    |> Seq.map fst
                for file in ordered do
                    for line in File.ReadAllLines(file) do
                        yield LogEntry.Parse(line)
            }
            |> Seq.filter(fun entry -> not (entry.EntryType = LogEntryType.LogVersion || entry.EntryType = LogEntryType.PosChanged))
            |> Seq.fold (fun (previous, current) entry ->  // Keep the latest mission reports with the proper mission start date
                match current, entry with
                | _, (:? MissionStartEntry as start) ->
                    // Move current to previous if it's non empty and has lasted long enough (30 minutes)
                    let previous =
                        match current with
                        | [] -> previous
                        | (last : LogEntry) :: _ ->
                            if missionHasPassed30Min last then
                                current
                            else
                                previous
                    let expectedMissionFile = sprintf "Multiplayer/%s.msnbin" config.MissionName
                    if start.MissionTime = state.Date && start.MissionFile = expectedMissionFile then
                        previous, [entry] // Start new list
                    else
                        previous, [] // Start new skip
                | [], _ ->
                    previous, [] // Skip, looking for next mission start that has the right time
                | _ :: _, _ ->
                    previous, entry :: current // Recording, update current
            ) ([], [])
            |> function // current if it's longer than 30 min, previous otherwise
                | (previous, []) -> previous
                | (previous, ((last :: _) as current)) ->
                    if missionHasPassed30Min last then
                        current
                    else
                        previous
            |> List.rev

        if List.isEmpty entries then
            failwith "No entries found suitable for result extraction"
        let shipments =
            let shipmentAxis = extractSuppliesShipped state axisOrders.Resupply entries |> List.ofSeq
            let shipmentAllies = extractSuppliesShipped state alliesOrders.Resupply entries |> List.ofSeq
            shipmentAxis @ shipmentAllies
        let resups =
            let resupsAxis = extractResupplies world state axisOrders.Resupply entries |> List.ofSeq
            let resupsAllies = extractResupplies world state alliesOrders.Resupply entries |> List.ofSeq
            resupsAxis @ resupsAllies
        let staticDamages = extractStaticDamages world state entries |> List.ofSeq
        let vehicleDamages = extractVehicleDamages (axisOrders.Columns @ alliesOrders.Columns) (axisOrders.Resupply @ alliesOrders.Resupply)
        let takeOffs, landings =
            let both =
                extractTakeOffsAndLandings world state entries
                |> List.ofSeq
            both |> List.choose (function Choice1Of2 x -> Some x | _ -> None),
            both |> List.choose (function Choice2Of2 x -> Some x | _ -> None)
        let movements = axisOrders.Columns @ alliesOrders.Columns
        let columnDepartures = extractColumnDepartures movements entries |> List.ofSeq
        let columnArrivals = extractColumnArrivals world state movements entries |> List.ofSeq
        let dt = (1.0f<H>/60.0f) * float32 config.MissionLength

        let state2 = newState dt world state movements shipments resups staticDamages takeOffs landings columnDepartures columnArrivals

        (entries, shipments, resups, staticDamages, takeOffs, landings, columnDepartures, columnArrivals), (state, state2)

    let stage2 config (state, state2) =
        let outputDir = config.OutputDir

        let backupFile name =
            let backupName =
                let dateString =
                    state.Date.ToString("yyyy-MM-dd_HH-mm-ss")
                sprintf "%s_%s.xml" name dateString
            let backupDest = Path.Combine(outputDir, backupName)
            if File.Exists backupDest then
                File.Delete(backupDest)
            File.Copy(Path.Combine(outputDir, sprintf "%s.xml" name), backupDest)
    
        do
            let serializer = FsPickler.CreateXmlSerializer(indent = true)
            backupFile "state"
            backupFile "axisOrders"
            backupFile "alliesOrders"
            use stateFile = File.CreateText(Path.Combine(outputDir, "state.xml"))
            serializer.Serialize(stateFile, state2)
