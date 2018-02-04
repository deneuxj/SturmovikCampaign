module Campaign.Commenting

open System
open System.IO
open System.Text.RegularExpressions
open Campaign.BasicTypes
open Campaign.Configuration
open Util
open ploggy
open System.Numerics
open FSharp.Control
open Campaign.ResultExtraction
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.BasicTypes
open Campaign.PlaneModel
open Campaign.PlayerDiscipline
open MBrace.FsPickler
open Campaign.Orders
open SturmovikMission.Blocks.Util.String

let private logger = NLog.LogManager.GetCurrentClassLogger()

type EventHandlers =
    // player name, coalition, airfield, coalition of airfield, plane, cargo
    { OnTookOff : string * CoalitionId * AirfieldId * CoalitionId option * PlaneModel * float32<E> -> Async<unit>
      // playername, coalition of player, airfield, coalition of airfield, plane, cargo, health, damages inflicted
      OnLanded : string * CoalitionId * AirfieldId * CoalitionId option * PlaneModel * float32<E> * float32 * float32<E> -> Async<unit>
      // Region name, attacker coalition
      OnMaxBattleDamageExceeded : string * CoalitionId -> Async<unit>
      // A player was banned or kicked
      OnPlayerPunished : Judgement -> Async<unit>
    }

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (config : Configuration, handlers : EventHandlers, world : World, state : WorldState, convoys : ResupplyOrder list, columns : ColumnMovement list) =
    let missionLogsDir = Path.Combine(config.ServerDataDir, "logs")
    // retrieve entries from most recent mission
    let files =
        let unordered = Directory.EnumerateFiles(missionLogsDir, "missionReport*.txt")
        let r = Regex(@"(missionReport\(.*\))\[([0-9]+)\]\.txt")
        let filtered =
            unordered
            |> Seq.choose(fun path ->
                let m = r.Match(Path.GetFileName(path))
                if not m.Success then
                    None
                else
                    Some(path, (m.Groups.[1].ToString(), System.Int32.Parse(m.Groups.[2].ToString()))))
        let lastGroup =
            filtered
            |> Seq.groupBy (snd >> fst)
            |> Seq.tryLast
            |> Option.map snd
            |> Option.defaultVal Seq.empty
        let sorted =
            lastGroup
            |> Seq.sortBy snd
            |> Seq.map fst
        sorted
        |> List.ofSeq
    let initialEntries =
        seq {
            for file in files do
                yield! File.ReadAllLines(file)
        }
        |> Seq.choose (fun line ->
            try
                (line, LogEntry.Parse(line))
                |> Some
            with _ -> None)
        |> List.ofSeq
    let watcher = new FileSystemWatcher()
    do watcher.Path <- missionLogsDir
       watcher.Filter <- "missionReport*.txt"
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    // An AsyncSeq built from new file notifications: generate log entries
    let rec getEntry (batched, alreadyHandled : Set<string>) = 
        async {
            match batched with
            | [] ->
                let! ev = Async.AwaitEvent watcher.Changed
                let entries2 =
                    try
                        [
                            for line in File.ReadAllLines(ev.FullPath) do
                                if not(alreadyHandled.Contains line) then
                                    yield line, LogEntry.Parse(line)
                                else
                                    logger.Info(sprintf "Skipping log line %s" line)
                        ]
                    with
                    | e ->
                        logger.Warn(sprintf "Failed to parse '%s' because '%s'" ev.FullPath e.Message)
                        []
                let alreadyHandled =
                    entries2
                    |> Seq.map fst
                    |> Seq.fold (fun alreadyHandled x -> Set.add x alreadyHandled) alreadyHandled
                let batched =
                    entries2
                    |> List.map snd
                // We are initially called with a batch corresponding to the initial entries.
                // If we are in this branch, it means the initial batch has been consumed by now, or it was empty.
                // In either case, returning Some(None, state) will generate a None, which will be recognized as the signal to start calling the handlers.
                let notifyStartEmittingMessages = None
                return Some(notifyStartEmittingMessages, (batched, alreadyHandled))
            | x :: xs ->
                return Some(Some x, (xs, alreadyHandled))
        }
    let asyncSeqEntries =
        AsyncSeq.unfoldAsync getEntry (initialEntries |> List.map snd, initialEntries |> List.map fst |> Set.ofList)
    let startEmitting =
        asyncSeqEntries
        |> AsyncSeq.filter (Option.isNone)
        |> AsyncSeq.map (fun _ -> ())
    let asyncSeqEntries =
        asyncSeqEntries
        |> AsyncSeq.choose id
    let asyncIterNonMuted f xs =
        let work =
            AsyncSeq.mergeChoice startEmitting xs
            |> AsyncSeq.foldAsync (fun muted item ->
                async {
                    match item with
                    | Choice1Of2() ->
                        // Unmute
                        return false
                    | Choice2Of2 x->
                        if not muted then
                            logger.Trace(sprintf "Do: %A" x)
                            do! f x
                        else
                            logger.Trace(sprintf "Muted: %A" x)
                        return muted
                }) true
        async {
            let! _ = work
            return ()
        }
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    // Notify of interesting take-offs and landings
    do
        let battleDamages =
            let battles =
                Battlefield.identifyBattleAreas world state
                |> Seq.cache
            asyncSeqEntries
            |> extractBattleDamages world state battles
        let takeOffsAndLandings =
            asyncSeqEntries
            |> extractTakeOffsAndLandings world state
        let staticDamages =
            asyncSeqEntries
            |> extractStaticDamages world
        let vehicleDamages =
            asyncSeqEntries
            |> extractVehicleDamages columns convoys
        let damages = AsyncSeq.merge staticDamages vehicleDamages
        let wg = world.FastAccess
        let sg = state.FastAccess
        let task =
            asyncSeq {
                let damageInflicted = ref Map.empty
                let coalitionOf = ref Map.empty
                let convoys =
                    convoys
                    |> Seq.map (fun convoy -> convoy.OrderId, convoy)
                    |> Map.ofSeq
                let columns =
                    columns
                    |> Seq.map (fun column -> column.OrderId, column)
                    |> Map.ofSeq
                for event in AsyncSeq.mergeChoice3 takeOffsAndLandings damages battleDamages do
                    match event with
                    | Choice1Of3((TookOff { PlayerName = Some player; Coalition = coalition }) as tookOff) ->
                        damageInflicted := Map.add player 0.0f<E> damageInflicted.Value
                        coalitionOf := Map.add player coalition coalitionOf.Value
                        yield tookOff, 0.0f<E>
                    | Choice1Of3((Landed { PlayerName = Some player }) as landed) ->
                        let damage = damageInflicted.Value.TryFind player |> Option.defaultVal 0.0f<E>
                        damageInflicted := Map.remove player damageInflicted.Value
                        yield landed, damage
                    | Choice1Of3(TookOff { PlayerName = None }) | Choice1Of3(Landed { PlayerName = None }) -> ()
                    | Choice2Of3 damage ->
                        match damage.Data.ByPlayer with
                        | Some player ->
                            let acc = damageInflicted.Value.TryFind player |> Option.defaultVal 0.0f<E>
                            let coalition = coalitionOf.Value.TryFind player |> Option.bind id
                            let cost =
                                match damage.Object with
                                | Production(region, idx) ->
                                    let pro = wg.GetRegion(region).Production.[idx]
                                    let lostDueToDamage =
                                        0.5f * pro.Production(world.SubBlockSpecs, world.ProductionFactor) * pro.RepairCost(world.SubBlockSpecs) * damage.Data.Amount / healLimit
                                    pro.RepairCost(world.SubBlockSpecs) + pro.Storage world.SubBlockSpecs + lostDueToDamage / damage.Data.Amount
                                | Storage(region, idx) ->
                                    let sto = wg.GetRegion(region).Storage.[idx]
                                    sto.RepairCost(world.SubBlockSpecs) + sto.Storage world.SubBlockSpecs
                                | Airfield(af, idx) ->
                                    let sto = wg.GetAirfield(af).Storage.[idx]
                                    sto.RepairCost(world.SubBlockSpecs) + sto.Storage world.SubBlockSpecs
                                | Convoy vehicle ->
                                    match convoys.TryFind(vehicle.OrderId) with
                                    | Some order ->
                                        match order.Means with
                                        | ByRiverShip
                                        | BySeaShip -> ResupplyOrder.ShipCapacity
                                        | ByAir(_, _) -> order.Convoy.TransportedSupplies
                                        | ByRail -> order.Convoy.TransportedSupplies
                                        | ByRoad -> ResupplyOrder.TruckCapacity
                                    | None ->
                                        0.0f<E>
                                | Column vehicle ->
                                    match columns.TryFind(vehicle.OrderId) with
                                    | Some order ->
                                        match order.TransportType with
                                        | ColByRiverShip
                                        | ColBySeaShip ->
                                            order.Composition
                                            |> Seq.sumBy (fun x -> x.Cost)
                                            |> fun x -> x * 0.5f
                                        | ColByTrain ->
                                            order.Composition
                                            |> Seq.sumBy (fun x -> x.Cost)
                                        | ColByRoad ->
                                            order.Composition.[vehicle.Rank].Cost
                                    | None ->
                                        0.0f<E>
                                | ParkedPlane(_, plane) ->
                                    plane.Cost
                                | Cannon _ ->
                                    cannonCost
                                | LightMachineGun _ ->
                                    lightMachineGunCost
                                | HeavyMachineGun _ ->
                                    heavyMachineGunCost
                                | Vehicle(_, vehicle) ->
                                    vehicle.Cost
                                | ActivePlane(_, plane) ->
                                    plane.Cost
                            // friendly-fire modifier: -1 coefficient
                            let penalty =
                                if coalition = damage.Object.Coalition(wg, sg) then
                                    -1.0f
                                else
                                    1.0f
                            damageInflicted := Map.add player (acc + penalty * damage.Data.Amount * cost) damageInflicted.Value
                        | None ->
                            ()
                    | Choice3Of3 ({ KilledByPlayer = Some killer } as battleKill) ->
                        match coalitionOf.Value.TryFind(killer) |> Option.bind id with
                        | Some coalition ->
                            let penalty =
                                if coalition = battleKill.Coalition then
                                    -1.0f
                                else
                                    1.0f
                            let acc = damageInflicted.Value.TryFind killer |> Option.defaultVal 0.0f<E>
                            damageInflicted := Map.add killer (acc + penalty * battleKill.Vehicle.Cost / float32 config.BattleKillRatio) damageInflicted.Value
                        | None ->
                            ()
                    | Choice3Of3 { KilledByPlayer = None } -> ()
                }
            |> asyncIterNonMuted(fun (event, damage) ->
                async {
                    match event with
                    | TookOff ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        let afCoalition = sg.GetRegion(wg.GetAirfield(x.Airfield).Region).Owner
                        return! handlers.OnTookOff(player, coalition, x.Airfield, afCoalition, x.Plane, x.Cargo)
                    | Landed ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        let afCoalition = sg.GetRegion(wg.GetAirfield(x.Airfield).Region).Owner
                        return! handlers.OnLanded(player, coalition, x.Airfield, afCoalition, x.Plane, x.Cargo, x.Health, damage)
                    | _ ->
                        return ()
                })
        let task2 =
            asyncSeq {
                let battleDamage = ref Map.empty
                for battleKill in battleDamages do
                    let oldBattleDamage =
                        battleDamage.Value.TryFind (battleKill.BattleId, battleKill.Coalition)
                        |> Option.defaultValue 0.0f<E>
                    let newDamage = oldBattleDamage + battleKill.Vehicle.Cost / (float32 config.BattleKillRatio)
                    battleDamage := Map.add (battleKill.BattleId, battleKill.Coalition) newDamage battleDamage.Value
                    let totalValue =
                        GroundAttackVehicle.AllVehicles
                        |> Seq.sumBy (fun vehicle ->
                            (float32(sg.GetRegion(battleKill.BattleId).GetNumVehicles(battleKill.Coalition, vehicle))) * vehicle.Cost)
                    if newDamage > totalValue * config.MaxBattleKillsRatioByPlayers then
                        yield battleKill.Coalition.Other, battleKill.BattleId
            }
            |> asyncIterNonMuted (fun (coalition, region) -> handlers.OnMaxBattleDamageExceeded(string region, coalition))
        let disciplineTask =
            disciplinePlayers world asyncSeqEntries
            |> asyncIterNonMuted (fun penalty -> handlers.OnPlayerPunished penalty)
        Async.Start(Async.catchLog "live commentator" task, cancelOnDispose.Token)
        Async.Start(Async.catchLog "battle limits notifier" task2, cancelOnDispose.Token)
        Async.Start(Async.catchLog "abuse detector" disciplineTask, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()

/// Monitor state.xml, (re-) starting a commentator whenever the file is modified
type CommentatorRestarter(config : Configuration, handlers : EventHandlers, onStateWritten : unit -> unit) =
    let missionName = config.MissionName
    let campaignDir = config.OutputDir
    let missionFile = missionName + ".mission"
    let watcher = new FileSystemWatcher()
    do watcher.Path <- campaignDir
       watcher.Filter <- missionFile
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    let serializer = FsPickler.CreateXmlSerializer()
    let rec awaitFiles remaining =
        async {
            if Set.isEmpty remaining then
                return ()
            else
                let! ev = Async.AwaitEvent watcher.Changed
                return! awaitFiles (Set.remove ev.Name remaining)
        }
    let rec work world commentator =
        async {
            match commentator with
            | Some (commentator : Commentator) -> commentator.Dispose()
            | None -> ()
            let state =
                try
                    use stateFile = File.OpenText(Path.Combine(campaignDir, "state.xml"))
                    serializer.Deserialize<WorldState>(stateFile) |> Some
                with
                | exc ->
                    logger.Error(sprintf "Failed to parse state.xml: %s" exc.Message)
                    None
            let axisOrders =
                try
                    use ordersFile = File.OpenText(Path.Combine(campaignDir, "axisOrders.xml"))
                    serializer.Deserialize<OrderPackage>(ordersFile) |> Some
                with
                | exc ->
                    logger.Error(sprintf "Failed to parse axisOrders.xml: %s" exc.Message)
                    None
            let alliesOrders =
                try
                    use ordersFile = File.OpenText(Path.Combine(campaignDir, "alliesOrders.xml"))
                    serializer.Deserialize<OrderPackage>(ordersFile) |> Some
                with
                | exc ->
                    logger.Error(sprintf "Failed to parse alliesOrders.xml: %s" exc.Message)
                    None
            match state, axisOrders, alliesOrders with
            | Some state, Some axisOrders, Some alliesOrders ->
                logger.Info("Starting new commentator")
                let commentator = new Commentator(config, handlers, world, state, axisOrders.Resupply @ alliesOrders.Resupply, axisOrders.Columns @ alliesOrders.Columns)
                do! awaitFiles (Set[missionFile])
                onStateWritten()
                return! work world (Some commentator)
            | _ ->
                logger.Info(sprintf "Waiting until next change to %s" missionFile)
                do! awaitFiles (Set[missionFile])
                onStateWritten()
                return! work world None
        }
    // Load world, then repeatedly monitor for new state and order files
    let prepare =
        async {
            let worldPath = Path.Combine(campaignDir, "world.xml")
            let missionPath = Path.Combine(campaignDir, missionFile)
            let rec loadWorld() =
                async {
                    // Wait until the mission file exists. When that file exists, we know that world.xml is ready to be read.
                    if not (File.Exists(missionPath)) then
                        do! Async.Sleep 5000
                        return! loadWorld()
                    else
                        use worldFile = File.OpenText(worldPath)
                        let world = serializer.Deserialize<World>(worldFile)
                        return world
                }
            let! world = loadWorld()
            return! work world None
        }
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    do Async.Start(prepare, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()