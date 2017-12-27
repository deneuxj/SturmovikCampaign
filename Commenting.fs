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
open MBrace.FsPickler
open Campaign.Orders

type EventHandlers =
    // player name, coalition, airfield, plane, cargo
    { OnCargoTookOff : string * CoalitionId * AirfieldId * PlaneModel * float32<E> -> Async<unit>
      // playername, coalition, airfield, plane, cargo, health, damages inflicted
      OnLanded : string * CoalitionId * AirfieldId * PlaneModel * float32<E> * float32 * float32<E> -> Async<unit>
    }

/// <summary>
/// Watch the log directory, and report new events as they appear in the log files
/// </summary>
type Commentator (missionLogsDir : string, handlers : EventHandlers, world : World, state : WorldState, convoys : ResupplyOrder list, columns : ColumnMovement list) =
    // retrieve entries from most recent mission, if it matches the state's mission and start date.
    let files =
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
        ordered
        |> List.ofSeq
    let lines =
        seq {
            for file in files do
                yield! File.ReadAllLines(file)
        }
        |> List.ofSeq
    let initialEntries =
        lines
        |> Seq.map LogEntry.Parse
        |> Seq.split (function :? MissionStartEntry -> true | _ -> false)
        |> Seq.filter (fun group ->
            if Seq.isEmpty group then
                false
            else
                match Seq.head group with
                | :? MissionStartEntry as start ->
                    start.MissionTime = state.Date
                | _ ->
                    false)
        |> Seq.tryLast
        |> Option.defaultVal Seq.empty
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
                if not(alreadyHandled.Contains(ev.FullPath)) then
                    let entries2 =
                        try
                            [
                                for line in File.ReadAllLines(ev.FullPath) do
                                    yield LogEntry.Parse(line)
                            ]
                        with
                        | e ->
                            eprintfn "Failed to parse '%s' because '%s'" ev.FullPath e.Message
                            []
                    return! getEntry (entries2, Set.add  ev.FullPath alreadyHandled)
                else
                    return! getEntry (batched, alreadyHandled)
            | x :: xs ->
                return Some(x, (xs, alreadyHandled))
        }
    let asyncSeqEntries =
        AsyncSeq.unfoldAsync getEntry (initialEntries, Set.ofSeq files)
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    // Notify of interesting take-offs and landings
    do
        let task =
            let takeOffsAndLandings =
                asyncSeqEntries
                |> extractTakeOffsAndLandings world state
            let staticDamages =
                asyncSeqEntries
                |> extractStaticDamages world
            asyncSeq {
                let damageInflicted = ref Map.empty
                let wg = world.FastAccess
                let sg = state.FastAccess
                let convoys =
                    convoys
                    |> Seq.map (fun convoy -> convoy.OrderId, convoy)
                    |> Map.ofSeq
                let columns =
                    columns
                    |> Seq.map (fun column -> column.OrderId, column)
                    |> Map.ofSeq
                for event in AsyncSeq.mergeChoice takeOffsAndLandings staticDamages do
                    match event with
                    | Choice1Of2((TookOff { PlayerName = Some player }) as tookOff) ->
                        damageInflicted := Map.add player 0.0f<E> damageInflicted.Value
                        yield tookOff, 0.0f<E>
                    | Choice1Of2((Landed { PlayerName = Some player }) as landed) ->
                        let damage = damageInflicted.Value.TryFind player |> Option.defaultVal 0.0f<E>
                        damageInflicted := Map.remove player damageInflicted.Value
                        yield landed, damage
                    | Choice1Of2(TookOff { PlayerName = None }) | Choice1Of2(Landed { PlayerName = None }) -> ()
                    | Choice2Of2 damage ->
                        match damage.Data.ByPlayer with
                        | Some player ->
                            let acc = damageInflicted.Value.TryFind player |> Option.defaultVal 0.0f<E>
                            let cost =
                                match damage.Object with
                                | Production(region, idx) ->
                                    let pro = wg.GetRegion(region).Production.[idx]
                                    let lostDueToDamage =
                                        0.5f * pro.Production(world.SubBlockSpecs, world.ProductionFactor) * pro.RepairCost(world.SubBlockSpecs) * damage.Data.Amount / healLimit
                                    pro.RepairCost(world.SubBlockSpecs) + pro.Storage + lostDueToDamage / damage.Data.Amount
                                | Storage(region, idx) ->
                                    let sto = wg.GetRegion(region).Storage.[idx]
                                    sto.RepairCost(world.SubBlockSpecs) + sto.Storage
                                | Airfield(af, idx) ->
                                    let sto = wg.GetAirfield(af).Storage.[idx]
                                    sto.RepairCost(world.SubBlockSpecs) + sto.Storage
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
                                | ParkedPlane(af, plane) ->
                                    plane.Cost
                                | Cannon _ ->
                                    cannonCost
                                | LightMachineGun _ ->
                                    lightMachineGunCost
                                | HeavyMachineGun _ ->
                                    heavyMachineGunCost
                                | Vehicle(_, vehicle) ->
                                    vehicle.Cost
                            damageInflicted := Map.add player (acc + damage.Data.Amount * cost) damageInflicted.Value
                        | None ->
                            ()
            }
            |> AsyncSeq.iterAsync (fun (event, damage) ->
                async {
                    match event with
                    | TookOff ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        if true || x.Cargo > 0.0f<E> then
                            return! handlers.OnCargoTookOff(player, coalition, x.Airfield, x.Plane, x.Cargo)
                    | Landed ({PlayerName = Some player; Coalition = Some coalition} as x) ->
                        if true || x.Cargo > 0.0f<E> || x.Health < 1.0f then
                            return! handlers.OnLanded(player, coalition, x.Airfield, x.Plane, x.Cargo, x.Health, damage)
                    | _ ->
                        return()
                })
        Async.Start(task, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()

/// Monitor state.xml, (re-) starting a commentator whenever the file is modified
type CommentatorRestarter(missionLogsDir : string, campaignDir : string, handlers : EventHandlers, onStateWritten : unit -> unit) =
    let watcher = new FileSystemWatcher()
    do watcher.Path <- campaignDir
       watcher.Filter <- "*.xml"
       watcher.NotifyFilter <- NotifyFilters.LastWrite
    let serializer = FsPickler.CreateXmlSerializer()
    let worldFile = File.OpenText(Path.Combine(campaignDir, "world.xml"))
    let world = serializer.Deserialize<World>(worldFile)
    do worldFile.Dispose()
    let rec awaitStateAndOrders remaining =
        async {
            if Set.isEmpty remaining then
                return ()
            else
                let! ev = Async.AwaitEvent watcher.Changed
                return! awaitStateAndOrders (Set.remove ev.Name remaining)
        }
    let rec work commentator =
        async {
            do! awaitStateAndOrders (Set["state.xml"; "axisOrders.xml"; "alliesOrders.xml"])
            onStateWritten()
            match commentator with
            | Some (commentator : Commentator) -> commentator.Dispose()
            | None -> ()
            let state =
                try
                    use stateFile = File.OpenText(Path.Combine(campaignDir, "state.xml"))
                    serializer.Deserialize<WorldState>(stateFile) |> Some
                with
                | exc ->
                    eprintfn "Failed to parse state.xml: %s" exc.Message
                    None
            let axisOrders =
                try
                    use ordersFile = File.OpenText(Path.Combine(campaignDir, "axisOrders.xml"))
                    serializer.Deserialize<OrderPackage>(ordersFile) |> Some
                with
                | exc ->
                    eprintfn "Failed to parse axisOrders.xml: %s" exc.Message
                    None
            let alliesOrders =
                try
                    use ordersFile = File.OpenText(Path.Combine(campaignDir, "alliesOrders.xml"))
                    serializer.Deserialize<OrderPackage>(ordersFile) |> Some
                with
                | exc ->
                    eprintfn "Failed to parse alliesOrders.xml: %s" exc.Message
                    None
            match state, axisOrders, alliesOrders with
            | Some state, Some axisOrders, Some alliesOrders ->
                let commentator = new Commentator(missionLogsDir, handlers, world, state, axisOrders.Resupply @ alliesOrders.Resupply, axisOrders.Columns @ alliesOrders.Columns)
                return! work(Some commentator)
            | _ ->
                return! work None
        }
    // Stop notifications when we are disposed
    let cancelOnDispose = new System.Threading.CancellationTokenSource()
    do Async.Start(work None, cancelOnDispose.Token)
    do watcher.EnableRaisingEvents <- true

    member this.Dispose() =
        watcher.Dispose()
        cancelOnDispose.Cancel()