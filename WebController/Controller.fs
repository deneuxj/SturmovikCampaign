namespace Campaign.WebController

open System.IO
open FSharp.Control
open FSharp.Json

open MBrace.FsPickler

open Util
open Util.Json

open Campaign
open Campaign.Common

open Campaign.NewWorldDescription
open Campaign.NewWorldDescription.IO
open Campaign.WarState
open Campaign.WarState.IO
open Campaign.GameServerSync.BaseFileNames
open Campaign.WebController.Dto
open Campaign.WebController.DtoCreation
open Campaign.WebController.Routes

/// Cached query results and intermediate data
type private ControllerStateCaches =
    {
        WarStateCache : Map<int, IWarStateQuery>
        RoadTransportCache : Map<int, BasicTypes.RegionId * BasicTypes.RegionId -> float>
        RailTransportCache : Map<int, BasicTypes.RegionId * BasicTypes.RegionId -> float>
        DtoDates : Dto.DateTime[] option
        DtoWorld : Dto.World option
        DtoStateCache : Map<int, Dto.WarState>
        DtoSimulationCache : Map<int, Dto.SimulationStep[]>
    }
with
    static member Default =
        {
            WarStateCache = Map.empty
            RoadTransportCache = Map.empty
            RailTransportCache = Map.empty
            DtoDates = None
            DtoWorld = None
            DtoStateCache = Map.empty
            DtoSimulationCache = Map.empty
        }

    member this.PickleCaches(stream) =
        let serializer = FsPickler.CreateBinarySerializer()
        serializer.Serialize(stream, this)

    static member RestoreCaches(stream) =
        let serializer = FsPickler.CreateBinarySerializer()
        let data : ControllerStateCaches = serializer.Deserialize(stream)
        data

/// Internal state of a controller
type private ControllerState =
    {
        World : NewWorldDescription.World option
        War : IWarStateQuery option
        Sync : (GameServerSync.Sync * System.IDisposable) option
    }
with
    static member Default =
        {
            World = None
            War = None
            Sync = None
        }


/// Interface between the web service and the campaign
type Controller(settings : GameServerControl.Settings) =
    let logger = NLog.LogManager.GetCurrentClassLogger()

    // Cache Player GUID hashes
    let cache = Seq.mutableDict []
    let hashGuid = SturmovikMission.Cached.cached cache hashGuid

    // Try to restore controller state from pickled data
    let stateCachePath = System.IO.Path.Combine(settings.WorkDir, "cache.bin")

    let writeCaches(cache : ControllerStateCaches) =
        try
            use stream = System.IO.File.Create(stateCachePath)
            cache.PickleCaches(stream)
        with e ->
            logger.Warn("Failed to pickle controller state caches to {CachePath}", stateCachePath)

    // A mailbox processor to serialize access to the ControllerState
    let mb = new MailboxProcessor<_>(fun mb ->
        let rec work (state : ControllerState, cache : ControllerStateCaches) =
            async {
                let! req = mb.Receive()
                let! state2, cache2  = req(state, cache)
                return! work(state2, cache2)
            }
        let state0 = ControllerState.Default
        let cache0 =
            if File.Exists stateCachePath then
                try
                    use stream = System.IO.File.OpenRead(stateCachePath)
                    ControllerStateCaches.RestoreCaches(stream)
                with e ->
                    logger.Warn("Failed to restore controller state caches from {CachePath}", stateCachePath)
                    logger.Debug(e)
                    ControllerStateCaches.Default
            else
                ControllerStateCaches.Default
        work(state0, cache0)
    )

    let doOnState fn = mb.Post fn
    
    let wkPath f = Path.Combine(settings.WorkDir, f)

    let registerOnStateChanged(sync : GameServerSync.Sync) =
        sync.StateChanged.Subscribe(fun war ->
            doOnState (fun (s, cache) -> async {
                let dates =
                    match cache.DtoDates with
                    | Some [| |] ->
                        Some [| war.Date.ToDto() |]
                    | Some dates ->
                        let warDate = war.Date.ToDto()
                        if dates.[dates.Length - 1] <> warDate then
                            Array.append dates [| warDate |]
                        else
                            dates
                        |> Some
                    | None ->
                        None
                return { s with War = Some war}, { cache with DtoDates = dates }
            })
        )

    do
        mb.Start()
        // Create work area if it doesn't exist
        if not(Directory.Exists settings.WorkDir) then
            try
                Directory.CreateDirectory settings.WorkDir
                |> ignore
            with
            | e -> failwithf "Directory '%s' not found, and could not be created because: %s" settings.WorkDir e.Message


    member this.SaveCaches() =
        mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
            writeCaches(cache)
            channel.Reply()
            return (s, cache)
        }

    member this.GetWorldDto() =
        mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
            return
                match cache.DtoWorld with
                | Some world ->
                    channel.Reply(Ok(world))
                    s, cache
                | None ->
                    let path = wkPath worldFilename
                    if not(File.Exists(path)) then
                        channel.Reply(Error "Campaign not initialized")
                        s, cache
                    else
                        try
                            let world =
                                s.World
                                |> Option.defaultWith (fun () -> World.LoadFromFile(wkPath worldFilename))
                            let dto = world.ToDto()
                            channel.Reply(Ok(dto))
                            { s with World = Some world }, { cache with DtoWorld = Some dto }
                        with e ->
                            logger.Warn(sprintf "Failed to load world: %s" e.Message)
                            logger.Warn(e)
                            channel.Reply(Error "Failed to load world")
                            s, cache
        }

    member this.GetState(idx : int) =
        mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
            match cache.WarStateCache.TryFind idx with
            | Some state ->
                channel.Reply(Ok state)
                return s, cache
            | None ->
                let path = wkPath(getStateFilename idx)
                if File.Exists path then
                    try
                        let world =
                            s.World
                            |> Option.defaultWith (fun () -> World.LoadFromFile(wkPath worldFilename))
                        let state = WarState.LoadFromFile(path, world)
                        channel.Reply(Ok(upcast state))
                        return { s with World = Some world }, { cache with WarStateCache = cache.WarStateCache.Add(idx, state) }
                    with
                    | e ->
                        channel.Reply(Error <| sprintf "Failed to load state n.%d" idx)
                        logger.Warn e
                        return s, cache
                else
                    channel.Reply(Error <| sprintf "Failed to load state n.%d" idx)
                    return s, cache
        }

    member this.GetState() =
        async {
            let idx = getCurrentIndex settings.WorkDir
            return! this.GetState(idx)
        }

    member this.GetRoadTransport(idx : int) =
        mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
            match cache.RoadTransportCache.TryFind idx with
            | Some func ->
                channel.Reply(Ok func)
                return s, cache
            | None ->
                match cache.WarStateCache.TryFind(idx) with
                | Some state ->
                    let func = state.ComputeRoadCapacity() >> float
                    channel.Reply(Ok func)
                    return s, { cache with RoadTransportCache = cache.RoadTransportCache.Add(idx, func) }
                | None ->
                    channel.Reply(Error "Failed to retrieve road transport capacity")
                    return s, cache
        }

    member this.GetRailTransport(idx : int) =
        mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
            match cache.RailTransportCache.TryFind idx with
            | Some func ->
                channel.Reply(Ok func)
                return s, cache
            | None ->
                match cache.WarStateCache.TryFind(idx) with
                | Some state ->
                    let func = state.ComputeRailCapacity() >> float
                    channel.Reply(Ok func)
                    return s, { cache with RailTransportCache = cache.RailTransportCache.Add(idx, func) }
                | None ->
                    channel.Reply(Error "Failed to retrieve rail transport capacity")
                    return s, cache
        }

    member this.GetStateDto() =
        async {
            let idx = getCurrentIndex settings.WorkDir
            return! this.GetStateDto(idx)
        }

    /// Try to get a recorded state by its number
    member this.GetStateDto(idx : int) =
        async {
            let! state = this.GetState(idx)
            match state with
            | Ok state ->
                return!
                    mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                        return
                            match cache.DtoStateCache.TryGetValue idx with
                            | true, x ->
                                channel.Reply(Ok x)
                                s, cache
                            | false, _ ->
                                let dto = state.ToDto()
                                channel.Reply(Ok dto)
                                s, { cache with DtoStateCache = cache.DtoStateCache.Add(idx, dto) }
                    }
            | Error e ->
                return(Error e)
        }

    /// Try to get the simulation steps leading to the state identified by the given index
    member this.GetSimulationDto(idx : int) =
        // Filter Timestamp steps with zero commands.
        let noTsSteps (steps : SimulationStep []) =
            steps
            |> Array.filter (fun dto -> dto.Description <> "Timestamp" || dto.Command.Length > 0 )

        mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
            return
                match cache.DtoSimulationCache.TryGetValue idx with
                | true, x ->
                    channel.Reply(Ok (noTsSteps x))
                    s, cache
                | false, _ ->
                    // Optional effects file
                    let path1 = wkPath(getEffectsFilename idx)
                    // Mandatory simulation file
                    let path2 = wkPath(getSimulationFilename idx)
                    if File.Exists path2 then
                        try
                            let world =
                                s.World
                                |> Option.defaultWith (fun () -> World.LoadFromFile(wkPath worldFilename))
                            let state = WarState.LoadFromFile(wkPath (getStateFilename idx), world)
                            let effects =
                                if File.Exists path1 then
                                    use reader = new StreamReader(path1)
                                    let json = reader.ReadToEnd()
                                    Json.deserializeEx JsonConfig.IL2Default json
                                else
                                    []
                            let simulation =
                                use reader = new StreamReader(path2)
                                let json = reader.ReadToEnd()
                                Json.deserializeEx JsonConfig.IL2Default json
                            let steps =
                                List.append effects simulation
                                |> List.map (fun (description : string, command : WarStateUpdate.Commands option, results : WarStateUpdate.Results list) ->
                                    { Dto.SimulationStep.Description = description
                                      Dto.Command = command |> Option.map Array.singleton |> Option.defaultValue [||] |> Array.map (fun x -> x.ToDto(state))
                                      Dto.Results = results |> List.map (fun r -> r.ToDto(state)) |> Array.ofList
                                    }
                                )
                                |> Array.ofList
                            channel.Reply(Ok (noTsSteps steps))
                            { s with World = Some world }, { cache with DtoSimulationCache = cache.DtoSimulationCache.Add(idx, steps) }
                        with
                        | e ->
                            channel.Reply(Error <| sprintf "Failed to load simulation n.%d" idx)
                            logger.Warn e
                            s, cache
                    else
                        channel.Reply(Error <| sprintf "No simulation n.%d" idx)
                        s, cache
        }

    member this.GetDates() =
        async {
            try
                let N = getCurrentIndex settings.WorkDir

                let! dates =
                    mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                        match cache.DtoDates with
                        | Some dates ->
                            channel.Reply(dates)
                            return s, cache
                        | None ->
                            let! dates =
                                AsyncSeq.initAsync (int64 N + 1L) (fun idx ->
                                    async {
                                        let idx = int idx
                                        match cache.DtoStateCache.TryGetValue idx with
                                        | true, x ->
                                            return (Some x.Date)
                                        | false, _ ->
                                            let path = wkPath(getStateFilename idx)
                                            if File.Exists path then
                                                try
                                                    let world =
                                                        s.World
                                                        |> Option.defaultWith (fun () -> World.LoadFromFile(wkPath worldFilename))
                                                    let state = WarState.LoadFromFile(path, world)
                                                    return (Some (state.Date.ToDto()))
                                                with
                                                | e ->
                                                    logger.Warn e
                                                    return None
                                            else
                                                return None
                                    }
                                )
                                |> AsyncSeq.choose id
                                |> AsyncSeq.toArrayAsync
                            channel.Reply(dates)
                            return s, { cache with DtoDates = Some dates }
                    }
                return Ok dates
            with e ->
                logger.Warn e
                return (Error "Failed to retrieve dates")
        }

        member this.Run(maxSteps) =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                let! sync =
                    match s.Sync with
                    | Some sync -> async.Return (Ok sync)
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            let disp = registerOnStateChanged sync
                            let! status = sync.Init()
                            match status with
                            | Ok _ -> return Ok(sync, disp)
                            | Error e -> return Error e
                        }
                match sync with
                | Ok(sync, onStateChanged) ->
                    let rec work stepsLeft =
                        async {
                            if stepsLeft <= 0 then
                                channel.Reply(Ok "Scenario advanced")
                            else
                                let! r = sync.Advance()
                                match r with
                                | Error s -> channel.Reply(Error s)
                                | Ok _ -> return! work (stepsLeft - 1)
                        }
                    do! work maxSteps
                    sync.Die("Scenario was forcibly advanced")
                    sync.Dispose()
                    onStateChanged.Dispose()
                    return { s with Sync = None }, cache
                | Error e ->
                    channel.Reply(Error e)
                    return { s with Sync = None }, cache
            }

        member this.BackTo(idx) =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                let! sync =
                    match s.Sync with
                    | Some sync -> async.Return (Ok sync)
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            let disp = registerOnStateChanged sync
                            let! status = sync.Init()
                            match status with
                            | Ok _ -> return Ok(sync, disp)
                            | Error e -> return Error e
                        }
                match sync with
                | Ok(sync, onStateChanged) ->
                    let r = sync.Back(idx)
                    channel.Reply(r)
                    sync.Die("Scenario was forcibly backed")
                    sync.Dispose()
                    onStateChanged.Dispose()
                    // Clear cache entries that now refer to the future
                    let cache =
                        { cache with
                            WarStateCache = cache.WarStateCache |> Map.filter (fun idx2 _ -> idx2 <= idx)
                            DtoDates = cache.DtoDates |> Option.map (Array.truncate idx)
                            DtoStateCache = cache.DtoStateCache |> Map.filter (fun idx2 _ -> idx2 <= idx)
                            DtoSimulationCache = cache.DtoSimulationCache |> Map.filter (fun idx2 _ -> idx2 < idx)
                        }
                    return
                        { s with
                            War = None
                            Sync = None
                        }, cache
                | Error e ->
                    channel.Reply(Error e)
                    return { s with Sync = None }, cache
            }

        member this.Forward() =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                let! sync =
                    match s.Sync with
                    | Some sync -> async.Return (Ok sync)
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            let disp = registerOnStateChanged sync
                            let! status = sync.Init()
                            match status with
                            | Ok _ -> return Ok(sync, disp)
                            | Error e -> return Error e
                        }
                match sync with
                | Ok(sync, onStateChanged) ->
                    let! r = sync.Forward()
                    channel.Reply(r)
                    sync.Die("Scenario was forwarded using backed up game logs")
                    sync.Dispose()
                    onStateChanged.Dispose()
                    return { s with Sync = None }, cache
                | Error e ->
                    channel.Reply(Error e)
                    return { s with Sync = None }, cache
            }

        member this.GetSyncState() =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                channel.Reply(
                    s.Sync
                    |> Option.map (fun (sync, _) ->
                        match sync.SyncState with
                        | Some(GameServerSync.PreparingMission _) -> "Preparing mission"
                        | Some GameServerSync.ResavingMission -> "Resaving missions"
                        | Some(GameServerSync.ExtractingResults _) -> "Extracting results"
                        | Some(GameServerSync.SkippingMission) -> "Skipping mission"
                        | Some GameServerSync.AdvancingScenario -> "Advancing scenario"
                        | Some(GameServerSync.RunningMission(_, endTime)) -> sprintf "Running mission (%03d minutes left)" (int (endTime - System.DateTime.UtcNow).TotalMinutes)
                        | Some(GameServerSync.ErrorState(msg, _)) -> sprintf "Error: %s" msg
                        | None -> "Unknown")
                    |> Option.defaultValue "Not running"
                    |> Ok
                    )
                return s, cache
            }

        member this.WarState =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                match s.War with
                | Some war ->
                    channel.Reply(Ok war)
                    return s, cache
                | None ->
                    let idx = getCurrentIndex settings.WorkDir
                    let path = wkPath(getStateFilename idx)
                    try
                        let world = World.LoadFromFile(wkPath worldFilename)
                        let war = WarState.LoadFromFile(path, world)
                        channel.Reply(Ok (upcast war))
                        return { s with World = Some war.World; War = Some(upcast war) }, cache
                    with exc ->
                        channel.Reply(Error "Failed to load current war state")
                        return s, cache
            }

        member this.GetPilots(filter : PilotSearchFilter) =
            async {
                let! war = this.WarState
                match war with
                | Ok war ->
                    let filterFuns =
                        [
                            match filter.Health with
                            | Some OnlyHealthy ->
                                yield fun (pilot : Pilots.Pilot) -> pilot.Health = Pilots.PilotHealth.Healthy
                            | Some NoDead ->
                                yield fun pilot -> not(pilot.Health = Pilots.PilotHealth.Dead)
                            | None ->
                                ()

                            match filter.Coalition with
                            | Some value ->
                                match
                                    (try
                                        BasicTypes.CoalitionId.FromString value
                                        |> Some
                                     with _ -> None)
                                    with
                                | None ->
                                    yield fun _ -> false
                                | Some coalition ->
                                    yield
                                        fun pilot ->
                                            war.World.Countries.TryGetValue(pilot.Country) = (true, coalition)
                            | None ->
                                ()

                            match filter.Country with
                            | Some value ->
                                match BasicTypes.CountryId.FromString value with
                                | None ->
                                    yield fun _ -> false
                                | Some country ->
                                    yield
                                        fun pilot -> pilot.Country = country
                            | None ->
                                ()

                            match filter.NamePattern with
                            | Some pattern ->
                                let pattern = pattern.Trim().ToLowerInvariant()
                                yield
                                    fun pilot ->
                                        (pilot.PilotFirstName + " " + pilot.PilotLastName).ToLowerInvariant().Contains(pattern)
                            | None ->
                                ()
                        ]
                    return
                        war.Pilots
                        |> List.filter (fun pilot -> filterFuns |> List.forall (fun f -> f pilot))
                        |> List.sortByDescending (fun pilot -> pilot.LatestFlightStart)
                        |> List.map (fun pilot -> pilot.ToDto(war))
                        |> List.truncate 100
                        |> Ok
                | Error e ->
                    return Error e
            }

        member this.GetPilot(id : string) =
            async {
                let! war = this.WarState
                return
                    match war with
                    | Ok war ->
                        try
                            let id = Pilots.PilotId(System.Guid(id))
                            let pilot = war.GetPilot(id)
                            let flights =
                                pilot.Flights
                                |> List.map (fun flight -> flight.ToDto(war.World))
                            Ok {| Pilot = pilot.ToDto(war); Missions = flights |}
                        with _ -> Error "Pilot not found"
                    | Error e ->
                        Error e
            }

        member this.GetPlayerPilots(hashedGuid : HashedGuid) =
            async {
                match! this.WarState with
                | Ok war ->
                    return
                        war.Pilots
                        |> Seq.filter (fun pilot -> hashGuid pilot.PlayerGuid = hashedGuid)
                        |> Seq.sortByDescending (fun pilot -> pilot.LatestFlightStart)
                        |> Seq.map (fun pilot -> pilot.ToDto(war))
                        |> List.ofSeq
                        |> Ok
                | Error e ->
                    return Error e
            }

        member this.StartSync(doLoop : bool) =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                let! sync =
                    match s.Sync with
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            sync.StopAfterMission <- not doLoop
                            let disp = registerOnStateChanged sync
                            let! status = sync.Init()
                            match status with
                            | Ok _ ->
                                return Ok(sync, disp)
                            | Error e ->
                                return Error e
                        }
                    | Some sync ->
                        async.Return (Ok sync)
                match sync with
                | Ok(sync, disp) ->
                    sync.Resume()
                    channel.Reply(Ok "Synchronization started")
                    return { s with Sync = Some(sync, disp) }, cache
                | Error e ->
                    channel.Reply(Error e)
                    return { s with Sync = None }, cache
            }

        member private this.RegisterSyncCleanUp(sync : GameServerSync.Sync, unregisterHandlers : System.IDisposable) =
            // Run clean-up after sync is terminated
            async {
                let! condemned = Async.AwaitEvent(sync.Terminated)
                doOnState <| fun (s, cache) -> async {
                    try
                        unregisterHandlers.Dispose()
                        condemned.Dispose()
                    with _ -> ()
                    return { s with Sync = None }, cache
                }
            }
            |> Async.Start

        member this.StopSyncAfterMission() =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                match s.Sync with
                | Some(sync, disp) ->
                    this.RegisterSyncCleanUp(sync, disp)
                    sync.StopAfterMission <- true
                    channel.Reply(Ok "Synchronization will stop after mission ends")
                | None ->
                    channel.Reply(Error "Synchronization is not active")
                return s, cache
            }

        member this.InterruptSync() =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                match s.Sync with
                | Some(sync, disp) ->
                    this.RegisterSyncCleanUp(sync, disp)
                    sync.Die("Synchronization interrupted")
                    channel.Reply(Ok "Synchronization interrupted")
                | None ->
                    channel.Reply(Error "Synchronization is not active")
                return s, cache
            }

        member this.ResetCampaign(scenario) =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                let sync =
                    match s.Sync with
                    | Some(sync, _) -> sync
                    | None ->
                        logger.Debug("Creating temporary sync")
                        // Create a temporary sync
                        GameServerSync.Sync.Create(settings)
                let! status = sync.ResetCampaign(scenario)
                logger.Debug("Campaign reset")
                // If we had to create a temporary sync, dispose it
                match s.Sync with
                | None ->
                    logger.Debug("Dispose temporary sync")
                    sync.Dispose()
                | Some _ -> ()
                // Reply and retain old sync, if any.
                match status with
                | Ok _ ->
                    channel.Reply(Ok "New campaign ready")
                | Error msg ->
                    channel.Reply(Error msg)
                return
                    { ControllerState.Default with Sync = s.Sync },
                    ControllerStateCaches.Default
            }

        member this.RebuildWorld() =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                let sync =
                    match s.Sync with
                    | Some(sync, _) -> sync
                    | None ->
                        logger.Debug("Creating temporary sync")
                        // Create a temporary sync
                        GameServerSync.Sync.Create(settings)
                let! status = sync.RebuildWorld()
                logger.Debug("World rebuild done")
                // If we had to create a temporary sync, dispose it
                match s.Sync with
                | None ->
                    logger.Debug("Dispose temporary sync")
                    sync.Dispose()
                | Some _ -> ()
                // Result
                channel.Reply(status)
                return
                    { ControllerState.Default with Sync = s.Sync },
                    ControllerStateCaches.Default
            }

        member this.ResolveError() =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                let! sync =
                    match s.Sync with
                    | Some sync -> async.Return(Ok sync)
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            let disp = registerOnStateChanged(sync)
                            let! status = sync.Init()
                            match status with
                            | Ok _ -> return Ok(sync, disp)
                            | Error e -> return Error e
                        }
                match sync with
                | Error e -> channel.Reply(Error e)
                | Ok(sync, onStateChanged) ->
                    let status = sync.ResolveError()
                    channel.Reply(status)
                    sync.Die("Stop sync after resolving error state")
                    onStateChanged.Dispose()
                    sync.Dispose()
                return { s with Sync = None }, cache
            }

        member this.FindPlayers(name : string) =
            async {
                let! war = this.WarState
                match war with
                | Ok war ->
                    let name = name.ToLowerInvariant()
                    let found =
                        war.Players
                        |> List.filter (fun player ->
                            player.Name :: player.OtherNames
                            |> List.exists (fun name2 ->
                                name2.ToLowerInvariant().Contains(name)
                            )
                        )
                        |> List.sortBy (fun player -> player.Name)
                        |> List.map (fun player -> player.ToDto(war))
                    return Ok found
                | Error e ->
                    return Error e
            }

        member this.GetPlayer(playerId) : Async<Result<Player option, string>>=
            async {
                let! war = this.WarState
                match war with
                | Ok war ->
                    let player =
                        war.Players
                        |> List.tryFind (fun player -> hashGuid player.Guid = playerId)
                        |> Option.map (fun player -> player.ToDto(war))
                    return Ok(player)
                | Error e ->
                    return Error e
            }

        member this.GetOnlinePlayers() =
            mb.PostAndAsyncReply <| fun channel (s, cache) -> async {
                match s.Sync with
                | Some(sync, _) ->
                    let! players = sync.GetOnlinePlayers()
                    channel.Reply(players)
                    return s, cache
                | None ->
                    channel.Reply(Error "Synchronization is not active")
                    return s, cache
            }

        member this.GetRegionCapacity(idx : int, region : string) =
            async {
                let! state = this.GetState(idx)
                match state with
                | Error e ->
                    return Error e
                | Ok state ->

                let region = BasicTypes.RegionId region
                if state.World.Regions.ContainsKey(region) then
                    return Ok(float (state.GetRegionBuildingCapacity(region)))
                else
                    return Error "No such region"
            }

        member this.GetAirfieldCapacity(idx : int, airfield : string) =
            async {
                let! state = this.GetState(idx)
                match state with
                | Error e ->
                    return Error e
                | Ok state ->

                let afId = BasicTypes.AirfieldId airfield
                if state.World.Airfields.ContainsKey(afId) then
                    return Ok(float (state.GetAirfieldBuildingCapacity(afId)))
                else
                    return Error "No such airfield"
            }

        member this.GetRegionSupplies(idx : int, region : string) =
            async {
                let! state = this.GetState(idx)
                match state with
                | Error e ->
                    return Error e
                | Ok state ->

                let region = BasicTypes.RegionId region
                if state.World.Regions.ContainsKey(region) then
                    let computeSupplies = state.ComputeSupplyAvailability()
                    return Ok(float(computeSupplies(region)))
                else
                    return Error "No such region"
            }

        member this.GetScenarioNames() =
            let nonScenarios =
                Set [
                    "Blocks"
                    "Blocks2"
                    "Buildings"
                    "Parking"
                    "Vehicles"
                ]
            try
                let exeDir = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
                System.IO.Directory.EnumerateFiles(exeDir, "*.mission")
                |> Seq.map (fun file -> System.IO.Path.GetFileNameWithoutExtension(file))
                |> Seq.filter (nonScenarios.Contains >> not)
                |> List.ofSeq
                |> Ok
            with _ ->
                Error "Failed to locate scenario mission files"

        member this.GetLandTransportCapacity(idx : int, regionA : string, regionB : string) =
            async {
                let! state = this.GetState(idx)
                match state with
                | Error e ->
                    return Error e
                | Ok state ->

                let regionA, regionB = BasicTypes.RegionId (min regionA regionB), BasicTypes.RegionId (max regionA regionB)
                if [regionA; regionB] |> List.forall state.World.Regions.ContainsKey then
                    let! computeRailCapacity = this.GetRailTransport(idx)
                    let! computeRoadCapacity = this.GetRoadTransport(idx)
                    match computeRailCapacity, computeRoadCapacity with
                    | Ok f1, Ok f2 ->
                        return Ok(f1(regionA, regionB) + f2(regionA, regionB))
                    | Error e, _ | _, Error e ->
                        return Error e
                else
                    return Error "No such region"
            }

        member this.GetCombatBonusesOfPilot(pilotId : string) =
            async {
                let! war = this.WarState
                match war with
                | Error e ->
                    return Error e
                | Ok war ->
                let id = Pilots.PilotId(System.Guid(pilotId))
                try
                    let pilot = war.GetPilot(id)
                    let bonuses = pilot.ComputeBonuses(war)
                    return Ok bonuses
                with
                _ -> return Error "Could not get pilot bonuses"
            }

        interface IRoutingResponse with
            member this.GetScenarioNames() = async.Return(this.GetScenarioNames())
            member this.AllPlayers() = this.FindPlayers("")
            member this.FindPlayersByName(name) = this.FindPlayers(name)
            member this.GetPlayer(hashedGuid) = this.GetPlayer(HashedGuid.Create hashedGuid)
            member this.GetWarState(idx) =
                match idx with
                | None -> this.GetStateDto()
                | Some idx -> this.GetStateDto(idx)

            member this.GetWorld() = this.GetWorldDto()
            member this.GetSimulation(idx) = this.GetSimulationDto(idx)
            member this.GetDates() = this.GetDates()
            member this.GetSyncState() = this.GetSyncState()
            member this.GetPilots(filter) = this.GetPilots(filter)
            member this.GetPilot(id) = this.GetPilot(id)
            member this.GetCombatBonusesOfPilot(id) =
                async {
                    let! bonuses = this.GetCombatBonusesOfPilot(id)
                    return bonuses |> Result.map (Array.map box)
                }
            member this.GetPlayerPilots(hashedGuid) = this.GetPlayerPilots(HashedGuid.Create hashedGuid)
            member this.GetOnlinePlayers() =
                async {
                    let! names = this.GetOnlinePlayers()
                    return
                        names
                        |> Result.map (fun names -> {| Players = names |})
                }
            member this.GetAirfieldCapacity(stateIdx, airfield) = this.GetAirfieldCapacity(stateIdx, airfield)
            member this.GetRegionCapacity(stateIdx, region) = this.GetRegionCapacity(stateIdx, region)
            member this.GetRegionSupplies(stateIdx, region) = this.GetRegionSupplies(stateIdx, region)
            member this.GetLandTransportCapacity(stateIdx, regionA, regionB) = this.GetLandTransportCapacity(stateIdx, regionA, regionB)

        interface IControllerInteraction with
            member this.Advance(n) = this.Run(n)
            member this.BackTo(idx) = this.BackTo(idx)
            member this.Forward() = this.Forward()
            member this.ResetCampaign(scenario) = this.ResetCampaign(scenario)
            member this.RebuildWorld() = this.RebuildWorld()
            member this.StartSyncLoop() = this.StartSync(true)
            member this.StartSyncOnce() = this.StartSync(false)
            member this.StopSyncAfterMission() = this.StopSyncAfterMission()
            member this.InterruptSync() = this.InterruptSync()
            member this.ResolveError() = this.ResolveError()