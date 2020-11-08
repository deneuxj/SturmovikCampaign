namespace Campaign.WebController

open System.IO
open FSharp.Control
open FSharp.Json

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

/// Internal state of a controller
type private ControllerState =
    {
        War : IWarStateQuery option
        DtoWorld : Dto.World option
        DtoStateCache : Map<int, Dto.WarState>
        DtoSimulationCache : Map<int, Dto.SimulationStep[]>
        Sync : GameServerSync.Sync option
    }

/// Interface between the web service and the campaign
type Controller(settings : GameServerControl.Settings) =
    let logger = NLog.LogManager.GetCurrentClassLogger()

    // Cache Player GUID hashes
    let cache = Seq.mutableDict []
    let hashGuid = SturmovikMission.Cached.cached cache hashGuid

    // A mailbox processor to serialize access to the ControllerState
    let mb = new MailboxProcessor<_>(fun mb ->
        let rec work (state : ControllerState) =
            async {
                let! req = mb.Receive()
                let! state  = req state
                return! work state
            }
        work {
            War = None
            DtoWorld = None
            DtoStateCache = Map.empty
            DtoSimulationCache = Map.empty
            Sync = None
        }
    )
    do mb.Start()

    let doOnState fn = mb.Post fn
    
    let wkPath f = Path.Combine(settings.WorkDir, f)

    do
        // Create work area if it doesn't exist
        if not(Directory.Exists settings.WorkDir) then
            try
                Directory.CreateDirectory settings.WorkDir
                |> ignore
            with
            | e -> failwithf "Directory '%s' not found, and could not be created because: %s" settings.WorkDir e.Message


    member this.GetWorldDto() =
        mb.PostAndAsyncReply <| fun channel s -> async {
            return
                match s.DtoWorld with
                | Some world ->
                    channel.Reply(Ok(world))
                    s
                | None ->
                    let path = wkPath worldFilename
                    if not(File.Exists(path)) then
                        channel.Reply(Error "Campaign not initialized")
                        s
                    else
                        try
                            let world = World.LoadFromFile(path)
                            let dto = world.ToDto()
                            channel.Reply(Ok(dto))
                            { s with DtoWorld = Some dto }
                        with e ->
                            logger.Warn(sprintf "Failed to load world: %s" e.Message)
                            logger.Warn(e)
                            channel.Reply(Error "Failed to load world")
                            s
        }

    member this.GetStateDto() =
        async {
            let idx = getCurrentIndex settings.WorkDir
            return! this.GetStateDto(idx)
        }

    /// Try to get a recorded state by its number
    member this.GetStateDto(idx : int) =
        mb.PostAndAsyncReply <| fun channel s -> async {
            return
                match s.DtoStateCache.TryGetValue idx with
                | true, x ->
                    channel.Reply(Ok x)
                    s
                | false, _ ->
                    let path = wkPath(getStateFilename idx)
                    if File.Exists path then
                        try
                            let world = World.LoadFromFile(wkPath worldFilename)
                            let state = WarState.LoadFromFile(path, world).ToDto()
                            channel.Reply(Ok state)
                            { s with DtoStateCache = s.DtoStateCache.Add(idx, state) }
                        with
                        | e ->
                            channel.Reply(Error <| sprintf "Failed to load state n.%d" idx)
                            logger.Warn e
                            s
                    else
                        channel.Reply(Error <| sprintf "No state n.%d" idx)
                        s
        }

    /// Try to get the simulation steps leading to the state identified by the given index
    member this.GetSimulationDto(idx : int) =
        mb.PostAndAsyncReply <| fun channel s -> async {
            return
                match s.DtoSimulationCache.TryGetValue idx with
                | true, x ->
                    channel.Reply(Ok x)
                    s
                | false, _ ->
                    // Optional effects file
                    let path1 = wkPath(getEffectsFilename idx)
                    // Mandatory simulation file
                    let path2 = wkPath(getSimulationFilename idx)
                    if File.Exists path2 then
                        try
                            let world = World.LoadFromFile(wkPath worldFilename)
                            let state = WarState.LoadFromFile(wkPath (getStateFilename idx), world)
                            let effects =
                                if File.Exists path1 then
                                    use reader = new StreamReader(path1)
                                    let json = reader.ReadToEnd()
                                    Json.deserializeEx JsonConfig.IL2Default json
                                else
                                    Seq.empty
                            let simulation =
                                use reader = new StreamReader(path2)
                                let json = reader.ReadToEnd()
                                Json.deserializeEx JsonConfig.IL2Default json
                            let steps =
                                Seq.append effects simulation
                                |> Seq.map (fun (description : string, command : WarStateUpdate.Commands option, results : WarStateUpdate.Results list) ->
                                    { Dto.SimulationStep.Description = description
                                      Dto.Command = command |> Option.map Array.singleton |> Option.defaultValue [||] |> Array.map (fun x -> x.ToDto(state))
                                      Dto.Results = results |> List.map (fun r -> r.ToDto(state)) |> Array.ofList
                                    }
                                )
                                |> Array.ofSeq
                            channel.Reply(Ok steps)
                            { s with DtoSimulationCache = s.DtoSimulationCache.Add(idx, steps) }
                        with
                        | e ->
                            channel.Reply(Error <| sprintf "Failed to load simulation n.%d" idx)
                            logger.Warn e
                            s
                    else
                        channel.Reply(Error <| sprintf "No simulation n.%d" idx)
                        s
        }

    member this.GetDates() =
        mb.PostAndAsyncReply <| fun channel s -> async {
            try
                let world =
                    lazy
                        World.LoadFromFile(wkPath worldFilename)
                let dates =
                    Directory.EnumerateFiles(settings.WorkDir, "*.json")
                    |> Seq.filter (fun filename -> filename.EndsWith(stateBaseFilename))
                    |> Seq.sort
                    |> Seq.map (fun path ->
                        try
                            WarState.LoadFromFile(path, world.Value).Date.ToDto()
                        with _ ->
                            { Year = -1
                              Month = -1
                              Day = -1
                              Hour = -1
                              Minute = -1
                            }
                        )
                    |> Array.ofSeq
                channel.Reply(Ok dates)
            with e ->
                channel.Reply(Error "Failed to retrieve dates")
                logger.Warn e
            return s
        }

        member this.Run(maxSteps) =
            mb.PostAndAsyncReply <| fun channel s -> async {
                let! sync =
                    match s.Sync with
                    | Some sync -> async.Return (Ok sync)
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            let! status = sync.Init()
                            match status with
                            | Ok _ -> return Ok sync
                            | Error e -> return Error e
                        }
                match sync with
                | Ok sync ->
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
                    return { s with Sync = None }
                | Error e ->
                    channel.Reply(Error e)
                    return { s with Sync = None }
            }

        member this.GetSyncState() =
            mb.PostAndAsyncReply <| fun channel s -> async {
                channel.Reply(
                    s.Sync
                    |> Option.map (fun sync ->
                        match sync.SyncState with
                        | Some(GameServerSync.PreparingMission _) -> "Preparing mission"
                        | Some GameServerSync.ResavingMission -> "Resaving missions"
                        | Some(GameServerSync.ExtractingResults _) -> "Extracting results"
                        | Some GameServerSync.AdvancingScenario -> "Advancing scenario"
                        | Some(GameServerSync.RunningMission(_, endTime)) -> sprintf "Running mission (%03d minutes left)" (int (endTime - System.DateTime.UtcNow).TotalMinutes)
                        | Some(GameServerSync.ErrorState(msg, _)) -> sprintf "Error: %s" msg
                        | None -> "Unknown")
                    |> Option.defaultValue "Not running"
                    |> Ok
                    )
                return s
            }

        member this.WarState =
            mb.PostAndAsyncReply <| fun channel s -> async {
                match s.War with
                | Some war ->
                    channel.Reply(Ok war)
                    return s
                | None ->
                    let idx = getCurrentIndex settings.WorkDir
                    let path = wkPath(getStateFilename idx)
                    try
                        let world = World.LoadFromFile(wkPath worldFilename)
                        let war = WarState.LoadFromFile(path, world)
                        channel.Reply(Ok (upcast war))
                        return { s with War = Some(upcast war) }
                    with exc ->
                        channel.Reply(Error "Failed to load current war state")
                        return s
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
                        |> List.choose (fun pilot ->
                            if filterFuns |> List.forall (fun f -> f pilot) then
                                pilot.ToDto(war) |> Some
                            else
                                None)
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
                            Ok (pilot.ToDto(war), flights)
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
                        |> Seq.choose (fun pilot ->
                            if hashGuid pilot.PlayerGuid = hashedGuid then
                                Some(pilot.ToDto(war))
                            else
                                None)
                        |> List.ofSeq
                        |> Ok
                | Error e ->
                    return Error e
            }

        member this.StartSync(doLoop : bool) =
            mb.PostAndAsyncReply <| fun channel s -> async {
                let! sync =
                    match s.Sync with
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            sync.StopAfterMission <- not doLoop
                            let! status = sync.Init()
                            match status with
                            | Ok _ ->
                                return Ok sync
                            | Error e ->
                                return Error e
                        }
                    | Some sync ->
                        async.Return (Ok sync)
                match sync with
                | Ok sync ->
                    sync.StateChanged.Add(fun war -> doOnState (fun s -> async.Return { s with War = Some war }))
                    sync.Resume()
                    channel.Reply(Ok "Synchronization started")
                    return { s with Sync = Some sync }
                | Error e ->
                    channel.Reply(Error e)
                    return { s with Sync = None }
            }

        member private this.RegisterSyncCleanUp(sync : GameServerSync.Sync) =
            // Run clean-up after sync is terminated
            async {
                let! condemned = Async.AwaitEvent(sync.Terminated)
                doOnState <| fun s -> async {
                    try
                        condemned.Dispose()
                    with _ -> ()
                    return { s with Sync = None }
                }
            }
            |> Async.Start

        member this.StopSyncAfterMission() =
            mb.PostAndAsyncReply <| fun channel s -> async {
                match s.Sync with
                | Some sync ->
                    this.RegisterSyncCleanUp(sync)
                    sync.StopAfterMission <- true
                    channel.Reply(Ok "Synchronization will stop after mission ends")
                | None ->
                    channel.Reply(Error "Synchronization is not active")
                return s
            }

        member this.InterruptSync() =
            mb.PostAndAsyncReply <| fun channel s -> async {
                match s.Sync with
                | Some sync ->
                    this.RegisterSyncCleanUp(sync)
                    sync.Die("Synchronization interrupted")
                    channel.Reply(Ok "Synchronization interrupted")
                | None ->
                    channel.Reply(Error "Synchronization is not active")
                return s
            }

        member this.ResetCampaign(scenario) =
            mb.PostAndAsyncReply <| fun channel s -> async {
                let sync =
                    match s.Sync with
                    | Some sync -> sync
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
                return s
            }

        member this.ResolveError() =
            mb.PostAndAsyncReply <| fun channel s -> async {
                let! sync =
                    match s.Sync with
                    | Some sync -> async.Return(Ok sync)
                    | None ->
                        async {
                            let sync = GameServerSync.Sync.Create(settings)
                            let! status = sync.Init()
                            match status with
                            | Ok _ -> return Ok sync
                            | Error e -> return Error e
                        }
                match sync with
                | Error e -> channel.Reply(Error e)
                | Ok sync ->
                    let status = sync.ResolveError()
                    channel.Reply(status)
                    sync.Die("Stop sync after resolving error state")
                    sync.Dispose()
                return { s with Sync = None }
            }

        member this.UpdatePlayerBan(playerId : HashedGuid, duration : System.TimeSpan option) =
            async {
                let! war = this.WarState
                match war with
                | Ok war ->
                    return!
                        mb.PostAndAsyncReply <| fun channel s -> async {
                            let! sync =
                                match s.Sync with
                                | Some sync -> async.Return(Ok sync)
                                | None ->
                                    async {
                                        let sync = GameServerSync.Sync.Create(settings)
                                        let! status = sync.Init()
                                        match status with
                                        | Ok _ -> return Ok sync
                                        | Error e -> return Error e
                                    }

                            let player =
                                war.Players
                                |> List.tryFind (fun player -> hashGuid player.Guid = playerId)

                            let! player =
                                match sync, player with
                                | Ok sync, Some player ->
                                    sync.ModifyPlayer(player.Guid,
                                        fun player ->
                                            let banStatus =
                                                match duration with
                                                | Some duration ->
                                                    Pilots.BanStatus.Banned (System.DateTime.UtcNow, duration)
                                                | None ->
                                                    Pilots.BanStatus.Clear
                                            { player with BanStatus = banStatus }
                                    )
                                | Error e, Some _ ->
                                    async.Return(Error e)
                                | _, None ->
                                    async.Return(Error "Player not found")

                            match player with
                            | Ok player ->
                                channel.Reply(Ok(player.ToDto(war)))
                            | Error msg ->
                                channel.Reply(Error msg)
                            return s
                        }
                | Error e ->
                    return Error e
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
                            player.OtherNames.Add(player.Name)
                            |> Set.exists (fun name2 ->
                                name2.ToLowerInvariant().Contains(name)
                            )
                        )
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

        interface IRoutingResponse with
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
            member this.GetPlayerPilots(hashedGuid) = this.GetPlayerPilots(HashedGuid.Create hashedGuid)

        interface IControllerInteraction with
            member this.BanPlayer(hashedGuid, duration) = this.UpdatePlayerBan(HashedGuid.Create hashedGuid, Some duration)
            member this.ClearBan(hashedGuid) = this.UpdatePlayerBan(HashedGuid.Create hashedGuid, None)
            member this.Advance() = this.Run(1)
            member this.Run() = this.Run(15)
            member this.ResetCampaign(scenario) = this.ResetCampaign(scenario)
            member this.StartSyncLoop() = this.StartSync(true)
            member this.StartSyncOnce() = this.StartSync(false)
            member this.StopSyncAfterMission() = this.StopSyncAfterMission()
            member this.InterruptSync() = this.InterruptSync()
            member this.ResolveError() = this.ResolveError()