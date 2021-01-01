namespace Coconutside.BanEnforcer

open System
open NLog

open Config
open Players

/// Type of requests passed to the mailbox processor of the Monitor
type MonitorCommand =
    | SetDatabase of PlayerDb
    | GetDatabase of AsyncReplyChannel<PlayerDb option>
    | RefreshAll of AsyncReplyChannel<unit>
    | AddBan of {| Guid : string; Duration : TimeSpan |}
    | ClearBan of {| Guid : string |}

/// Handle interactions with the player ban db and the DServers
type Monitor(config : Config) =
    let logger = LogManager.GetCurrentClassLogger()

    let queues =
        config.DServerRCons
        |> List.map (fun x ->
            new RConClient.ClientMessageQueue(x.Url, x.Port, x.User, x.Password))

    let token = new Threading.CancellationTokenSource()

    let rec processs (db : PlayerDb option) (mb : MailboxProcessor<MonitorCommand>) =
        async {
            match! mb.Receive() with
            | SetDatabase db ->
                return! processs (Some db) mb

            | GetDatabase r ->
                r.Reply(db)
                return! processs db mb

            | RefreshAll r ->
                match db with
                | Some db ->
                    do!
                        queues
                        |> Seq.map (fun queue ->
                            async {
                                let! players = queue.Run(lazy queue.Client.GetPlayerList())
                                match players with
                                | Some(Some players) ->
                                    let guids =
                                        players
                                        |> Seq.map (fun player -> player.PlayerId)
                                        |> List.ofSeq
                                    let toKick =
                                        db.MatchBans(guids)
                                        |> Set
                                    for player in players do
                                        if toKick.Contains(player.PlayerId) then
                                            let! status = queue.Run(lazy queue.Client.KickPlayer(player.ClientId))
                                            let status = defaultArg status "<no status>"
                                            logger.Info(sprintf "Kicked '%s': %s" player.Name status)
                                | _ ->
                                    logger.Warn(sprintf "Failed to retrieve player list from %s" (queue.Client.Client.RemoteEndPoint.ToString()))
                            })
                        |> Async.Parallel
                        |> Async.Ignore
                    let db = db.Refresh(DateTime.UtcNow)
                    try
                        let path = config.GetPlayerDbPath()
                        do! db.Save(path)
                        logger.Info(sprintf "Saved refreshed ban db at %s" path)
                    with exc ->
                        logger.Warn(sprintf "Failed to write refreshed ban db: '%s'" exc.Message)
                        logger.Debug(exc)
                    r.Reply()
                    return! processs (Some db) mb
                | None ->
                    logger.Warn("Cannot refresh because: No player db")

            | AddBan x ->
                let db = defaultArg db (PlayerDb.Default)
                let db = db.Add(x.Guid, x.Duration, DateTime.UtcNow)
                return! processs (Some db) mb

            | ClearBan x ->
                let db = defaultArg db (PlayerDb.Default)
                let db = db.Unban(x.Guid)
                return! processs (Some db) mb
        }

    let mb = new MailboxProcessor<MonitorCommand>(processs None, token.Token)

    let startMonitor =
        async {
            let! db = PlayerDb.Load(config.GetPlayerDbPath())
            mb.Start()
            mb.Post(SetDatabase db)
        }

    do Async.Start(startMonitor)

    // Start loop that requests refreshes at regular intervals
    do Async.Start(
        async {
            while not token.IsCancellationRequested do
                do! Async.Sleep(config.GetCheckInterval())
                do! mb.PostAndAsyncReply RefreshAll
        }
    )

    /// Add or update a ban
    member this.Add(guid : string, duration : TimeSpan) =
        mb.Post(AddBan{| Guid = guid; Duration = duration |})

    /// Clear a ban
    member this.Clear(guid : string) =
        mb.Post(ClearBan {| Guid = guid |})

    /// Try to get a banned player by their GUID
    member this.TryGetPlayer(guid : string) =
        async {
            let! db = mb.PostAndAsyncReply GetDatabase
            match db with
            | Some db ->
                let player =
                    db.Players
                    |> List.tryFind (fun player -> player.Guid = guid)
                return player
            | None ->
                return None
        }

    /// Find banned players by name
    member this.Find(name : string) =
        async {
            let! db = mb.PostAndAsyncReply GetDatabase
            match db with
            | Some db ->
                return db.FindPlayers name
            | None ->
                return []
        }

    /// Stop DServer clients and stop processing requests
    member this.Shutdown() =
        token.Cancel()
        for queue in queues do
            queue.Dispose()
        token.Dispose()

    /// Shut down
    member this.Dispose() = this.Shutdown()

    interface System.IDisposable with
        member this.Dispose() = this.Dispose()
