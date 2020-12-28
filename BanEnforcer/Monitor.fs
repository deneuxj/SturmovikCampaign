namespace Coconutside.BanEnforcer

open System
open NLog

open Config
open Players

type Monitor(config : Config) =
    let logger = LogManager.GetCurrentClassLogger()

    let queues =
        config.DServerRCons
        |> List.map (fun x ->
            new RConClient.ClientMessageQueue(x.Url, x.Port, x.User, x.Password))

    let token = Threading.CancellationToken()

    let rec monitor (db : PlayerDb) =
        async {
            if not token.IsCancellationRequested then
                do! Async.Sleep(config.GetCheckInterval() * 1000)
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
                    do! db.Save(config.GetPlayerDbPath())
                with exc ->
                    logger.Warn(sprintf "Failed to write refreshed ban db: '%s'" exc.Message)
                    logger.Debug(exc)
                return! monitor db
        }

    let startMonitor =
        async {
            let! db = PlayerDb.Load(config.GetPlayerDbPath())
            return! monitor db
        }

    do Async.Start(startMonitor)

    member this.Add(guid : string, duration : TimeSpan) = failwith "TODO"

    member this.Clear(guid : string) = failwith "TODO"

    member this.Shutdown() =
        for queue in queues do
            queue.Dispose()

    member this.Dispose() = this.Shutdown()

    interface System.IDisposable with
        member this.Dispose() = this.Dispose()
