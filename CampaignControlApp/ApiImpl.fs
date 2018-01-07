module ApiImpl

open System.IO
open SturmovikMission.DataProvider.Ast
open CampaignServerControl.Api
open Campaign.BasicTypes
open Campaign.Configuration
open SdsFile

type Logging() =
    interface LoggingApi with
        member this.LogError(msg) = eprintfn "%s" msg
        member this.LogInfo(msg) = printfn "%s" msg

type NamedPlayer =
    { PlayerName : string
      PlayerId : int
    }
with
    interface PlayerId with
        member this.GetName() = this.PlayerName

type Team =
    { Coalition : CoalitionId
    }
with
    interface TeamId with
        member this.GetName() = string this.Coalition

type ServerControl(config : Configuration) =
    let axis = { Coalition = Axis }
    let allies = { Coalition = Allies }
    let rcon =
        let sdsConfigFile = Path.Combine(config.ServerDataDir, config.ServerSdsFile)
        let d = parseSds sdsConfigFile
        let bindString = Option.bind (function Value.String s -> Some s | _ -> None)
        let hostname =
            d.TryFind "RconIP"
            |> bindString
            |> Option.defaultValue "127.0.0.1"
        let port =
            d.TryFind "RconPort"
            |> Option.bind (function Value.Integer x -> Some x | _ -> None)
            |> Option.defaultValue 8991
        let login =
            d.TryFind "RconLogin"
            |> bindString
            |> Option.defaultValue "login"
        let password =
            d.TryFind "RconPassword"
            |> bindString
            |> Option.defaultValue "password"
        new RConClient.ClientMessageQueue(hostname, port, login, password)
    do ()

    interface System.IDisposable with
        member this.Dispose(): unit = 
            rcon.Dispose()

    interface ServerControlApi with
        member this.GetAlliesTeam(): TeamId = 
            upcast allies

        member this.GetAxisTeam(): TeamId = 
            upcast axis

        member this.GetPlayerList: Async<PlayerId list> = 
            async {
                let! players = rcon.Run(lazy rcon.Client.GetPlayerList())
                match players with
                | Some(Some players) ->
                    return
                        players
                        |> Seq.map (fun p -> { PlayerName = p.Name; PlayerId = p.ClientId } :> PlayerId)
                        |> List.ofSeq
                | None | Some None ->
                    return []
            }

        member this.MessageAll(msgs: string list): Async<unit> = 
            async {
                for msg in msgs do
                    let! _ = rcon.Run(lazy rcon.Client.MessageAll msg)
                    do! Async.Sleep 1000
            }

        member this.MessagePlayer(player: PlayerId, msgs: string list): Async<unit> = 
            async {
                let playerId =
                    match player with
                    | :? NamedPlayer as player -> Some player.PlayerId
                    | _ -> None
                match playerId with
                | Some pid ->
                    for msg in msgs do
                        let! _ = rcon.Run(lazy rcon.Client.MessagePlayer(pid, msg))
                        do! Async.Sleep 1000
                | None ->
                    ()
            }

        member this.MessageTeam(team: TeamId, msgs: string list): Async<unit> = 
            async {
                let teamId =
                    match team with
                    | :? Team as team ->
                        match team.Coalition with
                        | Axis -> 2
                        | Allies -> 1
                        |> Some
                    | _ -> None
                match teamId with
                | Some tid ->
                    for msg in msgs do
                        let! _ = rcon.Run(lazy rcon.Client.MessageTeam(tid, msg))
                        do! Async.Sleep 1000
                | None ->
                    ()
            }

        member this.SkipMission: Async<unit> = 
            async {
                let! _ = rcon.Run(lazy rcon.Client.ServerInput("ReqMissionEnd"))
                do! Async.Sleep 1000
            }

let rec insertTask ((date, _, _) as task) tasks =
    match tasks with
    | [] -> [SomeTask task]
    | SomeTask (date2, _, _) as task2 :: rest ->
        if date2 <= date then
            task2 :: insertTask task rest
        else
            SomeTask task :: tasks
    | NoTask :: rest ->
        insertTask task rest

type Scheduler(configFile : string) =
    let config = loadConfigFile configFile
    let logging = Logging()
    let serverCtl = new ServerControl(config)
    let rec scheduler scheduled =
        async {
            match scheduled with
            | [] ->
                printfn "No more tasks to schedule"
            | task :: rest ->
                match task with
                | NoTask ->
                    return! scheduler rest
                | SomeTask(date, task, desc) ->
                    let now = System.DateTime.UtcNow
                    if now >= date then
                        printfn "Execute '%s'" desc
                        let! next = task
                        match next with
                        | NoTask ->
                            return! scheduler rest
                        | SomeTask(x, y, z) ->
                            return! scheduler (insertTask (x, y, z) rest)
                    else
                        do! Async.Sleep 5000
                        return! scheduler scheduled
        }
    let cancel = new System.Threading.CancellationTokenSource()

    member this.ContinueOrReset(doReset) =
        let campaign = Campaign.ServerControlPlugin.Plugin() :> CampaignServerApi
        campaign.Init { Logging = logging; ServerControl = serverCtl }
        let task =
            if doReset then
                campaign.Reset(configFile)
            else
                campaign.StartOrResume(configFile)
        match task with
        | Choice1Of2 task ->
            Async.Start(scheduler [task], cancel.Token)
        | Choice2Of2 msg ->
            eprintfn "%s" msg

    member this.Cancel() =
        cancel.Cancel()

    interface System.IDisposable with
        member this.Dispose(): unit = 
            (serverCtl :> System.IDisposable).Dispose()
            cancel.Dispose()