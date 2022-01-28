module LoadWatcher.Main

open FSharp.Control

open RConClient
open Campaign.WatchLogs
open Campaign.GameLogEvents
open Util.StringPatterns

type Options =
    {
        GameLogsPath : string
        RconHostname : string
        RconPort : int
        RconUsername : string
        RconPassword : string
        ServerInput : string option
        CheckPeriod : int
        OverloadThreshold : float32
        OverloadVerboseWindow : System.TimeSpan
    }
with
    static member Default =
        {
            GameLogsPath = System.IO.Path.Combine("..", "data", "logs")
            RconHostname = "localhost"
            RconPort = 8990
            RconUsername = "admin"
            RconPassword = "pwd"
            ServerInput = None
            CheckPeriod = 15000
            OverloadThreshold = 49.0f
            OverloadVerboseWindow = System.TimeSpan.FromMinutes(2)
        }


let rec parseOptions (args : string list, options : Options) =
    match args with
    | [] ->
        Ok options
    | "--logs" :: path :: args ->
        parseOptions (args, { options with GameLogsPath = path })
    | "--port" :: AsInt32 port :: args ->
        parseOptions (args, { options with RconPort = port })
    | "--auth" :: user :: pwd :: args ->
        parseOptions (args, { options with RconUsername = user; RconPassword = pwd })
    | ("--help" | "-h" | "/h") :: args ->
        printfn "Usage: LoadWatcher --logs <path to text logs> --port <RCon port> --auth <RCon user> <RCon password> [--input <server input>]"
        parseOptions (args, options)
    | other :: _ ->
        Error $"Unsupported switch {other}"


let checkLoad(options : Options, aborted, onOverload) =
    async {
        use client = new Client(options.RconHostname, options.RconPort, options.RconUsername, options.RconPassword)
        let! _ = client.Auth()
        while not (aborted()) && client.Connected do
            let! _ = client.ResetSPS()
            do! Async.Sleep(options.CheckPeriod)
            let! sps = client.GetSPS()
            if sps.Average < options.OverloadThreshold then
                match options.ServerInput with
                | Some input ->
                    let! _ = client.ServerInput(input)
                    ()
                | None ->
                    ()
                onOverload(sps.Average)
    }


let extractLogs(options : Options, onBinding, onKilled, onTaken, onEnded) =
    let logWatcher = watchLogs options.GameLogsPath "missionReport*" (System.TimeSpan.FromMinutes 2)
    async {
        for file in logWatcher do
            match file with
            | NewLogFile file ->
                for line in System.IO.File.ReadAllLines(file) do
                    match line with
                    | ObjectEvent(ts, ObjectBound binding) ->
                        onBinding(ts, binding)
                    | ObjectEvent(ts, ObjectKilled killed) ->
                        onKilled(ts, killed.TargetId)
                    | PlayerEvent(ts, PlayerTakesObject taken) ->
                        onTaken(ts, taken)
                    | PlayerEvent(ts, PlayerEndsMission ended) ->
                        onEnded(ts, ended.PilotId)
                    | _ ->
                        ()
            | _ ->
                ()
    }


type MailboxMessage =
    | Overloaded of float32
    | BindingEvent of System.TimeSpan * Binding
    | KilledEvent of System.TimeSpan * TargetId:int
    | TakenEvent of System.TimeSpan * ObjectTaken
    | EndedEvent of System.TimeSpan * PilotId:int
    | Exit


let printOverload(sps) =
    printfn "SPS: %3.0f" sps


let printStats(players, counters) =
    printfn "  spawned players: %d" (Map.count players)
    for key, count in Map.toSeq counters do
        printfn "  %s: %d" key count


let mkAgent (options : Options) =
    let reIncDecNul = System.Text.RegularExpressions.Regex("(INC|DEC|NUL)_(.*)")
    new MailboxProcessor<MailboxMessage>(
    fun mb ->
        let rec work(lastOverload, bound, players, counters) =
            async {
                let! msg = mb.TryReceive(15000)
                match msg with
                | None ->
                    match lastOverload with
                    | Some time ->
                        let passed = System.DateTime.UtcNow - time
                        if passed < options.OverloadVerboseWindow then
                            printStats(players, counters)
                            return! work(lastOverload, bound, players, counters)
                        else
                            return! work(None, bound, players, counters)
                    | None ->
                        return! work(None, bound, players, counters)
                | Some msg ->

                let lastOverload =
                    match lastOverload with
                    | Some time ->
                        let passed = System.DateTime.UtcNow - time
                        if passed < options.OverloadVerboseWindow then
                            Some time
                        else
                            printStats(players, counters)
                            None
                    | None ->
                        None

                match msg with
                | Overloaded sps ->
                    printOverload(sps)
                    printStats(players, counters)
                    return! work(Some System.DateTime.UtcNow, bound, players, counters)
                | BindingEvent(ts, binding) ->
                    return! work(lastOverload, Map.add binding.Id binding bound, players, counters)
                | KilledEvent(ts, targetId) ->
                    match Map.tryFind targetId bound with
                    | Some binding ->
                        let matches = reIncDecNul.Match(binding.Name)
                        if matches.Success then
                            let key = string matches.Groups[2]
                            let oldVal = Map.tryFind key counters |> Option.defaultValue 0
                            match string matches.Groups[1] with
                            | "INC" ->
                                if Option.isSome lastOverload then
                                    printStats(players, counters)
                                return! work(lastOverload, bound, players, Map.add key (oldVal + 1) counters)
                            | "DEC" ->
                                return! work(lastOverload, bound, players, Map.add key (oldVal - 1) counters)
                            | "NUL" ->
                                return! work(lastOverload, bound, players, Map.add key 0 counters)
                            | _ ->
                                return! work(lastOverload, bound, players, counters)
                        else
                            return! work(lastOverload, bound, players, counters)
                    | None ->
                        return! work(lastOverload, bound, players, counters)
                | TakenEvent(ts, taken) ->
                    if Option.isSome lastOverload then
                        printStats(players, counters)
                    return! work(lastOverload, bound, Map.add taken.PilotId taken players, counters)
                | EndedEvent(ts, pilotId) ->
                    return! work(lastOverload, bound, Map.remove pilotId players, counters)
                | Exit ->
                    return()
            }
        work(None, Map.empty, Map.empty, Map.empty)
)


let main(args : string list) =
    let options = parseOptions (args, Options.Default)
    match options with
    | Error s ->
        eprintfn "%s" s
        1
    | Ok options ->
        // Mutable flag to signal abortion
        let mutable aborted = false
        // Start agent
        use agent = mkAgent options
        agent.Start()
        // Load checker and log extractor post messages to the agent
        async {
            let! loadChecker =
                Async.StartChild(
                    checkLoad(options, (fun () -> aborted), fun sps -> agent.Post(Overloaded sps))
                )
            let! logExtractor =
                Async.StartChild(
                    extractLogs(
                        options,
                        (fun (ts, binding) -> agent.Post(BindingEvent(ts, binding))),
                        (fun (ts, killed) -> agent.Post(KilledEvent(ts, killed))),
                        (fun (ts, taken) -> agent.Post(TakenEvent(ts, taken))),
                        (fun (ts, ended) -> agent.Post(EndedEvent(ts, ended)))
                    )
                )
            let! _ = Async.Parallel [| loadChecker; logExtractor |]
            return ()
        }
        |> Async.StartImmediate
        // Wait for key
        ignore(System.Console.ReadKey(false))
        // Abort load checker
        aborted <- true
        // Stop agent
        agent.Post(Exit)
        // Give a chance to end nicely
        Async.RunSynchronously(Async.Sleep(2000))
        0


main (List.ofArray(System.Environment.GetCommandLineArgs()))
|> System.Environment.Exit
