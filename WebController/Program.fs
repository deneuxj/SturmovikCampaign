open System
open System.Threading
open Suave
open Campaign.WebController
open Campaign.WebController.Routes

let private logger = NLog.LogManager.GetCurrentClassLogger()

[<EntryPoint>]
let main argv =
    let myConfig =
        let filename = "webcontroller.config"
        try
            Config.Config.LoadFromFile filename
        with exc ->
            logger.Warn("Failed to load config file, using defaults")
            logger.Warn(exc)
            let config =
                Config.Config.Default
            config.Save(filename)
            config

    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token; homeFolder = Some (IO.Path.GetFullPath(myConfig.SitePath)) }
    let campaignSettingsPath = IO.Path.Combine(Campaign.GameServerSync.Settings.DefaultWorkDir, "..", "campaign.cfg")
    let settings =
        if IO.File.Exists(campaignSettingsPath) then
            logger.Info(sprintf "Will load settings from %s" campaignSettingsPath)
            Campaign.GameServerSync.IO.loadFromFile campaignSettingsPath
        else
            logger.Info(sprintf "Will create default settings in %s" campaignSettingsPath)
            let defaultSettings = Campaign.GameServerSync.IO.createDefaultFile campaignSettingsPath
            printfn "Created default config in %s. Please edit as needed." campaignSettingsPath
            printfn "Running with default settings, but they are unlikely to work."
            defaultSettings

    let controller = Controller(settings)
    let routes = mkRoutes(controller, controller)
    let listening, server = startWebServerAsync conf routes
    Async.StartImmediate(server, cts.Token)
    printfn "Press a key to exit"
    Console.ReadKey(true) |> ignore
    cts.Cancel()
    0 // return an integer exit code
