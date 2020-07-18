open System
open System.Threading
open Suave
open Campaign.WebController
open Campaign.WebController.Routes

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let campaignSettingsPath = IO.Path.Combine(Campaign.GameServerSync.Settings.DefaultWorkDir, "..", "campaign.cfg")
    let settings =
        if IO.File.Exists(campaignSettingsPath) then
            Campaign.GameServerSync.IO.loadFromFile campaignSettingsPath
        else
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
