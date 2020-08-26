﻿open System
open System.Threading
open Suave
open Campaign.WebController
open Campaign.WebController.Routes

let private logger = NLog.LogManager.GetCurrentClassLogger()

[<EntryPoint>]
let main argv =
    let allowAdminPasswordChange =
        argv
        |> Array.exists (function "/Allow:AdminPasswordChange" -> true | _ -> false)

    let setAdminPasswordChange =
        argv
        |> Array.tryPick (fun s ->
            if s.StartsWith("/SetAdminPassword:") then
                match s.Split(':', 2) with
                | [| _; password |] -> Some password
                | _ ->
                    logger.Warn("Invalid /SetAdminPassword: switch")
                    None
            else
                None
        )

    let myConfig =
        let filename = "webcontroller.cfg"
        try
            logger.Info("Preparing to load config file " + filename)
            let config = Config.Config.LoadFromFile filename
            logger.Info("Config file loaded")
            config
        with exc ->
            logger.Warn("Failed to load config file, using defaults")
            logger.Warn(exc)
            let config =
                Config.Config.Default
            if IO.File.Exists(filename) then
                try
                    IO.File.Copy(filename, filename + ".bak")
                    logger.Info("Successfully backed up old config file")
                    config.Save(filename)
                    logger.Info("Successfully saved new file with default values. Please exit, edit config and restart program.")
                with exc ->
                    logger.Warn("Failed to back up old config and save new one")
                    logger.Warn(exc)
            else
                config.Save(filename)
                logger.Info("Created config file with default values. Please exit, edit config and restart program.")
            config

    // Suave setup
    let cts = new CancellationTokenSource()
    let conf =
        { defaultConfig with
            bindings =
                myConfig.Listen
                |> List.map (fun x -> HttpBinding.createSimple Protocol.HTTP x.IP (int x.Port))
            cancellationToken = cts.Token
            homeFolder = Some (IO.Path.GetFullPath(myConfig.SitePath)) }

    // Campaign settings
    let campaignSettingsPath = IO.Path.Combine(myConfig.CampaignPath, "campaign.cfg")
    let settings =
        if IO.File.Exists(campaignSettingsPath) then
            logger.Info(sprintf "Will load settings from %s" campaignSettingsPath)
            Campaign.GameServerControl.IO.loadFromFile campaignSettingsPath
        else
            logger.Info(sprintf "Will create default settings in %s" campaignSettingsPath)
            let defaultSettings = Campaign.GameServerControl.IO.createDefaultFile campaignSettingsPath
            printfn "Created default config in %s. Please edit as needed." campaignSettingsPath
            printfn "Running with default settings, but they are unlikely to work."
            defaultSettings

    let passwords = Campaign.Passwords.PasswordsManager()
    if allowAdminPasswordChange then
        logger.Warn("Unprotected setting of user passwords is enabled")

    // Set password from commandline
    setAdminPasswordChange
    |> Option.iter(fun password ->
        passwords.SetPassword("admin", password)
        |> Result.mapError (fun msg -> logger.Warn(msg))
        |> ignore)
    // Create route handler
    let controller = Controller(settings)
    let routes = mkRoutes(passwords, allowAdminPasswordChange, controller, controller)
    // Start
    let listening, server = startWebServerAsync conf routes
    Async.StartImmediate(server, cts.Token)
    printfn "Press a key to exit"
    Console.ReadKey(true) |> ignore
    cts.Cancel()
    0 // return an integer exit code
