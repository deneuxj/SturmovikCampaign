open ApiImpl
open NLog

type Options =
    { ConfigFile : string
      DoReset : bool
    }
with
    static member Create(argv : string[]) =
        let config =
            { ConfigFile = "config.yaml"
              DoReset = false }
        argv
        |> Array.fold (fun config arg ->
            match arg with
            | "-r" | "--reset" -> { config with DoReset = true }
            | path -> { config with ConfigFile = path }) config

[<EntryPoint>]
let main argv = 
    // Set NLog
    let config = NLog.Config.LoggingConfiguration()
    NLog.LogManager.Configuration <- config

    if argv |> Array.exists (fun s -> s = "-h" || s = "--help" || s = "/?") then
        printfn """
Usage: CampaignControlApp [-r] <config.yaml>

Start or continue a campaign. <config.yaml> must point to the campaign's
configuration file. Pass -r to reset the campaign.
"""
        2
    else
        let options = Options.Create(argv)
        use scheduler = new Scheduler(options.ConfigFile)
        scheduler.ContinueOrReset(options.DoReset)
        printfn "Press a key to exit."
        System.Console.ReadKey(false) |> ignore
        scheduler.Cancel()
        0