// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp
namespace Coconutside.BanEnforcer

open System

open Coconutside.BanEnforcer.Config
open Coconutside.BanEnforcer.Players

type CommandLineSettings =
    {
        ConfigPath : string
        SetPassword : string option
        PrintHelp : bool
    }
with
    static member Default =
        {
            ConfigPath = IO.Path.Combine(
                            IO.Path.GetDirectoryName(typeof<CommandLineSettings>.Assembly.Location),
                            "config.json")
            SetPassword = None
            PrintHelp = false
        }

    static member FromEnv(argv : string list) =
        let rec work (settings : CommandLineSettings, argv : string list) =
            match argv with
            | [] -> settings
            | ("/Config:" | "-c") :: path :: argv ->
                let settings =
                    { settings with
                        ConfigPath = path
                    }
                work(settings, argv)
            | "/Password:" :: password :: argv ->
                { settings with
                    SetPassword = Some password
                }

            | ("/?" | "-h" | "--help") :: _ ->
                { CommandLineSettings.Default with
                    PrintHelp = true
                }
            | invalid :: _ ->
                eprintfn "Unrecognized switch '%s', or missing follow-up argument" invalid
                { CommandLineSettings.Default with
                    PrintHelp = true
                }
        work(CommandLineSettings.Default, argv)

    static member PrintHelpText() =
        """
Usage: BanEnforcer /Config: <path to config.json> [/Password: <password>]
        """
        |> eprintfn "%s\n"


module Main =
    [<EntryPoint>]
    let main argv =
        let settings = CommandLineSettings.FromEnv(List.ofArray argv)
        if settings.PrintHelp then
            CommandLineSettings.PrintHelpText()
            1
        else
            let config =
                Config.Load(settings.ConfigPath)
            use monitor = new Monitor(config)

            0