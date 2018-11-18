// CampaignControlApp A small standalone controller that can run SturmovikCampaign
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

open ApiImpl
open NLog
open SturmovikMission.DataProvider

type Options =
    { ConfigFile : string
      ResetScenario : string option
    }
with
    static member Create(argv : string[]) =
        let config =
            { ConfigFile = "config.yaml"
              ResetScenario = None }
        let rec parse config args =
            match args with
            | [] -> config
            | "-r" :: scenario :: rest ->
                parse { config with ResetScenario = Some scenario } rest
            | ["-r"] ->
                failwithf "Command line option -r must be followed by a scenario name"
            | path :: rest ->
                parse { config with ConfigFile = path } rest
        parse config (List.ofArray argv)

[<EntryPoint>]
let main argv = 
    // Set NLog
//    let config = NLog.Config.LoggingConfiguration()
//    NLog.LogManager.Configuration <- config

    if argv |> Array.exists (fun s -> s = "-h" || s = "--help" || s = "/?") then
        printfn """
Usage: CampaignControlApp [-r] <config.yaml>

Start or continue a campaign. <config.yaml> must point to the campaign's
configuration file. Pass -r to reset the campaign.

    Copyright (C) 2018  Johann Deneux <johann.deneux@gmail.com>
    This program comes with ABSOLUTELY NO WARRANTY;
    This is free software, and you are welcome to redistribute it
    under certain conditions.
    Full license terms available at https://www.gnu.org/licenses/gpl-3.0.en.html

"""
        2
    else
        let options = Options.Create(argv)
        let victory = Campaign.ServerControlPlugin.Support.CampaignOver Campaign.BasicTypes.CoalitionId.Axis
        let config = Campaign.Configuration.loadConfigFile options.ConfigFile
        victory.Save(config, "victoryLoopState.xml")
        use scheduler = new Scheduler(options.ConfigFile)
        scheduler.ContinueOrReset(options.ResetScenario)
        printfn "Press a key to exit."
        System.Console.ReadKey(false) |> ignore
        scheduler.Cancel()
        0