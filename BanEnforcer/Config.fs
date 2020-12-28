module Coconutside.BanEnforcer.Config

open FSharp.Json

type Config =
    {
        DServerRCons : {| Url : string; User : string; Password : string |} list
        PlayerDbPath : string option
        CheckInterval : int option
        HtmlPath : string option
    }
with
    /// The file that contains the player bans
    member this.GetPlayerDbPath() =
        let path =
            this.PlayerDbPath
            |> Option.defaultValue "playerdb.json"
        if path.EndsWith "/" || path.EndsWith "\\" then
            path + "playerdb.json"
        else
            path

    /// The check interval, in seconds
    member this.GetCheckInterval() =
        this.CheckInterval
        |> Option.defaultValue 15

