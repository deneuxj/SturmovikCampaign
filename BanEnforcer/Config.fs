module Coconutside.BanEnforcer.Config

open System
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

    /// Load from Json file, or return default if file does not exist
    static member Load(path : string) =
        if IO.File.Exists(path) then
            let json = IO.File.ReadAllText(path)
            Json.deserialize<Config>(json)
        else
            {
                DServerRCons = []
                PlayerDbPath = None
                CheckInterval = None
                HtmlPath = None
            }

    /// Save to Json file
    member this.Save(path : string) =
        let json = Json.serialize(this)
        IO.File.WriteAllText(path, json)