module Coconutside.BanEnforcer.Config

open System
open FSharp.Json
open System.Security.Cryptography

[<Literal>]
let salt = "DFSRT"

type Config =
    {
        DServerRCons : {| Url : string; Port : int; User : string; Password : string |} list
        PlayerDbPath : string option
        CheckInterval : int option
        HtmlPath : string option
        PasswordHash : string option
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

    /// Set the password hash from the password
    member this.SetPassword(password : string) =
        use algo = HashAlgorithm.Create("SHA256")
        let hashed = algo.ComputeHash(Text.Encoding.UTF8.GetBytes(salt + password))
        let hashedStr = Convert.ToBase64String hashed
        { this with
            PasswordHash = Some hashedStr
        }

    /// Check if the given password matches the one in the config
    member this.CheckPassword(password : string) =
        match this.PasswordHash with
        | None ->
            false
        | Some hashedStr ->
            use algo = HashAlgorithm.Create("SHA256")
            let hashed = algo.ComputeHash(Text.Encoding.UTF8.GetBytes(salt + password))
            let hashedStr2 = Convert.ToBase64String hashed
            hashedStr = hashedStr2

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
                PasswordHash = None
            }

    /// Save to Json file
    member this.Save(path : string) =
        let json = Json.serialize(this)
        IO.File.WriteAllText(path, json)