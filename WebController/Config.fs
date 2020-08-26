module Campaign.WebController.Config

open FSharp.Json
open System

type Config = {
    /// IPs and ports to listen on
    Listen : {| IP : string; Port : uint16 |} list
    /// Location of campaign data dir
    CampaignPath : string
    /// Path to the root of the web site, containing the html/ and js/ subdirectories
    SitePath : string
}
with
    static member LoadFromFile(path : string) =
        let json = IO.File.ReadAllText(path)
        Json.deserialize<Config>(json)

    member this.Save(path : string) =
        let json = Json.serialize(this)
        use file = IO.File.CreateText(path)
        file.Write(json)

    static member Default =
        {
            SitePath = @"..\Web"
            Listen = [ {| IP = "127.0.0.1"; Port = 8080us |}]
            CampaignPath = IO.Path.GetDirectoryName(Campaign.GameServerControl.Settings.DefaultWorkDir)
        }
