module Campaign.WebController.Config

open FSharp.Json
open System

type Config = {
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
            SitePath = "."
        }
