#I @"..\bin\Debug"

#r "Campaign.dll"
#r "ploggy"
#r "FsPickler"
#r "NLog"
#r "FSharp.Control.AsyncSeq"

open System
open System.IO
open FSharp.Control
open MBrace.FsPickler
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.NewWorldState
open Campaign.BasicTypes
open Campaign

NLog.LogManager.Configuration <- NLog.Config.LoggingConfiguration()

let serializer = FsPickler.CreateXmlSerializer(indent = true)

let getPath filename =
    let docs = Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments)
    IO.Path.Combine(docs, "FromServer", "Expert", filename)

let date = "1941-12-27_12-00-00"

let getDatedPath filename =
    getPath (sprintf "%s_%s.xml" filename date)

let deserialize<'T> path =
    use file = File.OpenText(path)
    serializer.Deserialize<'T>(file)


let world = deserialize<World>(getPath "world.xml")
let state = deserialize<WorldState>(getDatedPath "state")
let hangars = PlayerHangar.tryLoadHangars(getDatedPath "hangars") |> Option.map (PlayerHangar.guidToStrings) |> Option.defaultValue Map.empty
let results = deserialize<MissionResults>(getDatedPath "results")

let limits : PlaneChecksContext.Limits =
    { InitialCash = 0.0f<E>
      MaxCash = 10000.0f<E>
      SpawnsAreRestricted = true
      MaxBonusLuxurySpawns = 2.0f
      LuxurySpawns = 2.0f
      FreshSpawns = Map.ofList [
        PlaneModel.PlaneType.Fighter, 5.0f
        PlaneModel.PlaneType.Attacker, 5.0f
        PlaneModel.PlaneType.Bomber, 3.0f
        PlaneModel.PlaneType.Transport, 3.0f ]
    }

let entries =
    results.Entries
    |> List.map (ploggy.LogEntry.Parse >> WatchLogs.Fresh)
    |> AsyncSeq.ofSeq

let stuff =
    PlaneAvailabilityChecks.checkPlaneAvailability 3.0f<H> limits world state hangars entries
    |> AsyncSeq.toBlockingSeq
    |> Array.ofSeq

let hangars2 =
    stuff
    |> Seq.choose (function PlaneChecksContext.Status(x, _) -> Some x | _ -> None)
    |> Seq.last

hangars2
|> Map.toSeq
|> Seq.filter (fun (_, h) -> h.PlayerName = "bushmantw")
