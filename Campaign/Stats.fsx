#I @"bin\Debug"

#r "Campaign.dll"
#r "ploggy"
#r "FsPickler"

#load "Configuration.fsx" 

open Configuration
open System
open MBrace.FsPickler
open Campaign.NewWorldState
open Campaign.ResultExtraction
open Campaign.PlaneModel
open Campaign.BasicTypes

let serializer = FsPickler.CreateXmlSerializer(indent = true)

let results =
    seq {
        for file in IO.Directory.EnumerateFiles(config.OutputDir, "results*.xml") do
            use file = IO.File.OpenText(file)
            yield serializer.Deserialize<MissionResults>(file)
    }
    |> Seq.cache

let takeOffs =
    seq {
        for result in results do
            for takeOff in result.TakeOffs do
                yield takeOff.Plane
    }
    |> Seq.groupBy id
    |> Seq.map (fun (plane, planes) -> plane, Seq.length planes)
    |> Map.ofSeq

let groupSum xs : Map<PlaneModel, float32> =
    xs
    |> Seq.groupBy fst
    |> Seq.map (fun (model, dmgs) -> model, dmgs |> Seq.sumBy snd)
    |> Map.ofSeq

let landed =
    seq {
        for result in results do
            for landing in result.Landings do
                if landing.Health >= 0.75f then
                    yield landing.Plane, 1.0f
                else
                    yield landing.Plane, landing.Health
    }
    |> groupSum

let planesDamaged =
    seq {
        for result in results do
            for damage in result.Damages do
                match damage.Object with
                | ParkedPlane(_, model) -> yield (model, damage.Data.Amount)
                | _ -> ()
    }
    |> groupSum

let allModels =
    Seq.concat [(takeOffs |> Map.toSeq |> Seq.map fst); (planesDamaged |> Map.toSeq |> Seq.map fst)]
    |> Set.ofSeq

let statsOfCoalition coalition =
    seq {
        let coalitionModels = allModels |> Set.filter (fun plane -> plane.Coalition = coalition)
        for model in coalitionModels do
            let takeOffs = takeOffs.TryFind(model) |> Option.defaultValue 0
            let landedBack = landed.TryFind(model) |> Option.defaultValue 0.0f |> int
            let damaged = planesDamaged.TryFind(model) |> Option.defaultValue 0.0f
            yield sprintf "%20s, %4d, %3d, %3.1f, %3.1f\n" model.PlaneName takeOffs landedBack damaged (100.0f * (float32 landedBack) / (float32 takeOffs))
        let takeOffs =
            coalitionModels
            |> Seq.sumBy (takeOffs.TryFind >> Option.defaultValue 0)
        let landedBack =
            coalitionModels
            |> Seq.sumBy (landed.TryFind >> Option.defaultValue 0.0f)
            |> int
        let damaged =
            coalitionModels
            |> Seq.sumBy (planesDamaged.TryFind >> Option.defaultValue 0.0f)
        yield sprintf "%20s, %4d, %3d, %3.1f, %3.1f\n" ("total for " + (string coalition)) takeOffs landedBack damaged (100.0f * (float32 landedBack) / (float32 takeOffs))
    }

printfn "%20s, %4s, %3s, %3s, %3s" "Axis plane" "TAK" "LND" "DMG" "RAT"
statsOfCoalition Axis |> Seq.iter (printf "%s")

printfn ""

printfn "%20s, %4s, %3s, %3s, %3s" "Allies plane" "TAK" "LND" "DMG" "RAT"
statsOfCoalition Allies |> Seq.iter (printf "%s")
