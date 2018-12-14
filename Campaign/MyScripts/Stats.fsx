#I @"..\bin\Debug"

#r "Campaign.dll"
#r "ploggy"
#r "FsPickler"

#load "Configuration.fsx" 

open System
open MBrace.FsPickler
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.NewWorldState
open Campaign.ResultExtraction
open Campaign.PlaneModel
open Campaign.BasicTypes

let serializer = FsPickler.CreateXmlSerializer(indent = true)

let results =
    seq {
        for file in IO.Directory.EnumerateFiles(@"C:\Users\johann\Documents\FromServer\Expert", "results_*.xml") do
            use file = IO.File.OpenText(file)
            yield serializer.Deserialize<MissionResults>(file)
    }
    |> Seq.cache

let states =
    seq {
        for file in IO.Directory.EnumerateFiles(@"C:\Users\johann\Documents\FromServer\Expert", "state_*.xml") do
            use file = IO.File.OpenText(file)
            yield serializer.Deserialize<WorldState>(file)
    }
    |> Seq.cache

let world =
    use file = IO.File.OpenText(@"C:\Users\johann\Documents\FromServer\Expert\world.xml")
    serializer.Deserialize<World>(file)

let takeOffs =
    seq {
        for result in results do
            for takeOff in result.TakeOffs do
                yield takeOff.Plane
    }
    |> Seq.groupBy id
    |> Seq.map (fun (plane, planes) -> plane, Seq.length planes)
    |> Map.ofSeq

let groupSum xs : Map<_, float32> =
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

let targetDamages =
    seq {
        let wg = world.FastAccess
        for result, state in Seq.zip results states do
            let sg = state.FastAccess
            for damage in result.Damages do
                let coalition = damage.Object.Coalition(wg, sg)
                match damage.Object with
                | Airfield _ -> yield (("airfield", coalition), damage.Data.Amount)
                | Production _ -> yield (("production", coalition), damage.Data.Amount)
                | Storage _ -> yield (("supplies", coalition), damage.Data.Amount)
                | Column _ | Vehicle _ -> yield (("tank", coalition), damage.Data.Amount)
                | Convoy _ -> yield (("convoy", coalition), damage.Data.Amount)
                | _ -> ()
            for damage in result.BattleKills do
                yield (("tank", Some damage.Coalition), 1.0f)
    }
    |> groupSum
    |> Map.toSeq

let damagesByPlayers =
    seq {
        let wg = world.FastAccess
        for result, state in Seq.zip results states do
            let sg = state.FastAccess
            for damage in result.Damages do
                yield (damage.Data.ByPlayer, damage.Value(wg, sg) / 1.0f<E>)
    }
    |> groupSum
    |> Map.toSeq
    |> Seq.sortByDescending snd

let allModels =
    Seq.concat [(takeOffs |> Map.toSeq |> Seq.map fst); (planesDamaged |> Map.toSeq |> Seq.map fst)]
    |> Set.ofSeq

let dedicatedFighterMedianTakeOffs, numFighterDataPoints =
    let data =
        seq {
            for result in results do
                for takeOff in result.TakeOffs do
                    if takeOff.Plane.PlaneType = Fighter && takeOff.PlayerName.IsSome then
                        yield takeOff.PlayerName.Value, takeOff.Plane
        }
        |> Seq.groupBy fst
        |> Seq.map (fun (_, xs) -> Seq.length xs |> float)
        |> Seq.filter (fun n -> n > 5.0)
        |> Array.ofSeq
    match data with
    | [||] -> 0.0, 0
    | _ ->
        let rec work lower upper =
            if upper - lower < 0.1 then
                0.5 * (upper + lower)
            else
                let v = 0.5 * (upper + lower)
                let numAbove =
                    data
                    |> Seq.filter (fun x -> x > v)
                    |> Seq.length
                if 2 * numAbove > data.Length then
                    work v upper
                else
                    work lower v
        work (Seq.min data) (Seq.max data), data.Length

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
        yield "\n"
        for (item, c), amount in targetDamages do
            if c = Some coalition then
                yield sprintf "%16s: %3.1f\n" item amount
    }

printfn "%20s, %4s, %3s, %3s, %3s" "Axis plane" "TAK" "LND" "DMG" "RAT"
statsOfCoalition Axis |> Seq.iter (printf "%s")

printfn ""

printfn "%20s, %4s, %3s, %3s, %3s" "Allies plane" "TAK" "LND" "DMG" "RAT"
statsOfCoalition Allies |> Seq.iter (printf "%s")

printfn ""

for pos, (player, damages) in Seq.truncate 10 damagesByPlayers |> Seq.indexed do
    printfn "%02d %20s %8.0f" (pos + 1) (player |> Option.defaultValue "Unknown") damages

printfn ""
printfn "Median fighter take-offs per pilot: %3.1f (based on %d pilots)" dedicatedFighterMedianTakeOffs numFighterDataPoints
