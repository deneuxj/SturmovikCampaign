﻿open System.IO
open Campaign.WorldDescription
open Campaign.PlaneSet
open Campaign.BasicTypes
open Campaign
open System.Reflection

// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv =
    let missionFile =
        match argv with
        | [| x |] ->
            if File.Exists x then
                x
            else
                failwithf "Can't open %s" x
        | _ -> failwith "Usage: WorldCheck <mission file>"

    let subBlocksFile = "SubBlocks.yaml"
    let exePath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    let planeSet = PlaneSet.Default
    let world =
        try
            World.Create(planeSet, missionFile, 1.0f<E/H>, Path.Combine(exePath, subBlocksFile))
        with
        | e -> failwithf "Error in mission file: %s" e.Message

    let capacity =
        world.Regions
        |> Seq.map (fun region -> region.RegionId, region.Storage |> Seq.sumBy (fun sto -> sto.Storage world.SubBlockSpecs))
        |> Map.ofSeq

    let production =
        world.Regions
        |> Seq.map (fun region -> region.RegionId, region.Production |> Seq.sumBy (fun prod -> prod.Production(world.SubBlockSpecs, world.ProductionFactor)))
        |> Seq.map (fun (region, production) -> region, production)
        |> Map.ofSeq

    let antiAirUsage =
        world.AntiAirDefenses
        |> Seq.map (fun def -> def.Home, def.MaxNumGuns)
        |> Seq.groupBy fst
        |> Seq.map (fun (region, canons) -> region, canons |> Seq.sumBy snd)
        |> Map.ofSeq

    let state =
        try
            WorldState.mkInitialState(world, missionFile, 0.0f)
        with
        | e -> failwithf "Error in campaign init: %s" e.Message

    let sg = state.FastAccess

    let description =
        [
            yield sprintf "%20s | %14s | %14s" "region" "Prod - cap" "AA"
            for region in world.Regions do
                let (RegionId regionName) = region.RegionId
                let aa = Map.tryFind region.RegionId antiAirUsage |> fun x -> defaultArg x 0
                let cap = Map.tryFind region.RegionId capacity |> fun x -> defaultArg x 0.0f<E>
                let prod = world.ProductionFactor * (Map.tryFind region.RegionId production |> fun x -> defaultArg x 0.0f<E/H>)
                yield sprintf "%20s | %6.1f - %5.0f | %4d - %5.1f%%" regionName prod cap aa (100.0f * float32 aa / (cap / cannonCost))
            for coalition in [Axis; Allies] do
                let totalProd =
                    production
                    |> Map.filter (fun region _ -> sg.GetRegion(region).Owner = Some coalition)
                    |> Map.toSeq
                    |> Seq.sumBy snd
                yield sprintf "Total production for %s: %0.0f" (string coalition) totalProd
        ]

    for line in description do
        System.Console.WriteLine(line)

    System.Console.WriteLine("Press a key to exit")
    System.Console.ReadKey(true) |> ignore
    0 // return an integer exit code