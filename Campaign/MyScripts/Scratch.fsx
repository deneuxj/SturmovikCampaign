#I @"..\bin\Debug"
#I @"C:\Users\johann\Documents\SturmovikCampaign\packages\FSharp.Data.3.3.2\lib\net45"

#r "Campaign.dll"
#r "System.Numerics.Vectors.dll"
#r "DataProvider.dll"
#r "ploggy"
#r "FsPickler"

open System
open System.IO
open System.Numerics
open Campaign.BasicTypes
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.Missions
open Campaign.MissionGenerator
open Campaign.PlaneModel
open DamageExtension

let missionDir = @"C:\Users\johann\Documents\SturmovikCampaign\Campaign\data"

let planeDb =
    planeDb
    |> List.map (fun plane -> plane.Id, plane)
    |> Map.ofList

let planeSet =
    Bodenplatte.allPlanesOf Axis @ Bodenplatte.allPlanesOf Allies
    |> List.choose (fun plane -> planeDb.TryFind plane |> Option.orElseWith (fun () -> printfn "%s missing" (string plane); None))
    |> Seq.map (fun plane -> plane.Id, plane)
    |> dict

let world =
    let truck = 5.0f<M^3>
    let separation = 10.0f<M>
    let speed = 50000.0f<M/H>
    let numTrucks = speed / separation
    let roadCapacity = numTrucks * truck
    let x = Loading.loadWorld(Path.Combine(missionDir, "RheinlandSummer.Mission"), 10000.0f<E/H>, roadCapacity, roadCapacity * 3.0f)
    { x with PlaneSet = planeSet }

let war = Init.mkWar world
Bodenplatte.initAirfields Axis war
Bodenplatte.initAirfields Allies war

let mutable step = Bodenplatte.start war
let random = System.Random(0)

let advance() =
    match step with
    | Stalemate -> printfn "Stalemate"
    | Victory side -> printfn "%s is victorious" (string side)
    | Ongoing data ->
        printfn "New mission: %s" data.Briefing
        let sim = MissionSimulator(random, war, data.Missions, 10.0f<H>)
        let events = sim.DoAll()
        for cmd, descr in events do
            printfn "Action: %s" descr
            cmd
            |> Option.iter (fun cmd ->
                for result in cmd.Execute(war) do
                    printfn "Result: %s" (Results.asString war result))
        step <- data.Next war

