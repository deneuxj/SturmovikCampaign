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

let planeDb =
    planeDb
    |> List.map (fun plane -> plane.Id, plane)
    |> Map.ofList

let planeSet =
    Bodenplatte.allPlanesOf Axis @ Bodenplatte.allPlanesOf Allies
    |> List.choose (fun plane -> planeDb.TryFind plane |> Option.orElseWith (fun () -> printfn "%s missing" (string plane); None))
