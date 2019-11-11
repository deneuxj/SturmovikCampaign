#I @"..\bin\Debug"

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

let nearestNodes (v : Vector2) (nodes : NetworkNode list) =
    let withinRadius =
        nodes
        |> List.filter (fun node -> (node.Pos - v).LengthSquared() <= 25e6f)
    withinRadius
    |> List.sortBy (fun node -> (node.Pos - v).LengthSquared())
    |> List.truncate 5

let dir = @"C:\Users\johann\Documents\ungtp\Maps\graphics\landscape_rheinland_wi\roads"
let origin = Vector2(30000.0f, 30000.0f)
let corner = Vector2(354000.0f, 431000.0f)
let network = NetworkNode.NodesFromIni(Path.Combine(dir, "roadssystem.ini"), Path.Combine(dir, "railroads.ini"), origin, corner, 1.0f<E/H>)

nearestNodes (Vector2(290000.0f, 390000.0f)) network