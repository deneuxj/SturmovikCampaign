// RoadExtractor, a commandline utility to extract road and railroad networks
// from the IL-2 Sturmovik: Great Battles series.
//
// Copyright (C) 2019 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

open System
open System.IO
open FSharp.Json

/// A node in the logistics network
type NetworkNode = {
    Id : int
    Pos : float32 * float32
    Neighbours : int list 
}

type Network = {
    Nodes : NetworkNode list
}
with
    /// Build a graph from map ini files (roadssystem.ini and roads.ini or railroads.ini)
    static member NodesFromIni(system : string, roads : string) =
        // parse a float using the invariant culture
        let parseFloat s =
            try
                System.Single.Parse(s, System.Globalization.CultureInfo.InvariantCulture)
            with _ -> failwithf "Failed to parse '%s' as a float" s
        // Shorthand for Regex.Match
        let matchf(line, pat) = System.Text.RegularExpressions.Regex.Match(line, pat)
        // Helper function to extract a float value from an ini file
        let getValue (key : string) lines =
            let line =
                try
                    lines
                    |> Array.find (fun (line : string) -> line.StartsWith(key))
                with _ -> failwithf "Could not find entry '%s'" key
            let posEqual = line.IndexOf("=")
            if posEqual = -1 then
                failwithf "Entry '%s' found but ill-formed" key
            parseFloat(line.Substring(posEqual + 1))
        // Relevant values extracted from roadssystem.ini
        let parseStep, mapHeight, scaleFactor =
            let lines =
                try
                    System.IO.File.ReadAllLines(system)
                with _ ->
                    failwithf "Failed to read road system ini file '%s'" system
            getValue "RoadSegmentParseStep" lines,
            getValue "Map_Height" lines,
            getValue "Map_ScaleFactor" lines
        // Transformation to apply to each coordinate component
        let truncateAndScale x =
            floor(x / parseStep) * parseStep * float32 scaleFactor
        // Extract coordinates from roads.ini
        let lanes =
            [
                for line in System.IO.File.ReadAllLines(roads) do
                    yield [
                        let mutable s = line
                        while not(System.String.IsNullOrWhiteSpace(s)) do
                            let m = matchf(s, @"(\d+(\.\d*)?),(\d+(\.\d*)?)(.*)")
                            if m.Success then
                                yield parseFloat(m.Groups.[1].Value), parseFloat(m.Groups.[3].Value)
                                s <- m.Groups.[5].Value
                            else
                                s <- ""
                    ]
            ]
            |> List.map(List.map (fun (x, y) -> truncateAndScale (mapHeight - y), truncateAndScale x))
        // Create all nodes
        let nodes =
            lanes
            |> List.concat
            |> List.fold (fun nodes (x, y) ->
                let node =
                    Map.tryFind (x, y) nodes
                    |> Option.defaultValue
                        { Id = nodes.Count
                          Pos = (x, y)
                          Neighbours = []
                        }
                Map.add (x, y) node nodes
            ) Map.empty
        // Set neighbours
        let nodes =
            (nodes, lanes)
            ||> List.fold (fun nodes lane ->
                (nodes, List.pairwise lane)
                ||> List.fold (fun (nodes : Map<_, NetworkNode>) (v1, v2) ->
                    let node1 = nodes.[v1]
                    let node2 = nodes.[v2]
                    nodes
                    |> Map.add v1 { node1 with Neighbours = node2.Id :: node1.Neighbours }
                    |> Map.add v2 { node2 with Neighbours = node1.Id :: node2.Neighbours }
                ))
        // Retain nodes from the mapping, remove self-references in neighbours
        let nodes =
            nodes
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.map (fun node -> { node with Neighbours = node.Neighbours |> List.filter ((<>) node.Id) })
            |> List.ofSeq
        // Result
        { Nodes = nodes }


[<EntryPoint>]
let main argv =
    match argv with
    | [| path |] ->
        let dir = Path.GetDirectoryName(path)
        let system = Path.Combine(dir, "roadssystem.ini")
        let nodes = Network.NodesFromIni(system, path)
        let json = Json.serialize nodes
        let outputPath = Path.Combine(dir, Path.GetFileNameWithoutExtension(path) + ".json")
        use file = File.CreateText(outputPath)
        file.Write(json)
    | _ ->
        printfn """
Usage: RoadExtractor <road or railroad description path>

Extract road network from provided file, and write result as in file with same name and extension .json
"""

    printfn "Done."
    0 // return an integer exit code
