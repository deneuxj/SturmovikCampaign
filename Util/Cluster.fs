// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2021 Johann Deneux <johann.deneux@gmail.com>
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

namespace Campaign.Cluster

open System.Numerics
open VectorExtension

type Cluster<'T> =
    {
        Items : ('T * Vector2) list
        Radius : float32
    }

type ClusterPartition<'T> =
    {
        Clusters : Cluster<'T> list
    }

[<RequireQualifiedAccess>]
module ClusterPartition =
    /// Create a cluster partition composed of a single group of all the provided items
    let create (getPosition : 'T -> Vector2) (items : 'T seq) =
        let posItems =
            items
            |> Seq.map (fun item -> item, getPosition item)
            |> List.ofSeq

        let (minx, miny, maxx, maxy) =
            ((System.Single.NegativeInfinity, System.Single.NegativeInfinity, System.Single.PositiveInfinity, System.Single.PositiveInfinity), posItems)
            ||> List.fold (fun (minx, miny, maxx, maxy) (_, v) ->
                (
                    min minx v.X,
                    min miny v.Y,
                    max maxx v.X,
                    max maxy v.Y
                )
            )
        let radius = Vector2(maxx - minx, maxy - miny).Length()
        let cluster =
            {
                Items = posItems
                Radius = radius
            }
        {
            Clusters = [cluster]
        }

    /// Partition the items in a cluster according to proximity between items
    let private partition (radius : float32) (items : ('T * Vector2) list) =
        let r2 = radius * radius
        let rec work (clusters : ('T * Vector2) list list) (working : ('T * Vector2) list) =
            match working with
            | [] -> clusters
            | ((_, pos) as item) :: working ->
                let close, far =
                    clusters
                    |> List.partition (List.exists (fun (_, pos2) -> (pos - pos2).LengthSquared() <= r2))
                let clusters = (item :: (List.concat close)) :: far
                work clusters working
        work [] items

    /// Reduce the accumulation radius in a cluster and split it accordingly
    let split k (cluster : Cluster<_>) =
        if k > 1.0f then
            invalidArg "factor" "must be 1.0 or less"
        if k <= 0.0f then
            invalidArg "factor" "must be positive"
        let radius = k * cluster.Radius
        let clusters =
            cluster.Items
            |> partition radius
            |> List.map (fun items -> { Items = items; Radius = radius })
        clusters

    /// Get the maximum distance between any two items in a cluster
    let diameter (items : ('T * Vector2) list) =
        let rec distFrom (pos : Vector2) maxDist (working : ('T * Vector2) list) =
            match working with
            | [] -> maxDist
            | (_, pos2) :: working -> 
                let dist = (pos - pos2).LengthSquared()
                let maxDist = max maxDist dist
                distFrom pos maxDist working
        let rec work maxDist working =
            match working with
            | [] -> maxDist
            | (_, pos) :: working ->
                let maxDist = distFrom pos maxDist working
                work maxDist working
        work 0.0f items
        |> sqrt

    /// Refine a cluster partition until all clusters have a diameter less than a specified limit
    let refine k maxDist maxIter (cp : ClusterPartition<_>) =
        let rec work maxIter (cp : ClusterPartition<_>) =
            let diameters =
                cp.Clusters
                |> List.map (fun c -> diameter c.Items)
            let clusters =
                (cp.Clusters, diameters)
                ||> List.map2 (fun cluster d ->
                    if d <= maxDist then
                        [cluster]
                    else
                        split k cluster)
                |> List.concat
            let cp2 =
                { cp with
                    Clusters = clusters
                }
            if maxIter <= 0 || diameters |> List.forall (fun d -> d <= maxDist) then
                cp2
            else
                work (maxIter - 1) cp2
        work maxIter cp