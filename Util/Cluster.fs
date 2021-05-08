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

type ClusterPartition<'T> =
    {
        Clusters : ('T * Vector2) list list
        Radius : float32
    }

[<RequireQualifiedAccess>]
module ClusterPartition =
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
        {
            Clusters = [posItems]
            Radius = radius
        }

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

    let refine k (cp : ClusterPartition<_>) =
        if k > 1.0f then
            invalidArg "factor" "must be 1.0 or less"
        if k <= 0.0f then
            invalidArg "factor" "must be positive"
        let radius = k * cp.Radius
        let clusters =
            cp.Clusters
            |> List.collect (partition radius)
        {
            Clusters = clusters
            Radius = radius
        }

//module private Tests =
//    open FsCheck
//    open FsCheck.Xunit