// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
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

/// Misc useful algorithms.
module Campaign.Common.Algo
    /// <summary>
    /// Assign values to nodes in a graph, starting from a set of roots
    /// </summary>
    /// <param name="getSuccessors">Returns the neighbours of a node that are potentially affected by a change of value of that node</param>
    /// <param name="update">Given the old and new value of a predecessor, returns whether some updated value if any, None otherwise</param>
    /// <param name="roots">The initial list of nodes with their values</param>
    let propagate getSuccessors update roots =
        let rec work (mapping, working) =
            match working with
            | [] -> mapping
            | (node, value) :: rest ->
                let affected =
                    getSuccessors node
                    |> List.choose (fun next -> update node next (Map.tryFind next mapping) value |> Option.map (fun v -> next, v))
                let mapping = Map.add node value mapping
                let working = rest @ affected
                work (mapping, working)
        work (Map.ofList roots, roots)

    /// <summary>
    /// Partition a list according to a provided "similarity relation":
    /// Two items are considered equivalent if two items are similar according to the transitive closure of the similarity relation
    /// </summary>
    /// <param name="items">The list to partition</param>
    /// <param name="areSimilar">The similarity relation.</param>
    let computePartition areSimilar items =
        let singletons =
            items
            |> List.map (fun v -> [v])
        let classes =
            items
            |> Seq.fold (fun equivClasses v ->
                let near, far =
                    equivClasses
                    |> List.partition (fun points ->
                        points
                        |> List.exists (areSimilar v)
                    )
                List.concat near :: far
            ) singletons
        classes

    /// <summary>
    /// Given a partition, compute a function that returns some unique representative for each item in some of the equivalence classes.
    /// </summary>
    /// <param name="classes"></param>
    let getEquivalent classes =
        let m =
            classes
            |> Seq.map (fun cl ->
                match cl with
                | lead :: rest ->
                    cl
                    |> List.map (fun v -> v, lead)
                | [] ->
                    []
            )
            |> Seq.concat
            |> dict
        fun v -> m.[v]
