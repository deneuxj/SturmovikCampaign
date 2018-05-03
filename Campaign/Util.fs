// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
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

module Util

open FSharp.Control

/// <summary>
/// Build a dictionary from a sequence of items and expose it as getter function.
/// The entire dictionary is computed at once when the first call to the getter function is called.
/// </summary>
/// <param name="data">Sequence of items</param>
/// <param name="mkKey">Function computing the key of each item</param>
let mkGetStuffFast data mkKey =
    let m =
        lazy
            data
            |> Seq.map (fun x -> mkKey x, x)
            |> dict
    fun x -> m.Value.[x]

/// <summary>
/// Build a dictionary from a sequence of items and expose it as try-get function.
/// The entire dictionary is computed at once when the first call to the getter function is called.
/// </summary>
/// <param name="data">Sequence of items</param>
/// <param name="mkKey">Function computing the key of each item</param>
let mkTryGetStuffFast data mkKey =
    let m =
        lazy
            data
            |> Seq.map (fun x -> mkKey x, x)
            |> dict
    fun x ->
        match m.Value.TryGetValue(x) with
        | true, y -> Some y
        | _ -> None

/// Active pattern checking if a string contains a given substring.
let (|Contains|_|) substring (s : string) =
    if s.Contains(substring) then
        Some()
    else
        None

/// Expand a mapping from kinds to quantity to an array of kinds
let expandMap (m : Map<_, int>) =
    m
    |> Map.toSeq
    |> Seq.choose (fun (x, num) -> if num > 0 then Some(Array.create num x) else None)
    |> Array.concat

/// Count occurences of items in a sequence
let compactSeq (items : _ seq) =
    items
    |> Seq.fold (fun m item ->
        match Map.tryFind item m with
        | Some n -> n + 1
        | None -> 1
        |> fun n -> Map.add item n m
    ) Map.empty

/// Add two maps which target integers
let addMaps (m1 : Map<_, int>) (m2 : Map<_, int>) =
    m1
    |> Map.fold (fun m2 k count ->
        let old =
            Map.tryFind k m2
            |> function None -> 0 | Some n -> n
        let count = old + count
        Map.add k count m2
    ) m2

/// <summary>
/// Add a sequence of items to a multiset
/// </summary>
/// <param name="m">Map from items to count</param>
/// <param name="items">Sequence of items and count</param>
let addList (m : Map<'T, int>) (items : ('T * int) seq) =
    let addOne (m : Map<'T, int>) (x, n) =
        let n2 =
            m.TryFind x
            |> function None -> 0 | Some n -> n
            |> (+) n
        m.Add(x, n2)
    items
    |> Seq.fold addOne m
    |> Map.filter (fun _ v -> v > 0)

/// Subtract m2 from m1 where both are maps which target integers
let subMaps (m1 : Map<_, int>) (m2 : Map<_, int>) =
    m2
    |> Map.fold (fun m1 k count ->
        let old =
            Map.tryFind k m1
            |> function None -> 0 | Some n -> n
        let count = old - count
        Map.add k count m1
    ) m1

/// X divided by Y, rounded up
let divUp x y =
    assert(x >= 0)
    assert(y > 0)
    (x / y) + (if x % y = 0 then 0 else 1)

/// Extensions to Option module
module Option =
    let defaultVal x y = defaultArg y x

/// Extensions to Array module
module Array =
    let shuffle (random : System.Random) xs =
        xs
        |> Array.map (fun xs -> random.NextDouble(), xs)
        |> Array.sortBy fst
        |> Array.map snd

    let cross2 (arr1 : _[]) (arr2 : _[]) =
        seq {
            for x in arr1 do
                for y in arr2 do
                    yield (x, y)
        }

module Map =
    let getKeys m =
        m
        |> Map.toSeq
        |> Seq.map fst
        |> Set.ofSeq

    let sumUnion m1 m2 =
        let allKeys = Set.unionMany [ getKeys m1; getKeys m2 ]
        let z = LanguagePrimitives.Float32WithMeasure 0.0f
        seq {
            for k in allKeys do
                let n1 =
                    m1
                    |> Map.tryFind k
                    |> Option.defaultVal z
                let n2 =
                    m2
                    |> Map.tryFind k
                    |> Option.defaultVal z
                yield k, n1 + n2
        }
        |> Map.ofSeq

 module Seq =
    /// <summary>
    /// Split a sequence at elements where a predicate evaluates to true.
    /// </summary>
    /// <param name="pred">The predicate</param>
    /// <param name="xs">The sequence</param>
    let split pred (xs : 'T seq) =
        let it = xs.GetEnumerator()
        let untilPred() =
            [
                while it.MoveNext() && not (pred it.Current) do
                    yield it.Current
            ]
        let current() =
            try
                Some it.Current
            with
            | _ -> None
        seq {
            if it.MoveNext() then
                yield it.Current :: untilPred()
            while current().IsSome do
                yield it.Current :: untilPred()
        }

/// Misc useful algorithms.
module Algo =
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

module AsyncSeq =
    let mergeChoice3 xa xb xc =
        [ xa |> AsyncSeq.map Choice1Of3
          xb |> AsyncSeq.map Choice2Of3
          xc |> AsyncSeq.map Choice3Of3 ]
        |> AsyncSeq.mergeAll

module Async =
    open System.IO
    open System.ComponentModel

    let catchLog label task =
        async {
            try
                return! task
            with
            | e ->
                printfn "Task '%s' failed: %s at\n%s" label e.Message e.StackTrace
                return()
        }

    /// <summary>
    /// Try to run a task, catch ay raised exception and return is an error
    /// </summary>
    let tryTask task =
        async {
            try
                let! res = task
                return Result.Ok res
            with e -> return Result.Error e
        }

    type Attempt<'S, 'R> =
    | KeepTrying of 'S
    | GiveUp of 'S
    | Completed of 'R

    /// <summary>
    /// Repeatedly try a task until it completes or gives up.
    /// </summary>
    /// <param name="wait">Task to run between invocations of task. Typical use would be to wait</param>
    /// <param name="exch">Task run instead of wait when the task raises an exception</param>
    /// <param name="task">Task to run. It must return some state if it failed to complete. In that case this state is passed to the task in the next try.</param>
    /// <param name="state">Initial value of state to pass to the task</param>
    let keepTrying (wait : 'S2 -> Async<'S1>) (exch : 'S1 -> System.Exception -> Async<'S1>) (task : 'S1 -> Async<Attempt<'S2, 'R>>) state =
        let rec work state =
            async {
                let! res = tryTask (task state)
                match res with
                | Result.Ok(GiveUp partial) ->
                    return Result.Error partial
                | Result.Ok(Completed result) ->
                    return Result.Ok result
                | Result.Ok(KeepTrying s) ->
                    let! s = wait s
                    return! work s
                | Result.Error e ->
                    let! s = exch state e
                    return! work s
            }
        work state

    /// <summary>
    /// Repeatedly try to perform an action on a list of items. Return number of tries and list of failed items wrapped in Error, or OK if successful.
    /// </summary>
    let keepTryingPaced retries wait action (items : 'F list) =
        let failed files =
            seq {
                for file in files do
                    let failed =
                        try
                            action(file)
                            None
                        with
                        | :? System.IO.IOException -> Some file
                    match failed with
                    | Some file -> yield file
                    | None -> ()
            }
        let attempt (retries, items) =
            async {
                if Seq.isEmpty items then
                    return Completed()
                elif retries <= 0 then
                    return GiveUp(0, items)
                else
                    let failed = List.ofSeq (failed items)
                    return KeepTrying(retries - 1, failed)
            }
        let exch s _ =
            async {
                do! Async.Sleep(wait)
                return s
            }
        let wait s =
            async {
                do! Async.Sleep(wait)
                return s
            }
        keepTrying wait exch attempt (retries, items)