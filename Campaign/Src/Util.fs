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

/// Get approximate sunrise and sunset times for a day (northern hemisphere)
let suntimes(date : System.DateTime) =
    let longestDay = 16.0
    let shortestDay = 9.0
    let t = 2.0 * System.Math.PI * (date - System.DateTime(date.Year, 6, 22, 12, 0, 0)).TotalDays / 365.0
    let t2 = 0.5 * (cos(t) + 1.0)
    let dayLength = (1.0 - t2) * shortestDay + t2 * longestDay
    let rise = 13.0 - 0.5 * dayLength
    let set = 13.0 + 0.5 * dayLength
    let sunrise = System.DateTime(date.Year, date.Month, date.Day, int rise, 0, 0)
    let sunset = System.DateTime(date.Year, date.Month, date.Day, int set, 0, 0)
    sunrise, sunset

/// Function to cache computationally expensive properties of objects.
/// Use this from a static variable, e.g. a module-level let binding.
/// Do not use from a static property, as those are evaluated every call.
let cachedProperty f =
    let cache = System.Runtime.CompilerServices.ConditionalWeakTable()
    fun this ->
        match cache.TryGetValue this with
        | false, _ ->
            let value = f this
            cache.Add(this, box value)
            value
        | true, x ->
            unbox x

/// Extensions to Option module
module Option =
    let defaultVal x y = defaultArg y x

    /// Turn result of TryGetValue-like functions into an option
    let ofPair =
        function
        | true, x -> Some x
        | false, _ -> None

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

    let avg (arr : _[]) =
        let len = float32 arr.Length
        if len = 0.0f then
            invalidArg "arr" "must not be empty"
        else
            (Array.sum arr) / len


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

    let isSumNull m1 =
        m1
        |> Map.toSeq
        |> Seq.sumBy snd
        |> ((=) 0)

 module Seq =
    open System.Collections.Generic

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

    type System.Collections.Generic.IEnumerator<'T> with
        /// Try to move the enumerator, if it succeded return the current value
        member this.MoveNext2() =
            let ok = this.MoveNext()
            if ok then
                Some this.Current
            else
                None

    /// Return the remaining elements in an enumerator as a seq
    let unroll (it : IEnumerator<'T>) =
        seq {
            while it.MoveNext() do
                yield it.Current
        }

    /// Merge operation similar to the one used in merge sort, but allows the two sequences to be of different types.
    let mergeOrdered keyA keyB (seqA : 'T1 seq) (seqB : 'T2 seq) =
        seq {
            let it1 = seqA.GetEnumerator()
            let it2 = seqB.GetEnumerator()
            let rec advBoth()=
                seq {
                    let left = it1.MoveNext2()
                    let right = it2.MoveNext2()

                    match left, right with
                    | None, None -> ()
                    | Some x, None ->
                        yield Choice1Of2 x
                        yield! unroll it1 |> Seq.map Choice1Of2
                    | None, Some x ->
                        yield Choice2Of2 x
                        yield! unroll it2 |> Seq.map Choice2Of2
                    | Some a, Some b ->
                        match keyA a, keyB b with
                        | x, y when x < y ->
                            yield Choice1Of2 a
                            yield! advLeft (b, y)
                        | x, _ ->
                            yield Choice2Of2 b
                            yield! advRight (a, x)
                }
            and advLeft (b, y) =
                seq {
                    match it1.MoveNext2() with
                    | None ->
                        yield Choice2Of2 b
                        yield! unroll it2 |> Seq.map Choice2Of2
                    | Some a ->
                        match keyA a with
                        | x when x < y ->
                            yield Choice1Of2 a
                            yield! advLeft (b, y)
                        | x ->
                            yield Choice2Of2 b
                            yield! advRight (a, x)
                }
            and advRight (a, x) =
                seq {
                    match it2.MoveNext2() with
                    | None ->
                        yield Choice1Of2 a
                        yield! unroll it1 |> Seq.map Choice1Of2
                    | Some b ->
                        match keyB b with
                        | y when x < y ->
                            yield Choice1Of2 a
                            yield! advLeft (b, y)
                        | _ ->
                            yield Choice2Of2 b
                            yield! advRight (a, x)
                }
            yield! advBoth()
        }

    let mutableDict (xs : ('K * 'V) seq) =
        let dict = Dictionary(HashIdentity.Structural)
        for k, v in xs do
            dict.Add(k, v)
        dict

    let selectFrom2 selector (xs1 : 'T1 seq) (xs2 : 'T2 seq) =
        seq {
            let it1 = xs1.GetEnumerator()
            let it2 = xs2.GetEnumerator()

            let rec select(x, y) =
                seq {
                    if selector x y then
                        yield Choice2Of2 y
                        yield! advRight x
                    else
                        yield Choice1Of2 x
                        yield! advLeft y
                }
            and advLeft(y) =
                seq {
                    match it1.MoveNext2() with
                    | None ->
                        yield Choice2Of2 y
                        yield! unroll it2 |> Seq.map Choice2Of2
                    | Some x ->
                        yield! select(x, y)
                }
            and advRight(x) =
                seq {
                    match it2.MoveNext2() with
                    | None ->
                        yield Choice1Of2 x
                        yield! unroll it1 |> Seq.map Choice1Of2
                    | Some y ->
                        yield! select(x, y)
                }
            match it1.MoveNext2(), it2.MoveNext2() with
            | None, None -> ()
            | None, Some y ->
                yield Choice2Of2 y
                yield! unroll it2 |> Seq.map Choice2Of2
            | Some x, None ->
                yield Choice1Of2 x
                yield! unroll it1 |> Seq.map Choice1Of2
            | Some x, Some y ->
                yield! select(x, y)
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

    /// Return the remaining elements in an enumerator as an AsyncSeq
    let unroll (it : IAsyncEnumerator<'T>) =
        let rec loop() =
            asyncSeq {
                let! x = it.MoveNext()
                match x with
                | Some x ->
                    yield x
                    yield! loop()
                | None ->
                    ()
            }
        loop()

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
    /// Try to run a task, catch any raised exception and return it as an error
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