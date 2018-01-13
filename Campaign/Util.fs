﻿module Util

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