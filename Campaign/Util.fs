module Campaign.Util

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
                    |> List.choose (fun node -> update node (Map.tryFind node mapping) value |> Option.map (fun v -> node, v))
                let mapping = Map.add node value mapping
                let working = rest @ affected
                work (mapping, working)
        work (Map.empty, roots)
