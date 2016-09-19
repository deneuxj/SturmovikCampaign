/// Utility functions for the manipulation of predicates.
module SturmovikMission.Blocks.Predicates

/// <summary>
/// Get value of a function for a given input. If the function is undefined for that function, throws
/// a KeyNotFound exception. If the function is not actually function, i.e. the underlying predicate
/// has multiple values associated to the input, one of them is arbitraly returned.
/// </summary>
/// <param name="set">
/// The function, i.e. a binary predicate P such that for every x, there is at most one y such P(x, y).
/// </param>
/// <param name="key">The value of the input.</param>
let get set key =
    set
    |> Set.filter (fst >> ((=) key))
    |> Set.minElement
    |> snd

/// <summary>
/// Try to get the value of a function for a given input. If the function is not defined for that
/// input, return None. If the function is not actually a function, i.e. the underlying predicate has
/// multiple values associated to the input, return some arbitrary value.
/// </summary>
/// <param name="set">The function, i.e. a  binary predicate P such that for every x, there is at most one y such P(x, y).</param>
/// <param name="key">The value of the input.</param>
let tryGet set key =
    let candidates =
        set
        |> Set.filter (fst >> ((=) key))
    if Set.isEmpty candidates then
        None
    else
        candidates
        |> Set.minElement
        |> snd
        |> Some

/// <summary>
/// Filter a tertiary predicate by specific optional values for each position.
/// </summary>
/// <param name="set">The tertiary predicate.</param>
/// <param name="key1">Optional value for the first position.</param>
/// <param name="key2">Optional value for the second position.</param>
/// <param name="key3">Optional value for the third position.</param>
let filter3 set (key1, key2, key3) =
    set
    |> Set.filter (fun (x, y, z) ->
        match key1 with
        | None -> true
        | Some k -> k = x
        &&
        match key2 with
        | None -> true
        | Some k -> k = y
        &&
        match key3 with
        | None -> true
        | Some k -> k = z)

/// <summary>
/// Return the set of reachable states from a given start state and a transition relation.
/// </summary>
/// <param name="binaryRel">
/// The transition relation, i.e. a binary predicate where the types of the first and second
/// arguments are the same.
/// </param>
/// <param name="start">Starting state.</param>
let star binaryRel start =
    let rec work current res =
        // FIXME: this code actually works only for transition relations where each state has at most one successor.
        match tryGet binaryRel current with
        | Some next -> work next (Set.add next res)
        | None -> res
    work start (Set.singleton start)
