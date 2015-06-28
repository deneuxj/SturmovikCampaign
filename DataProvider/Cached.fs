module SturmovikMission.DataProvider.Cached

open System.Collections.Generic

/// <summary>
/// Cache calls to a function and its results.
/// </summary>
/// <param name="cache">Dictionary which caches results.</param>
/// <param name="f">Function to cache</param>
/// <param name="x">Parameter to pass to the function</param>
let cached (cache : IDictionary<'a, 'b>) f x =
    match cache.TryGetValue(x) with
    | true, y -> y
    | false, _ ->
        let y = f x
        cache.Add(x, y)
        y
