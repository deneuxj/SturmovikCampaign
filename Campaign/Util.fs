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