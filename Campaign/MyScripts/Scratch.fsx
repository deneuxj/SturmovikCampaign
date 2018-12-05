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
    let rec loop() =
        seq {
            match it.MoveNext2() with
            | Some x ->
                yield x
                yield! loop()
            | None ->
                ()
        }
    loop()

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

mergeOrdered id id [1; 3; 0; 5; 0; 7] [2; 0; 4; 6; 8; 10; 0]