namespace Util

/// Extensions to Option module
module Option =
    /// Turn result of TryGetValue-like functions into an option
    let ofPair =
        function
        | true, x -> Some x
        | false, _ -> None

    /// Turn option content to a pair using y as the second component
    let attach y =
        Option.map (fun x -> x, y)

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
                    |> Option.defaultValue z
                let n2 =
                    m2
                    |> Map.tryFind k
                    |> Option.defaultValue z
                yield k, n1 + n2
        }
        |> Map.ofSeq

    let isSumNull m1 =
        m1
        |> Map.toSeq
        |> Seq.sumBy snd
        |> ((=) 0)

[<AutoOpen>]
module MapExt =
    type Map<'K, 'V when 'K : comparison> with
        member this.Keys = this |> Seq.map (fun kvp -> kvp.Key)
        member this.Values = this |> Seq.map (fun kvp -> kvp.Value)

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

    /// Return the shortest prefix of a sequence where the last item
    /// in the prefix satisfies a predicate, or the entire sequence if no
    /// item satisfies the predicate.
    let takeUntil pred (xs : 'T seq) =
        seq {
            let it = xs.GetEnumerator()
            let rec work() =
                seq {
                    if it.MoveNext() then
                        yield it.Current
                        if not(pred it.Current) then
                            yield! work()
                }
            yield! work()
        }

    /// Return the item in a sequence which has highest projected value,
    /// interrupting the search when the projected value satisfies the
    /// given predicate.
    let maxByUntil proj pred xs =
        let init = xs |> Seq.head

        xs
        |> Seq.skip 1
        |> Seq.scan (fun ((_, h0) as s) x ->
            let h = proj x
            if h0 < h then
                (x, h)
            else
                s) (init, proj init)
        |> Seq.map fst
        |> takeUntil pred
        |> Seq.last

    /// Given sequences x1 ... xn, y1 ... yn', z1 ... zn'', return the sequence x1, y1, z1, x2, y2, z2...
    let interleave xs = xs |> Seq.transpose |> Seq.concat


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
    let keepTryingPaced retries (wait : int) action (items : 'F list) =
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

module RegexActivePatterns =
    open System.Text.RegularExpressions
    
    let (|MatchesRegex|_|) (regex : Regex) s =
        let m = regex.Match(s)
        if m.Success then
            Some m
        else
            None
    
    let (|GroupList|) (m : Match) =
        if m.Success then
            GroupList [
                for idx in 1 .. m.Groups.Count - 1 do
                    yield m.Groups.[idx].Value
            ]
        else
            GroupList []
    
    let (|AsInt|_|) s =
        match System.Int32.TryParse(s) with
        | true, x -> Some x
        | false, _ -> None

    let (|AsFloat|_|) s =
        let invCulture = System.Globalization.CultureInfo.InvariantCulture
        match System.Single.TryParse(s, System.Globalization.NumberStyles.Float, invCulture) with
        | true, x -> Some x
        | false, _ -> None


