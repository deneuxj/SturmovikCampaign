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
open FSharp.Json

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

