//    Copyright 2015 Johann Deneux
//
//    This file is part of SturmovikMission.
//
//    SturmovikMission is free software: you can redistribute it and/or modify
//    it under the terms of the GNU Lesser General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    SturmovikMission is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public License
//    along with SturmovikMission.  If not, see <http://www.gnu.org/licenses/>.

module SturmovikMission.DataProvider.Cached

open System.Collections.Generic

/// <summary>
/// Cache calls to a function and its results.
/// </summary>
/// <param name="cache">Dictionary which caches results.</param>
/// <param name="f">Function to cache</param>
/// <param name="x">Parameter to pass to the function</param>
let cached (cache : IDictionary<'a, 'b>) f x = 
    lock cache (fun () -> 
        match cache.TryGetValue(x) with
        | true, y -> y
        | false, _ -> 
            let y = f x
            cache.Add(x, y)
            y)

