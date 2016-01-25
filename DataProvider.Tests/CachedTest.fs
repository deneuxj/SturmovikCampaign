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
//
//

module SturmovikMission.DataProvider.CachedTest

open FsCheck
open FsCheck.Xunit
open System.Collections.Generic

[<Property>]
let cachedIdentity x =
    let called = ref(false)
    let id = fun x -> 
        if !called then failwith "Computation should be memoized, not computed twice"
        else called := true; x
    let cache = new Dictionary<int, int>()
    Cached.cached cache id x = Cached.cached cache id x

[<Property>]
let cachedFunction x =
    let called = ref(false)
    let addOne = fun x ->
        if !called then failwith "Computation should be memoized, not computed twice"
        else called := true; x + 1
    let cache = new Dictionary<int, int>()
    Cached.cached cache addOne x = Cached.cached cache addOne x