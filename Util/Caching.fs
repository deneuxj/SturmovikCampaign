// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
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
module Util.Caching

/// Function to cache computationally expensive properties of objects.
/// Use this from a static variable, e.g. a module-level let binding.
/// Do not use from a static property, as those are evaluated every call.
let cachedProperty f =
    let cache = System.Runtime.CompilerServices.ConditionalWeakTable()
    fun this ->
        match cache.TryGetValue this with
        | false, _ ->
            let value = f this
            cache.Add(this, box value)
            value
        | true, x ->
            unbox x
