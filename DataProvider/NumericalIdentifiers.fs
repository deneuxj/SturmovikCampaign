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


module SturmovikMission.DataProvider.NumericalIdentifiers

/// <summary>
/// Class providing mapping from old numerical identifiers to new identifiers, avoiding collisions.
/// </summary>
type IdStore() = 
    let mutable nextId = 1
    let mappings = ref []
    member this.SetNextId(id) = nextId <- id
    
    /// <summary>
    /// Get a function that checks if its input is present in an internal mapping, and if so returns it.
    /// Otherwise, retrieve nextId from the IdStore and return it after adding it to the internal mapping.
    /// </summary>
    member this.GetIdMapper() = 
        let m = ref Map.empty
        lock this (fun () -> mappings := m :: !mappings)
        fun (x : int) -> 
            match Map.tryFind x !m with
            | Some n -> n
            | None -> 
                let n = System.Threading.Interlocked.Increment(&nextId) - 1
                m := Map.add x n !m
                n
    
    /// <summary>
    /// Get a mapping from an old id to its set of new ids, built from all the internal mappings created in calls to GetIdMapper.
    /// </summary>
    member this.GetMappings() = 
        let mappings = lock this (fun () -> !mappings)
        mappings |> List.fold (fun M m -> 
                        !m |> Map.fold (fun M x n -> 
                                  let prev = defaultArg (Map.tryFind x M) Set.empty
                                  Map.add x (Set.add n prev) M) Map.empty) Map.empty