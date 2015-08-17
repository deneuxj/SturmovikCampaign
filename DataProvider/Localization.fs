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

module SturmovikMission.DataProvider.Localization

open System.IO

/// <summary>
/// Transfer localized strings from a file to a new list with updated identifiers.
/// </summary>
/// <param name="includeMission">If true, include entries for identifiers 0 (mission name), 1 (briefing), 2 (author). Otherwise, skip those entries.</param>
/// <param name="getLcId">Mapping from old numerical identifiers to new ones.</param>
/// <param name="path">Path to the localization file.</param>
let transfer includeMission (getLcId : int -> int) path =
    let lines = File.ReadLines(path)
    [
        for line in lines do
            let idx = line.IndexOf ":"
            if idx <> -1 then
                match System.Int32.TryParse(line.[0..idx - 1]) with
                | true, num ->
                    if num > 2 || includeMission then
                        let num = getLcId num
                        yield (num, line.Substring(idx + 1))
                | false, _ ->
                    ()
    ]    