//    Copyright 2017 Johann Deneux
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

module SturmovikMission.DataProvider.McuOutput

open System.IO

/// MCUs organized in groups
type Hierarchy =
    | Flat of Mcu.McuBase
    | Group of (string * int) * Hierarchy list

/// Build a hierarchy from the paths of MCUs. The paths are all set to the empty list upon return.
let mkHierarchy (mcus : Mcu.McuBase list) =
    let rec work (mcus : Mcu.McuBase list) =
        let flat, nested = mcus |> List.partition (fun mcu -> mcu.Path.IsEmpty)
        [
            yield! flat |> McuUtil.moveEntitiesAfterOwners |> List.map Flat
            for grp in nested |> List.groupBy (fun mcu -> mcu.Path.Head) do
                let name, members = grp
                for m in members do
                    m.Path <- m.Path.Tail
                yield Group(name, work members)
        ]
    for m in mcus do
        m.Path <- List.rev m.Path
    work mcus

/// Get the string representation of a list of MCUs where groups have been recreated.
let getAsStringWithHierarchy (mcus : Mcu.McuBase list) =
    let rec work h =
        match h with
        | Flat m -> m.AsString()
        | Group((name, idx), hs) ->
            hs
            |> List.map work
            |> String.concat "\n"
            |> sprintf "Group {\nName = \"%s\";\nIndex = %d;\nDesc = \"\";\n%s\n}" name idx
    mcus
    |> mkHierarchy
    |> List.map work
    |> String.concat "\n"

/// <summary>
/// Write a list of MCUs into a file that can be imported as a template by the mission editor.
/// </summary>
/// <param name="filename">Name of the file to create.</param>
/// <param name="mcus">List of MCUs.</param>
let writeGroupFile filename mcus =
    use file = File.CreateText(filename)
    let groupStr =
        mcus
        |> getAsStringWithHierarchy
    file.Write(groupStr)

/// <summary>
/// Write a localization file.
/// </summary>
/// <param name="filename">Name of the file</param>
/// <param name="allLcStrings">Sequence of index and localized string pairs.</param>
let writeLcFile filename allLcStrings =
    use file = new StreamWriter(filename, false, System.Text.UnicodeEncoding(false, true))
    for (idx, s) in allLcStrings do
        file.WriteLine(sprintf "%d:%s" idx s)

/// <summary>
/// Write a group file with its list of localized strings.
/// </summary>
/// <param name="lang">Language, e.g. "eng" for english</param>
/// <param name="groupFilename">Name of the group file, including path.</param>
/// <param name="groups">Groups of MCUs to write out.</param>
let writeLocalizedGroupFiles lang groupFilename (groups : McuUtil.IMcuGroup seq) =
    let dir = Path.GetDirectoryName(groupFilename)
    let basename = Path.GetFileNameWithoutExtension(groupFilename)
    let groupFile = Path.Combine(dir, Path.ChangeExtension(basename, "group"))
    writeGroupFile groupFile (groups |> Seq.map McuUtil.deepContentOf |> List.concat)
    let lcFile = Path.Combine(dir, Path.ChangeExtension(basename, lang))
    writeLcFile lcFile (groups |> Seq.map McuUtil.deepLcStrings |> List.concat)

/// <summary>
/// Write a text mission file.
/// </summary>
/// <param name="filename">Name of the mission file</param>
/// <param name="options">Options header, containing the mission weather, time, map...</param>
/// <param name="mcus">List of MCUs to write in the file. Those are written in a flat manner, ignoring the group hierarchy.</param>
let inline writeMissionFile filename (options : ^T) mcus =
    use file = File.CreateText(filename)
    let optionsStr = (^T : (member AsString : unit -> string) options)
    let groupStr =
        mcus
        |> getAsStringWithHierarchy
    file.WriteLine("# Mission File Version = 1.0;")
    file.WriteLine(optionsStr)
    file.Write(groupStr)
    file.Write("# end of file")

/// <summary>
/// Write a text mission and its localized strings.
/// </summary>
/// <param name="lang">Language, e.g. "eng" for english.</param>
/// <param name="missionFilename">Name of the mission file, including path</param>
/// <param name="options">Options header, containing the mission weather, time, map...</param>
/// <param name="groups">Groups of MCUs to write out.</param>
let inline writeMissionFiles lang missionFilename (options : ^T) (groups : McuUtil.IMcuGroup seq) =
    let dir = Path.GetDirectoryName(missionFilename)
    let basename = Path.GetFileNameWithoutExtension(missionFilename)
    let missionFile = Path.Combine(dir, Path.ChangeExtension(basename, "mission"))
    writeMissionFile missionFile options (groups |> Seq.map McuUtil.deepContentOf |> List.concat)
    let lcFile = Path.Combine(dir, Path.ChangeExtension(basename, lang))
    writeLcFile lcFile (groups |> Seq.map McuUtil.deepLcStrings |> List.concat)
