module SturmovikMission.Blocks.IO

open System
open System.IO
open SturmovikMission.DataProvider

/// <summary>
/// Write a list of MCUs into a file that can be imported as a template by the mission editor.
/// </summary>
/// <param name="filename">Name of the file to create.</param>
/// <param name="mcus">List of MCUs.</param>
let writeGroupFile filename mcus =
    use file = File.CreateText(filename)
    let groupStr =
        mcus
        |> McuUtil.moveEntitiesAfterOwners
        |> Seq.map (fun mcu -> mcu.AsString())
        |> String.concat "\n"
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
        |> McuUtil.moveEntitiesAfterOwners
        |> Seq.map (fun mcu -> mcu.AsString())
        |> String.concat "\n"
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
