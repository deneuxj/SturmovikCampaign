module SturmovikMission.Blocks.IO

open System
open System.IO
open SturmovikMission.DataProvider

/// <summary>
/// Write a list of MCUs into a file that can be imported as a template by the mission editor.
/// </summary>
/// <param name="filename">Name of the file to create.</param>
/// <param name="group">List of MCUs.</param>
let writeGroupFile filename group =
    use file = File.CreateText(filename)
    let groupStr =
        group
        |> McuUtil.moveEntitiesAfterOwners
        |> Seq.map (fun mcu -> mcu.AsString())
        |> String.concat "\n"
    file.Write(groupStr)
