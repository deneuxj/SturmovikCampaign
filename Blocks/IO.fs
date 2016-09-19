module IO

open System
open System.IO
open SturmovikMission.DataProvider

let writeGroupFile filename group =
    use file = File.CreateText(filename)
    let groupStr =
        group
        |> McuUtil.moveEntitiesAfterOwners
        |> Seq.map (fun mcu -> mcu.AsString())
        |> String.concat "\n"
    file.Write(groupStr)
