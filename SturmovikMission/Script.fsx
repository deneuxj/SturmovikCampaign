// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "scripts/load-references.fsx"

open SturmovikMissionTypes
open SturmovikMission.DataProvider.Parsing

type T = Provider< @"C:\users\johann\documents\visual studio 2013\projects\sturmovikmission\data\Conquest\StalingradConquest.Mission" >

// Define your library scripting code here

let fromString s = Stream.SubString(s, 0)

let x, _ = T.Boolean.Parse(fromString "0 ")
let b = x.Value

let x2, _ = T.Date.Parse(fromString "24.01.1978")
let year = x2.Year
let month = x2.Month
let day = x2.Day

let x3, _ = T.PairOfBooleanAndBoolean.Parse(fromString "0: 1 ")
let p1, p2 = x3.Value