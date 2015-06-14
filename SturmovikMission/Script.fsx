// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "scripts/load-references.fsx"

open SturmovikMissionTypes
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Ast

type T = Provider< @"C:\users\johann\documents\visual studio 2013\projects\sturmovikmission\data\Conquest\StalingradConquest.Mission" >

let pb = T.PairOfBooleanAndBoolean(Value.Pair(Value.Boolean true, Value.Boolean false))

// Define your library scripting code here

let fromString s = Stream.SubString(s, 0)

let x, _ = T.PairOfBooleanAndBoolean.Parse(fromString "0: 1 ")
let v = x.Value