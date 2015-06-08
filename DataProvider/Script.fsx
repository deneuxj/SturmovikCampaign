// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "scripts/load-references.fsx"

#r "bin/Debug/DataProvider.dll"

open SturmovikMission

type T = Provider< @"C:\users\johann\documents\visual studio 2013\projects\sturmovikmission\data\Conquest\StalingradConquest.Mission">
