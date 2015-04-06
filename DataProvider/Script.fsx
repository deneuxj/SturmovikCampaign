// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "Ast.fs"
#load "Parsing.fs"
#load "MissionData.fs"
#load "AutoSchema.fs"

open SturmovikMission.DataProvider

let txt = """MCU_Icon
{
  Index = 116;
  Targets = [];
  Objects = [];
  XPos = 152312.206;
  YPos = 33.004;
  ZPos = 204562.250;
  XOri = 0.00;
  YOri = 0.00;
  ZOri = 0.00;
  Enabled = 1;
  LCName = 3;
  LCDesc = 4;
  IconId = 0;
  RColor = 255;
  GColor = 255;
  BColor = 255;
  LineType = 0;
  Coalitions = [1, 2];
}
"""

let s = Parsing.SubString(txt, 0)

try
    AutoSchema.getTopType Map.empty s
with
| :? Parsing.ParseError as e ->
    Parsing.printParseError e
    |> String.concat "\n"
    |> printfn "%s"
    failwith "ParseError"

let dir = @"C:\users\johann\documents\visual studio 2013\projects\sturmovikmission"
let conquest = @"data\Conquest\StalingradConquest.Mission"
let path = System.IO.Path.Combine(dir, conquest)

let data = Parsing.Stream.FromFile(path)
AutoSchema.getTopTypes data
