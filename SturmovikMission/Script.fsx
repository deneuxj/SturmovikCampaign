// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "scripts/load-references.fsx"

open System.Collections.Generic
open SturmovikMissionTypes
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Ast

type T = Provider< @"C:\users\johann\documents\visual studio 2013\projects\sturmovikmission\data\Conquest\StalingradConquest.Mission" >

let groupData =
    try
        T.GroupData(Stream.FromFile @"C:\users\johann\documents\visual studio 2013\projects\sturmovikmission\data\Conquest\StalingradConquest.Mission")
    with
    | :? ParseError as e ->
        printParseError(e) |> String.concat "\n" |> printfn "%s"
        raise e

groupData.ListOfMCU_Timer
groupData.ListOfAirfield

let pair = T.PairOfIntegerAndInteger((T.Integer(1), T.Integer(2)))
pair.Value

let parser = T.Parser()

let x2, _ =
    try
        """
    {
      LCName = 0;
      LCDesc = 1;
      LCAuthor = 2;
      PlayerConfig = "";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\bf109f4.txt";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\bf109g2.txt";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\fw190a3.txt";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\he111h6.txt";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\ju87d3.txt";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\la5s8.txt";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\lagg3s29.txt";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\pe2s87.txt";
      MultiplayerPlaneConfig = "LuaScripts\WorldObjects\Planes\yak1s69.txt";
      Time = 8:0:0;
      Date = 19.11.1942;
      HMap = "graphics\LANDSCAPE_Stalin_w\height.hini";
      Textures = "graphics\LANDSCAPE_Stalin_w\textures.tini";
      Forests = "graphics\LANDSCAPE_Stalin_w\trees\woods.wds";
      Layers = "";
      GuiMap = "stalingrad-1942";
      SeasonPrefix = "wi";
      MissionType = 2;
      AqmId = 0;
      CloudLevel = 500;
      CloudHeight = 200;
      PrecLevel = 0;
      PrecType = 0;
      CloudConfig = "00_clear_00\sky.ini";
      SeaState = 0;
      Turbulence = 0;
      TempPressLevel = 0;
      Temperature = -15;
      Pressure = 760;
      WindLayers
      {
        0 :     0 :     0;
        500 :     0 :     0;
        1000 :     0 :     0;
        2000 :     0 :     0;
        5000 :     0 :     0;
      }
      Countries
      {
        0 : 0;
        101 : 1;
        201 : 2;
        202 : 2;
      }
    } """
        |> Stream.FromString
        |> parser.Parse_Options
    with
    | :? ParseError as e ->
        printParseError(e) |> String.concat "\n" |> printfn "%s"
        raise e

let x3, _ =
    try
        """{
  Name = "Block";
  Index = 4;
  LinkTrId = 0;
  XPos = 159106.566;
  YPos = 102.203;
  ZPos = 263544.937;
  XOri = 0.00;
  YOri = 0.00;
  ZOri = 0.00;
  Model = "graphics\blocks\arf_tower_2.mgm";
  Script = "LuaScripts\WorldObjects\Blocks\arf_tower_2.txt";
  Country = 0;
  Desc = "";
  Durability = 25000;
  DamageReport = 50;
  DamageThreshold = 1;
  DeleteAfterDeath = 1;
}"""
        |> Stream.FromString
        |> parser.Parse_Block
    with
    | :? ParseError as e ->
        printParseError(e) |> String.concat "\n" |> printfn "%s"
        raise e

x3.SetDamaged(T.Damaged().SetItem(1, T.Boolean(true)) |> Some)

let x4, _ =
    try
        """
{
  Index = 1349;
  Name = "Translator Subtitle";
  Desc = "";
  Targets = [];
  Objects = [];
  XPos = 127899.280;
  YPos = 143.615;
  ZPos = 242019.378;
  XOri = 0.00;
  YOri = 0.00;
  ZOri = 0.00;
  Enabled = 1;
  SubtitleInfo
  {
    Duration = 30;
    FontSize = 40;
    HAlign = 1;
    VAlign = 0;
    RColor = 0;
    GColor = 0;
    BColor = 255;
    LCText = 142;
  }
  
  Coalitions = [0, 1, 2];
}
"""
        |> Stream.FromString
        |> parser.Parse_MCU_TR_Subtitle
    with
    | :? ParseError as e ->
        printParseError(e) |> String.concat "\n" |> printfn "%s"
        raise e

x4.SetEnabled(T.Boolean(true)).Enabled.Value