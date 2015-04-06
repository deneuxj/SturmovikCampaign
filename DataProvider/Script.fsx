// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.
#load "Ast.fs"
#load "Parsing.fs"
#load "MissionData.fs"
#load "AutoSchema.fs"

open SturmovikMission.DataProvider

let makeTopParser types =
    let kw, types = types
    fun s ->
        match s with
        | Parsing.ReId(k, s) ->
            if kw = k then
                let (Parsing.ParserFun f) = Parsing.makeParser types
                let x, s = f s
                x
            else
                Parsing.parseError(sprintf "Not %s" kw, s)
        | _ ->
            Parsing.parseError("Not an id", s)

let parseOptions s =
    let f = makeTopParser MissionData.optionsType
    f s

let txt = """Options
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
}
"""

let data =
    try
        parseOptions (Parsing.SubString(txt, 0))
        |> Some
    with
        | :? Parsing.ParseError as e ->
            Parsing.printParseError e
            |> String.concat "\n"
            |> printfn "%s"
            None

data
|> Option.map Ast.dump
|> Option.iter (printfn "%s")

AutoSchema.getTopTypes (Parsing.SubString(txt, 0))
