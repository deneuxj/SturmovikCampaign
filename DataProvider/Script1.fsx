#load @"Scripts\load-project.fsx"

open SturmovikMission.DataProvider

let s2 = """
  {
    0 = 1;
    1 = 1;
    2 = 1;
  }"""

AutoSchema.tryParseAsMapping (Parsing.Stream.FromString s2)

let s = """
{
  Name = "Block";
  Index = 2;
  LinkTrId = 0;
  XPos = 25948.639;
  YPos = 100.656;
  ZPos = 23914.419;
  XOri = 0.00;
  YOri = 0.00;
  ZOri = 0.00;
  Model = "graphics\blocks\meh_01.mgm";
  Script = "LuaScripts\WorldObjects\Blocks\meh_01.txt";
  Country = 0;
  Desc = "";
  Durability = 25000;
  DamageReport = 50;
  DamageThreshold = 1;
  DeleteAfterDeath = 1;
  Damaged
  {
    0 = 1;
    1 = 1;
    2 = 1;
  }
}"""

let (Some(vt, _)) = AutoSchema.tryParseAsComposite (Parsing.Stream.FromString s)

let parser = Parsing.makeParser vt
let v, _ = parser.Run (Parsing.Stream.FromString s)

Ast.dump v

#I "bin\Debug"
#r "DataProvider.dll"
type T = SturmovikMissionTypes.Provider<"../data/Sample.Mission", "../data/Blocks/Blocks.Mission">
T.Date(1978, 1, 24)
let parser2 = T.Parser()
let af, _ = parser2.Parse_Airfield(Parsing.Stream.FromString "")
af.SetPlanes(T.Airfield.Planes().SetPlane([]))