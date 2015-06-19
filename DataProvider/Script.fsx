// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#I __SOURCE_DIRECTORY__
#load @"Scripts\load-references.fsx"
#load @"Ast.fs"
      @"Unification.fs"
      @"Parsing.fs"
      @"AutoSchema.fs"

open SturmovikMission
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.AutoSchema

"""  {
    0 :     0 :     0;
    500 :     0 :     0;
    1000 :     0 :     0;
    2000 :     0 :     0;
    5000 :     0 :     0;
  }
"""
|> Stream.FromString
|> tryParseAsSet

"""{
  Index = 1226;
  Name = "Translator Subtitle";
  Desc = "";
  Targets = [];
  Objects = [];
  XPos = 127891.956;
  YPos = 143.615;
  ZPos = 240739.624;
  XOri = 0.00;
  YOri = 0.00;
  ZOri = 0.00;
  Enabled = 1;
  SubtitleInfo
  {
    Duration = 1;
    FontSize = 20;
    HAlign = 1;
    VAlign = 0;
    RColor = 255;
    GColor = 0;
    BColor = 0;
    LCText = 139;
  }
  
  Coalitions = [0, 1, 2];
}
"""
|> Stream.FromString
|> tryParseAsComposite

""" {
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
|> tryParseAsComposite
