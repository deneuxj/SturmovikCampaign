# README #

A set of F# libraries to parse and manipulate mission files in IL-2: Battle of Stalingrad.

## Current status ##

Nearing usability, but not yet used.

## Code Architecture ##

All the interesting code is in project DataProvider, whose files can be grouped in four layers:

* Support code: ProvidedTypes.fsi, ProvidedTypes.fs, Cached.fs

* Parsing that produces a dynamically typed AST: Ast.fs, Parsing.fs, Unification.fs, AutoSchema.fs

* Type provider on top of the parsing modules: TypeProvider.fs

* Utilities to facilitate manipulation of parsed data: Mcu.fs, McuFactory.fs, NumericalIdentifiers.fs

In the list above, layers mentioned early are used by layers mentioned later

## How do I get set up? ##

Build with Visual Studio 2013. Dependencies managed with nuget (FSharp.TypeProviders.StarterPack).

## How do I use this to build missions? ##

You can use the parsing layer with the manipulation layer directly, or through a type provider.

### Using direct access to the Abstract Syntax Tree ###

First, you will need to extract data schema from a sample file, and then build the parser for the mission or group files you will be working with.

```
#!fsharp
let types, _ = AutoSchema.getTopTypes (Parsing.Stream.FromFile "Sample.mission")
let parsers = types |> Map.map (fun name typ -> Parsing.makeParser typ)
```

Parse data from a mission file:

```
#!fsharp
let data = Parsing.parseFile (fun name -> parsers.[name]) (Parsing.Stream.FromFile "MyMission.mission"
```

At this point, you have mission data represented in trees of type Ast.Value, which are immutable. You can combine them in any way you see fit, producing new trees. Every tree can be dumped as a string that conforms to the mission file format:

```
#!fsharp
let value : Ast.Value = ...
let s = Ast.dump value
```

Instances of Ast.Value are dynamically typed, which means that of you attempt to use a field that does not exist or has the wrong type, you will discover your error first when you run your mission-building code (if you are lucky). To help with this issue, a type provider is available to convey compile and edit-time type safety.

### Using the type provider ###

The type provider accepts two string arguments: the path to a sample mission file, and a list of paths to mission files separated by semi-colons ";". The second argument can be the empty string. The first argument can be a relative path, in which case it will be relative to DataProvider.dll. If you plan to use FSI, you will unfortunately have to use an absolute path, as FSI copies DaraProvider.dll to a temporary directory, moving it away from the sample mission file you intended to use.

```
#!fsharp
type T = Provider< @"C:\Users\...\Sample.Mission", @"C:\Users\...\m2.Mission" >
```

At this point, T.Parser is a type that can be used to parse bits of mission files and strings. For instance, the code below creates an airfield tower as an instance of T.Block.

```
#!fsharp
let parser = T.Parser()
let x3, _ =
    try
        """{
  Name = "My Tower";
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
```

You can also import a mission into the type T. In the type alias example above, a mission "m2.mission" is imported. Be careful with that feature, and keep it to small files, as it can make your editing experience in Visual Studio rather sluggish. A limitation is that you may not import two missions with the same name, as the name is used for the type name.

For example, assuming I have an object name "Approach beacon" in "m2.mission":
```
#!fsharp
let beacon = T.m2.``Approach beacon``
```

Reading in an entire file is done with T.GroupData. The data it reads can be sorted by their type, and accessed in lists, one list per type:

```
#!fsharp
let groupData =
    try
        T.GroupData(Stream.FromFile @"C:\Users\...\StalingradConquest.Mission")
    with
    | :? ParseError as e ->
        printParseError(e) |> String.concat "\n" |> printfn "%s"
        raise e

let timers = groupData.ListOfMCU_Timer |> List.map...
groupData.ListOfAirfield |> List.iter...
```

Working with data with segregated types might be impractical, especially in a statically-typed language such as F#. There is a family of interfaces that provide access to most objects (commands, blocks, buildings, vehicles, planes...) in a mission. These interfaces are found in module SturmovikMission.DataProvider.Mcu. Instances can be created from dynamically typed instances of Ast.Value using SturmovikMission.DataProvider.McuFactory, or from the provided types. The two examples below illustrates the second alternative.

First, from GroupData:
```
#!fsharp
groupData.AsMcuList |> List.head |> fun x -> x.AsString()
```

Secondly, from an imported mission:

```
#!fsharp
let mcus = T.m2.AsMcuList
let rabbit =
    mcus
    |> List.pick (function :? McuEntity as ent -> Some ent | _ -> None)

rabbit.OnEvents <- [ { Type = 2; TarId = 123 }; { Type = 3; TarId = 456 } ]
rabbit.Name <- "Rabbit"
rabbit.Pos.Z <- -1.0
rabbit.Ori.X <- 0.1
printfn "%s" (rabbit.AsString())
```

This produced the output below:

```
#!verbatim
{
Index = 1759;
Name = "Rabbit";
Desc = "";
Targets = [1729];
Objects = [];
XPos = 43641.188000;
YPos = 84.329000;
ZPos = -1.000000;
XOri = 0.100000;
YOri = 0.000000;
ZOri = 0.000000;
Enabled = 1;
MisObjID = 1758;
OnEvents
{
OnEvent
{
Type = 2;
TarId = 123;
}
OnEvent
{
Type = 3;
TarId = 456;
}
}
}
```