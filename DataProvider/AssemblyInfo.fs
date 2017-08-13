//    Copyright 2016 Johann Deneux
//
//    This file is part of SturmovikMission.
//
//    SturmovikMission is free software: you can redistribute it and/or modify
//    it under the terms of the GNU Lesser General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    SturmovikMission is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public License
//    along with SturmovikMission.  If not, see <http://www.gnu.org/licenses/>.

namespace SturmovikMission

module Constants =
    [<Literal>]
    let version = "3.0.0.0"

open System.Reflection
open System.Resources;
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<assembly: AssemblyTitle("SturmovikMission.DataProvider")>]
[<assembly: AssemblyDescription("Parser for mission files from the game 'IL-2 Sturmovik: Battle Of Stalingrad' by 1CGS/777")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("SturmovikMission")>]
[<assembly: AssemblyCopyright("Copyright (c) 2016,2017 Johann Deneux")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]
[<assembly: NeutralResourcesLanguage("en")>]

[<assembly: AssemblyVersion(Constants.version)>]
[<assembly: AssemblyFileVersion(Constants.version)>]
//[<assembly: AssemblyInformationalVersion("???")>]

[<assembly: ComVisible(false)>]

()
