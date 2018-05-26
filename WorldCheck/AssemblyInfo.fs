namespace ConsoleApplication1.AssemblyInfo

open System.Reflection
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open SturmovikMission.Blocks.AssemblyInfo

module Constants =
    [<Literal>]
    let version = "1.0.0.0"

// General Information about an assembly is controlled through the following
// set of attributes. Change these attribute values to modify the information
// associated with an assembly.
[<assembly: AssemblyTitle("WorldCheck")>]
[<assembly: AssemblyDescription("Check campaign mission files")>]
[<assembly: AssemblyConfiguration("")>]
[<assembly: AssemblyCompany("")>]
[<assembly: AssemblyProduct("")>]
[<assembly: AssemblyCopyright("Copyright © Johann Deneux 2018")>]
[<assembly: AssemblyTrademark("")>]
[<assembly: AssemblyCulture("")>]

// Setting ComVisible to false makes the types in this assembly not visible
// to COM components.  If you need to access a type in this assembly from
// COM, set the ComVisible attribute to true on that type.
[<assembly: ComVisible(false)>]

// The following GUID is for the ID of the typelib if this project is exposed to COM
[<assembly: Guid("b91591ac-bbfb-4665-a935-e6ff3308040f")>]

// Version information for an assembly consists of the following four values:
//
//       Major Version
//       Minor Version
//       Build Number
//       Revision
//
// You can specify all the values or you can default the Build and Revision Numbers
// by using the '*' as shown below:
// [<assembly: AssemblyVersion("1.0.*")>]
[<assembly: AssemblyVersion(Constants.version)>]
[<assembly: AssemblyFileVersion(Constants.version)>]

do
    ()