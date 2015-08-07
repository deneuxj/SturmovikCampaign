// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open SturmovikMission.DataProvider

open SturmovikMission
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.AutoSchema

[<EntryPoint>]
let main argv =
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
    |> printfn "%A"
    0