// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SturmovikMission.Blocks
open SturmovikMission.Blocks.VirtualConvoy
open SturmovikMission.DataProvider

[<EntryPoint>]
let main argv = 
    let path =
        [ { Pos = McuUtil.newVec3(0.0, 0.0, 0.0)
            Ori = McuUtil.newVec3(1.0, 0.0, 0.0)
            Speed = 100
            Priority = 0
            Radius = 50
          }
          { Pos = McuUtil.newVec3(50.0, 0.0, 0.0)
            Ori = McuUtil.newVec3(1.0, 0.0, 0.0)
            Speed = 100
            Priority = 0
            Radius = 50
          }
        ]
    let path = Strategy.getSomePath()

    let store = NumericalIdentifiers.IdStore()
    let virtualConvoy = VirtualConvoy.Create(store, path, 3)
    let rel = virtualConvoy.CreateLinks()
    let mcus = McuUtil.deepContentOf virtualConvoy
    rel.Apply(mcus)
    IO.writeGroupFile "VirtualConvoy.group" mcus
    0 // return an integer exit code
