// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open SturmovikMission.Blocks
open SturmovikMission.Blocks.VirtualConvoy.Factory
open SturmovikMission.DataProvider

[<EntryPoint>]
let main argv = 
    let path = Strategy.getSomePath()
    let store = NumericalIdentifiers.IdStore()
    let virtualConvoy = VirtualConvoy.Create(store, path, 3, Mcu.CountryValue.Russia, Mcu.CoalitionValue.Allies)
    let rel = virtualConvoy.CreateLinks()
    let mcus = McuUtil.deepContentOf virtualConvoy
    rel.Apply(mcus)
    IO.writeGroupFile "VirtualConvoy.group" mcus
    0 // return an integer exit code
