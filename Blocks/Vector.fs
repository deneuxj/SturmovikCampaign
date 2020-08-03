module VectorExtension

open System.Numerics
open SturmovikMission.DataProvider

type Vector2
with
    static member FromMcu(mcuPos : Mcu.Vec3) =
        Vector2(float32 mcuPos.X, float32 mcuPos.Z) 

    member this.AssignTo(dst : Mcu.Vec3) =
        dst.X <- float this.X
        dst.Z <- float this.Y
