module SturmovikMission.DataProvider.McuUtil

open SturmovikMission.DataProvider.Mcu
open System

/// Get an Mcu from a list by its index.
/// The mcu list must not have had its indices substituted.
let getByIndex (idx : int) (mcus : McuBase list) =
    mcus
    |> List.find (fun x -> x.Index = idx)

/// Get an entity by its index.
let getEntityByIndex idx mcus =
    mcus
    |> getByIndex idx
    :?> McuEntity

/// Get an entity owner by its index.
let getHasEntityByIndex idx mcus =
    mcus
    |> getByIndex idx
    :?> HasEntity

/// Get a command by its index.
let getCommandByIndex idx mcus =
    mcus
    |> getByIndex idx
    :?> McuCommand

/// <summary>A group of Mcus.</summary>
/// <remark>Not to be mistaken with groups in missions, with which this type has nothing to do.</remark>
type IMcuGroup =
    abstract SubGroups : IMcuGroup list
    abstract Content : McuBase list

/// Recursively yield the content of an IMcuGroup
let rec deepContentOf (gr : IMcuGroup) =
    [
        yield! gr.Content
        for sg in gr.SubGroups do
            yield! deepContentOf sg
    ]

/// Build an IMcuGroup from a list of mcus. Does not have any subgroup.
let groupFromList mcus =
    { new IMcuGroup with
        member this.Content = mcus
        member this.SubGroups = []
    }

/// Get the string representation of the content of a group and its subgroups.
let rec asString (gr : IMcuGroup) : string =
    let subStrings = gr.SubGroups |> List.map asString
    let content = gr.Content |> List.map (fun mcu -> mcu.AsString())
    String.concat "\n" (List.append subStrings content)

// Vector math
/// Create a new Vec3.
let newVec3(x, y, z) =
    let x = ref x
    let y = ref y
    let z = ref z
    { new Vec3 with
        member this.X
            with get() = !x
            and set(v) = x := v
        member this.Y
            with get() = !y
            and set(v) = y := v
        member this.Z
            with get() = !z
            and set(v) = z := v
    }

/// Rotate a point around another point in the XZ plane.
/// Angle is specified in degrees.
let rotate (center : Vec3) angle (v : Vec3) =
    let dx = v.X - center.X
    let dz = v.Z - center.Z
    let cos = Math.Cos(angle / 180.0 * Math.PI)
    let sin = Math.Sin(angle / 180.0 * Math.PI)
    let x = center.X + dx * cos - dz * sin
    let z = center.Z + dx * sin + dz * cos
    newVec3(x, v.Y, z)

/// Translate a vector. Neither v nor t is mutated, a new vector is returned.
let translate (t : Vec3) (v : Vec3) = newVec3(v.X + t.X, v.Y + t.Y, v.Z + t.Z)

/// Difference between vectorx v - w. Neither v nor w is mutated, a new vector is returned.
let vecMinus (v : Vec3) (w : Vec3) = newVec3(v.X - w.X, v.Y - w.Y, v.Z - w.Z)

/// Copy a vector src into another vector dst. The destination vector is mutated.
let vecCopy (src : Vec3) (dst : Vec3) =
    dst.X  <- src.X
    dst.Y <- src.Y
    dst.Z <- src.Z