﻿module SturmovikMission.Blocks.MapGraphics

open System.Numerics

open BlocksMissionData
open SturmovikMission.DataProvider
open VectorExtension

/// Default icon from which all icons are cloned.
let private defaultIcon =
    let lcDesc = 1
    let lcName = 2
    T.MCU_Icon.Default
        .SetCoalitions(T.VectorOfIntegers.N [1;2])
        .SetEnabled(T.Boolean.N true)
        .SetIndex(T.Integer.N 1)
        .SetLCDesc(T.Integer.N lcDesc)
        .SetLCName(T.Integer.N lcName)

/// Make an icon.
let mkIcon (store : NumericalIdentifiers.IdStore) (lcStore : NumericalIdentifiers.IdStore) (lineType : int) (red, green, blue) (v : Vector2) =
    let subst = Mcu.substId <| store.GetIdMapper()
    let substLc = Mcu.substLCId <| lcStore.GetIdMapper()
    let mcu =
        defaultIcon
            .SetXPos(T.Float.N (float v.X))
            .SetZPos(T.Float.N (float v.Y))
            .SetLineType(T.Integer.N lineType)
            .SetRColor(T.Integer.N red)
            .SetGColor(T.Integer.N green)
            .SetBColor(T.Integer.N blue)
            .CreateMcu()
            :?> Mcu.McuIcon
    subst mcu
    substLc mcu
    mcu

/// Make two icons connected by a line.
let mkSegmentIcons mkIcon (segment : Vector2 * Vector2) =
    let icon1 : Mcu.McuIcon = mkIcon(fst segment)
    let icon2 = mkIcon(snd segment)
    icon1.Targets <- icon2.Index :: icon1.Targets
    [ icon1; icon2 ]

/// Make an arrow
let renderArrow
    (store, lcStore, coalitions)
    (start : Vector2, tip : Vector2, width : float32, headAngle : float32, color) =
    let dir =
        let x = (tip - start)
        x / x.Length()
    let points =
        let mkIcon =
            mkIcon store lcStore (int Mcu.LineTypeValue.Attack) color
            >> fun icon -> icon.Coalitions <- coalitions; icon
        [
            yield mkIcon (start + width * dir.Rotate(90.0f))
            yield mkIcon start
            yield mkIcon tip
            yield mkIcon (tip + width * dir.Rotate(90.0f + headAngle))
        ]
    for p1, p2 in Seq.pairwise points do
        p1.Targets <- [p2.Index]
    points

/// Groups icons and their labels.
type MapIcons = {
    Show : Mcu.McuTrigger option
    Hide : Mcu.McuTrigger option
    All : Mcu.McuBase list
    LcStrings : (int * string) list
}
with
    interface McuUtil.IMcuGroup with
        member x.Content = x.All
        member x.LcStrings = x.LcStrings
        member x.SubGroups = []

    /// <summary>
    /// Create an instance from an existing list of icons, where all locale strings are empty.
    /// </summary>
    static member FromIcons(icons : Mcu.McuIcon list) =
        let lcStrings =
            [
                for icon in icons do
                    match icon.IconLC with
                    | Some data ->
                        yield (data.LCDesc, "")
                        yield (data.LCName, "")
                    | None ->
                        ()
            ]
        { Show = None
          Hide = None
          All = icons |> List.map (fun x -> upcast x)
          LcStrings = lcStrings
        }

    /// <summary>
    /// Make the icons hidden by default, and add a trigger that shows them.
    /// Note that the returned instance of MapIcons and the original one share the same icons, which are mutated by this call.
    /// </summary>
    member this.AddShow(store : NumericalIdentifiers.IdStore) =
        let subst = Mcu.substId <| store.GetIdMapper()
        let show = newActivate 1
        subst show
        show.Targets <- this.All |> List.map (fun mcu -> mcu.Index)
        for mcu in this.All do
            match mcu with
            | :? Mcu.McuIcon as icon ->
                icon.Enabled <- false
            | _ ->
                ()
        { this with
            Show = Some show
            All = (upcast show) :: this.All
        }
