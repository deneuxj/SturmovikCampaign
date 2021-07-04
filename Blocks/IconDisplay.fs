module SturmovikMission.Blocks.IconDisplay

open SturmovikMission.DataProvider
open System.Numerics
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.DataProvider.McuUtil
open VectorExtension

type IconDisplay = {
    Show : Mcu.McuTimer
    Hide : Mcu.McuTrigger
    Icon : Mcu.McuIcon
    All : McuUtil.IMcuGroup
}
with
    /// Create an icon that can be shown or hidden
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, label : string, coalition : Mcu.CoalitionValue, iconType : Mcu.IconIdValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let substLc = Mcu.substLCId <| lcStore.GetIdMapper()
        let group = blocksData.GetGroup("Icon").CreateMcuList()
        for mcu in group do
            subst mcu
            substLc mcu
        // Get key nodes
        let show = getTriggerByName group "Show" :?> Mcu.McuTimer
        let hide = getTriggerByName group "Hide"
        let activate = getByIndex show.Targets.Head group :?> Mcu.McuTrigger
        let icon = getIconByIndex activate.Targets.Head group
        // Position of all nodes
        let refPos = Vector2.FromMcu icon.Pos
        let dv = pos - refPos
        for mcu in group do
            (Vector2.FromMcu mcu.Pos + dv).AssignTo(mcu.Pos)
        // Icon type
        icon.Coalitions <- [ coalition ]
        icon.IconId <- iconType
        { Show = show
          Hide = hide
          Icon = icon
          All =
            { new McuUtil.IMcuGroup with
                member x.Content = group
                member x.LcStrings = [ icon.IconLC.Value.LCName, label ]
                member x.SubGroups = []
            }
        }

    /// <summary>
    /// Create a pair of colocated icons, visible to each coalition as attack/cover
    /// </summary>
    /// <param name="store">ID store</param>
    /// <param name="lcStore">String ID store</param>
    /// <param name="pos">Position</param>
    /// <param name="label">Icon label</param>
    /// <param name="coalition">The coalition for which the icon should appear as friendly</param>
    /// <param name="iconType">The icon type, should be of one of the CoverXXX types</param>
    static member CreatePair(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, label : string, coalition : Mcu.CoalitionValue, iconType : Mcu.IconIdValue) =
        let one = IconDisplay.Create(store, lcStore, pos, label, coalition, iconType)
        let other =
            match coalition with
            | Mcu.CoalitionValue.Allies -> Mcu.CoalitionValue.Axis
            | Mcu.CoalitionValue.Axis -> Mcu.CoalitionValue.Allies
            | _ -> invalidArg "coalition" "Must be Axis or Allies"
        let two = IconDisplay.Create(store, lcStore, pos, label, other, enum((int iconType) - 50))
        one, two