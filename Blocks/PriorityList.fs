/// Priority list mission logic
module SturmovikMission.Blocks.PriorityList

open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.BlocksMissionData
open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider

/// A node in the list
type Node = {
    Do : Mcu.McuTrigger
    Pass : Mcu.McuTrigger
    Try : Mcu.McuTrigger
    Enable : Mcu.McuTrigger
    Disable : Mcu.McuTrigger
    All : IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("PriorityNode").CreateMcuList()
        for mcu in group do
            subst mcu
        // Get key nodes
        let act = getTriggerByName group T.Blocks.Do
        let pass = getTriggerByName group T.Blocks.Pass
        let attempt = getTriggerByName group T.Blocks.Try
        let enable = getTriggerByName group T.Blocks.Enable
        let disable = getTriggerByName group T.Blocks.Disable
        // Position of all nodes
        let refPoint = Vector2.FromMcu act.Pos
        let dv = pos - refPoint
        for mcu in group do
            (Vector2.FromMcu(mcu.Pos) + dv).AssignTo(mcu.Pos)
        // result
        { Do = act
          Pass  = pass
          Try = attempt
          Enable = enable
          Disable = disable
          All = groupFromList group
        }

/// An ordered list of nodes.
// When Try is triggered, Try in the first node is triggered.
// If it's enabled, it triggers its Do and disables itself.
// Otherwise, Pass is triggered, which triggers Try in the next node, and so on...
type NodeList = {
    Nodes : Node list
    Try : Mcu.McuTrigger
    All : McuUtil.IMcuGroup
}
with
    static member Create(store, positions : Vector2 list) =
        let nodes = positions |> List.map (fun pos -> Node.Create(store, pos))
        let subst = Mcu.substId <| store.GetIdMapper()
        let attempt = newTimer 1
        subst attempt
        match nodes with
        | hd :: _ ->
            Mcu.addTargetLink attempt hd.Try.Index
        | [] ->
            ()
        for (prev, next) in Seq.pairwise nodes do
            Mcu.addTargetLink prev.Pass next.Try.Index
        { Nodes = nodes
          Try = attempt
          All =
            { new IMcuGroup with
                  member x.Content = [ attempt ]
                  member x.LcStrings = []
                  member x.SubGroups = nodes |> List.map (fun node -> node.All)
            }
        }

