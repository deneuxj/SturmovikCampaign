// Learn more about F# at http://fsharp.net. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "scripts/load-references.fsx"

open System.Collections.Generic
open SturmovikMissionTypes
open SturmovikMission.DataProvider.Parsing
open SturmovikMission.DataProvider.Ast
open SturmovikMission.DataProvider.Mcu
open SturmovikMission.DataProvider.NumericalIdentifiers

type T = Provider< @"C:\Users\johann\Documents\Visual Studio 2013\Projects\sturmovikmission\data\Sample.Mission", @"C:\Users\johann\Documents\Visual Studio 2013\Projects\sturmovikmission\data\vluki-rescue\vluki-rescue.Mission" >

// Id store for MCUs, entities, objects...
let itemIds = IdStore()
// Id store for localized strings
let localeIds = IdStore()
// Reserve ids 0, 1 and 2 for LCName, LCDesc and LCAuthor in the mission options
let lcOptionsMapper = localeIds.GetIdMapper()
for id in 0..2 do lcOptionsMapper(id) |> ignore

// Shorthand for the imported mission type
type Vluki = T.``vluki-rescue``

// Shorthand for the mission data read at runtime
let vluki = T.GroupData(Stream.FromFile @"C:\Users\johann\Documents\Visual Studio 2013\Projects\sturmovikmission\data\vluki-rescue\vluki-rescue.Mission")

// Given a member in a graph of connected entities, objects and commands, return the graph.
let getGroup (isExcluded : McuBase -> bool) (root : McuBase) (all : McuBase list) =
    let visited = HashSet<McuBase>()
    let rec work (working : McuBase list) =
        match working with
        | [] -> ()
        | item :: rest ->
            if not(isExcluded(item) || visited.Contains(item)) then
                visited.Add(item) |> ignore
                // In the comments below, "an" refers the thing in all, "the" refers to "item" from working.
                let dependents =
                    all
                    |> List.filter(function
                        | :? HasEntity as owner ->
                            // Retain an owner of the entity
                            owner.LinkTrId = item.Index
                        | :? McuEntity as entity ->
                            // Retain an entity of the object
                            entity.MisObjID = item.Index
                            // Retain a wing of the entity
                            || entity.Targets |> List.exists ((=) item.Index)
                            // Retain an entity that targets the command through an event
                            || entity.OnEvents |> List.exists (fun ev -> ev.TarId = item.Index)
                            // retain an entity that targets the command through a command report
                            || entity.OnReports |> List.exists (fun rep -> rep.TarId = item.Index)
                        | :? McuCommand as cmd ->
                            // retain a command that has the entity
                            cmd.Objects |> List.exists ((=) item.Index)
                            // retain a command that targets the entity or the command
                            || cmd.Targets |> List.exists ((=) item.Index)
                        | _ -> false
                    )
                work (dependents @ rest)
            else
                work rest
    work [root]
    visited

// Get an Mcu from a list by its index.
// The mcu list must not have had its indices substituted.
let getByIndex (idx : T.Integer) (mcus : McuBase list) =
    let idx = idx.GetInteger()
    mcus
    |> List.find (fun x -> x.Index = idx)

let getEntityByIndex idx mcus =
    mcus
    |> getByIndex idx
    :?> McuEntity

let getCommandByIndex idx mcus =
    mcus
    |> getByIndex idx
    :?> McuCommand

// An instantiated group of tanks. Lists the items we need direct access to, and the rest that must also be instantiated (e.g. waypoints).
type TankGroup = {
    LeadEntity : McuEntity
    StartWaypoint : McuCommand
    Stop : McuCommand
    Continue : McuCommand
    Activate : McuCommand
    All : McuBase list
}
with
    static member Create() =
        // Make instances of McuBase and its subtypes
        let all = vluki.AsMcuList
        // Extract the leader's entity, which is used to get the rest of the group.
        let leadEntity = getEntityByIndex (Vluki.``Panzer leader``.LinkTrId) all
        let group =
            all
            |> getGroup (fun _ -> false) leadEntity
            |> List.ofSeq
        // Grab the components of TankGroup from group
        let startWp = getCommandByIndex Vluki.wp0.Index group
        let stopCmd = getCommandByIndex Vluki.stop.Index group
        let contCmd = getCommandByIndex Vluki.``continue``.Index group
        let activateCmd = getCommandByIndex Vluki.``activate group``.Index group
        // The instantiated group must have its own indexes, that do not collide with other groups.
        let getItemId = itemIds.GetIdMapper()
        for x in all do substId getItemId x
        // We do the same for localized strings, but that might not be needed if all groups use identical strings. This is mostly relevant for subtitles and map icons.
        let getLcId = localeIds.GetIdMapper()
        for x in group do substLCId getLcId x
        // Result
        { LeadEntity = leadEntity
          StartWaypoint = startWp
          Stop = stopCmd
          Continue = contCmd
          Activate = activateCmd
          All = List.ofSeq group
        }

    member this.AsString() =
        this.All
        |> List.map (fun x -> x.AsString())
        |> String.concat "\n"

// Multiple tank groups following each other.
type TankPlatoon = {
    StartAll : McuCommand
    Groups : TankGroup list
    Glue : McuBase list
}
with
    static member Create(numGroups : int) =
        let groups =
            List.init numGroups (fun _ -> TankGroup.Create())
        let consecutive = Seq.pairwise groups
        // When group i reaches wp0, activate group i+1
        for (group1, group2) in consecutive do
            addTargetLink group1.StartWaypoint group2.Activate.Index        
        // If group i+1 gets too close to group i, stop group i+1
        // Create a proximity trigger for each consecutive pair of groups.
        let proximities =
            groups
            |> Seq.pairwise
            |> Seq.mapi (fun i _ ->
                Vluki.proximity
                    .SetName(T.String("Too close"))
                    .SetCloser(T.Boolean(true))
                    .SetDistance(T.Integer(200))
                    .SetIndex(T.Integer(i))
                    .AsCommand())
            |> List.ofSeq
        // Give individual ids to each trigger
        let getNewId = itemIds.GetIdMapper()
        for p in proximities do substId getNewId p
        // Connect triggers to the leader of each group and to the stop command
        for p, (gr1, gr2) in Seq.zip proximities consecutive do
            addObjectLink p gr1.LeadEntity.Index
            addObjectLink p gr2.LeadEntity.Index
            addTargetLink p gr2.Stop.Index
            // Start the trigger when the second group spawns
            addTargetLink gr2.Activate p.Index
        // Similarly, a "further" proximity trigger to get tanks rolling again
        let further =
            groups
            |> Seq.pairwise
            |> Seq.mapi (fun i _ ->
                Vluki.proximity
                    .SetName(T.String("Far enough"))
                    .SetCloser(T.Boolean(false))
                    .SetDistance(T.Integer(250))
                    .SetIndex(T.Integer(i))
                    .AsCommand())
            |> List.ofSeq
        // Give individual ids to each trigger
        let getNewId = itemIds.GetIdMapper()
        for p in further do substId getNewId p
        // Connect triggers to the leader of each group and to the continue command
        for p, (gr1, gr2) in Seq.zip further consecutive do
            addObjectLink p gr1.LeadEntity.Index
            addObjectLink p gr2.LeadEntity.Index
            addTargetLink p gr2.Continue.Index
        // The closer triggers trigger the further triggers
        for closer, further in Seq.zip proximities further do
            addTargetLink closer further.Index
        // The closer triggers are triggered by reaching wp0
        for closer, (gr1, _) in Seq.zip proximities consecutive do
            addTargetLink gr1.StartWaypoint closer.Index
        // A timer that gets everybody moving by activating the first group
        let timer = Vluki.timer.SetTime(T.Float(1.0)).AsCommand()
        let getNewId = itemIds.GetIdMapper()
        substId getNewId timer
        addTargetLink timer (List.head groups).Activate.Index
        // Result
        { StartAll = timer
          Groups = groups
          Glue = (List.map (fun x -> upcast x) proximities) @ (List.map (fun x -> upcast x) further)
        }
    member this.AsString() =
        seq {
            yield this.StartAll.AsString()
            for g in this.Glue do yield g.AsString()
            for gr in this.Groups do yield gr.AsString()
        }
        |> String.concat("\n")

let platoon = TankPlatoon.Create(5)
using (System.IO.File.CreateText(@"C:\Users\johann\Documents\Visual Studio 2013\Projects\sturmovikmission\data\vluki-rescue\out.group")) (fun w ->
    let s = platoon.AsString()
    w.Write(s)
)
