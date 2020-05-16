module SturmovikMission.Blocks.McuInstantiation

open System.Numerics
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open VectorExtension
open SturmovikMission.Blocks.BlocksMissionData

type DirectedPoint =
    { Pos : Vector2
      Direction : float32
    }
with
    member this.AssignTo(mcu : Mcu.McuTrigger) =
        this.Pos.AssignTo mcu.Pos
        mcu.Ori.Y <- float this.Direction

    static member inline FromMCU(mcu) =
        let pos = Vector2.FromPos(mcu)
        let direction = mcu |> CommonMethods.getYOri |> CommonMethods.valueOf |> float32
        { Pos = pos; Direction = direction }

/// Extract a group of nodes and a node to use as reference when relocating.
let extractGroup group groupName refName =
    let selected =
        group |> filterByPath [groupName] |> List.ofSeq
    selected,
    selected |> filterByName refName |> Seq.exactlyOne

/// Move and rotate a group to a new location.
let relocateGroup (newCenter : DirectedPoint) (group : #Mcu.McuBase seq * Mcu.McuBase) =
    let mcus, refMcu = group
    let refPos = Vector2.FromMcu refMcu.Pos
    let rotation = newCenter.Direction - float32 refMcu.Ori.Y
    for mcu in mcus do
        let r = (Vector2.FromMcu mcu.Pos) - refPos
        let r = newCenter.Pos + r.Rotate(rotation)
        r.AssignTo(mcu.Pos)
        mcu.Ori.Y <- (mcu.Ori.Y + float rotation) % 360.0

/// Set the altitude of a group of nodes.
let setAltitude (altitude : int) (mcus : #Mcu.McuBase seq) =
    for mcu in mcus do
        mcu.Pos.Y <- float altitude

/// Get nodes from a group and give them new numerical IDs.
let getFreshGroup (source : T.GroupData) (store : NumericalIdentifiers.IdStore) (name : string) =
    let subst = Mcu.substId(store.GetIdMapper())
    let mcus = source.GetGroup(name).CreateMcuList()
    for mcu in mcus do
        subst mcu
    mcus, subst

/// Clone a group, giving its members new numerical IDs.
let cloneFresh (store : NumericalIdentifiers.IdStore) (group : #Mcu.McuBase seq) =
    let repr =
        group
        |> Seq.map (fun mcu -> mcu.AsString())
        |> String.concat "\n"
    let subst = Mcu.substId(store.GetIdMapper())
    let mcus = T.GroupData.Parse(Parsing.Stream.FromString repr).CreateMcuList()
    for mcu in mcus do
        subst mcu
    mcus

/// Put a nodes in a named group
let gatherInNamedGroup (store : NumericalIdentifiers.IdStore) name (mcus : #Mcu.McuBase seq) =
    let idx = store.GetIdMapper() 1
    for mcu in mcus do
        mcu.Path <- mcu.Path @ [name, idx]

type IMcuGroup with
    /// Put all nodes in this group under one named group
    member this.PushGroupName(store, name) =
        deepContentOf this
        |> gatherInNamedGroup store name

type IHasVehicles =
    abstract Vehicles : Mcu.HasEntity seq

let setVehiclesAfterPlane (proto : T.Plane) (hasVehicles : IHasVehicles) =
    for plane in hasVehicles.Vehicles do
        plane.Model <- proto.GetModel().Value
        plane.Script <- proto.GetScript().Value
        plane.WMMask <- Some (proto.GetWMMask().Value)
        plane.PayloadId <- Some (proto.GetPayloadId().Value)
        plane.AILevel <- Some (proto.GetAILevel().Value)
        plane.Country <- Some (enum(proto.GetCountry().Value))