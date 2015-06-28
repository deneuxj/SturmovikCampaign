module SturmovikMission.DataProvider.Mcu

open SturmovikMission.DataProvider.Ast

/// <summary>
/// Base interface for all MCUs.
/// A subset of the properties of objects is made accessible. These are the
/// properties that would typically need to be changed when instancing a
/// template.
/// Instances of this type are mutable.
/// </summary>
type McuBase =
    abstract Index : int with get, set
    abstract Name : string with get, set
    abstract Pos : (float * float * float) with get, set
    abstract Ori : (float * float * float) with get, set
    /// <summary>
    /// Build a string using the syntax of mission files that specifies all the
    /// fields of this instance (not only those in McuBase and its subtypes).
    /// </summary>
    abstract AsString : unit -> string

/// <summary>
/// Interface of commands (timers, proximity triggers...)
/// </summary>
type McuCommand =
    inherit McuBase
    abstract Objects : int list with get, set
    abstract Targets : int list with get, set

/// <summary>
/// Connection of an event from an entity to a target command.
/// </summary>
type EventConnection =
    { Type : int
      TarId : int }

/// <summary>
/// Interface of entities, i.e. active parts of vehicles and other 3d objects.
/// </summary>
type McuEntity =
    inherit McuCommand
    abstract MisObjID : int with get, set
    abstract OnEvents : EventConnection list with get, set

/// <summary>
/// Interface of things that have entities: ground vehicles, planes, artillery, buildings, bridges...
/// </summary>
type HasEntity =
    inherit McuBase
    /// <summary>
    /// Get or set the link to the entity. Set to 0 it lacks an entity.
    /// </summary>
    abstract LinkTrId : int with get, set

/// <summary>
/// Substitute occurrences of numerical ids in an MCU.
/// </summary>
/// <param name="getNewId">Function that provides the new id given an old id.</param>
/// <param name="mcu">The MCU whose ids are changed. Instance is mutated.</param>
let substId (getNewId : int -> int) (mcu : McuBase) =
    mcu.Index <- getNewId mcu.Index
    match mcu with
    | :? McuCommand as cmd ->
        cmd.Objects <- cmd.Objects |> List.map getNewId
        cmd.Targets <- cmd.Targets |> List.map getNewId
    | _ -> ()
    match mcu with
    | :? McuEntity as ent ->
        ent.MisObjID <- getNewId ent.MisObjID
        ent.OnEvents <- ent.OnEvents |> List.map (fun ev -> { ev with TarId = getNewId ev.TarId })
    | _ -> ()
    match mcu with
    | :? HasEntity as veh ->
        veh.LinkTrId <- getNewId veh.LinkTrId
    | _ -> ()

/// <summary>
/// Add an object link to a command.
/// </summary>
/// <param name="mcu">The command, which is mutated.</param>
/// <param name="objekt">The id of the object.</param>
let addObjectLink (mcu : McuCommand) (objekt : int) =
    mcu.Objects <- objekt :: mcu.Objects

/// <summary>
/// Add a target link to a command.
/// </summary>
/// <param name="mcu">The command, which is mutated.</param>
/// <param name="target">The id of the target.</param>
let addTargetLink (mcu : McuCommand) (target : int) =
    mcu.Targets <- target :: mcu.Targets

/// <summary>
/// Connect a entity-holding object and its entity.
/// </summary>
/// <param name="veh">The entity owner, which is mutated.</param>
/// <param name="ent">The entity, which is mutated.</param>
let connectEntity (veh : HasEntity) (ent : McuEntity) =
    veh.LinkTrId <- ent.Index
    ent.MisObjID <- veh.Index

let private hasField (fields) (fieldName : string, fieldTyp : ValueType) =
    match Map.tryFind fieldName fields with
    | Some (typ, _, _) -> typ = fieldTyp
    | None -> false

let private requiredForBase =
    [ ("Index", ValueType.Integer)
      ("Name", ValueType.String)
      ("XPos", ValueType.Float)
      ("YPos", ValueType.Float)
      ("ZPos", ValueType.Float)
      ("XOri", ValueType.Float)
      ("YOri", ValueType.Float)
      ("ZOri", ValueType.Float) ]

let getIntField (name : string) fields =
    fields
    |> Seq.pick (function (name2, Value.Integer n) when name = name2 -> Some n | _ -> None)

let getFloatField (name : string) fields =
    fields
    |> Seq.pick (function (name2, Value.Float n) when name = name2 -> Some n | _ -> None)

let getIntVecField (name : string) fields =
    fields
    |> Seq.pick (function (name2, Value.IntVector xs) when name = name2 -> Some xs | _ -> None)

let getStringField (name : string) fields =
    fields
    |> Seq.pick (function (name2, Value.String s) when name = name2 -> Some s | _ -> None)

let getSetField  (name : string) fields =
    fields
    |> Seq.pick (function (name2, Value.Set s) when name = name2 -> Some s | _ -> None)

let setField (name : string, value) fields =
    fields
    |> List.map (function (name2, _) when name2 = name -> (name, value) | x -> x)

let private mkAsBase (state : (string * Value) list ref) =
    {                
        new McuBase with
            member this.AsString() =
                dump (Composite !state)
                        
            member this.Ori
                with get() =
                    let x = !state |> getFloatField "XOri"
                    let y = !state |> getFloatField "YOri"
                    let z = !state |> getFloatField "ZOri"
                    (x, y, z)
                and set(x, y, z) =
                    state :=
                        !state
                        |> setField ("XOri", Value.Float x)
                        |> setField ("YOri", Value.Float y)
                        |> setField ("ZOri", Value.Float z)

            member this.Pos
                with get() =
                    let x = !state |> getFloatField "XPos"
                    let y = !state |> getFloatField "YPos"
                    let z = !state |> getFloatField "ZPos"
                    (x, y, z)
                and set(x, y, z) =
                    state :=
                        !state
                        |> setField ("XPos", Value.Float x)
                        |> setField ("YPos", Value.Float y)
                        |> setField ("ZPos", Value.Float z)

            member this.Index
                with get() =
                    !state |> getIntField "Index"
                and set idx =
                    state := !state |> setField ("Index", Value.Integer idx)

            member this.Name
                with get() =
                    !state |> getStringField "Name"
                and set name =
                    state := !state |> setField ("Name", Value.String name)

    }

let private mkAsCommand (state : (string * Value) list ref) =
    let baseImpl = mkAsBase state
    {
        new McuCommand with
            member this.Objects
                with get() =
                    !state |> getIntVecField "Objects"
                and set xs =
                    state := !state |> setField ("Objects", Value.IntVector xs)

            member this.Targets
                with get() =
                    !state |> getIntVecField "Targets"
                and set xs =
                    state := !state |> setField ("Targets", Value.IntVector xs)
                
        interface McuBase with
            member this.AsString() = baseImpl.AsString()                        
            member this.Ori
                with get() = baseImpl.Ori
                and set(x, y, z) = baseImpl.Ori <- (x, y, z)
            member this.Pos
                with get() = baseImpl.Pos
                and set(x, y, z) = baseImpl.Pos <- (x, y, z)
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.Name
                with get() = baseImpl.Name
                and set name = baseImpl.Name <- name
    }


let tryMkAsCommand (typ : ValueType) =
    match typ with
    | ValueType.Composite fields ->
        let required =
            [ ("Objects", ValueType.IntVector)
              ("Targets", ValueType.IntVector) ] @ requiredForBase
        let hasItAll =
            required
            |> List.forall (hasField fields)
        if hasItAll then
            function
            | Value.Composite fields ->                
                let state = ref fields
                mkAsCommand state
            | _ -> invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let private mkAsEntity (state : (string * Value) list ref) =
    let cmd = mkAsCommand state
    let baseImpl : McuBase = upcast cmd
    {
        new McuEntity with
            member this.OnEvents
                with get() =
                    let events =
                        !state
                        |> List.choose (function
                            | ("OnEvents", Ast.Value.Composite subFields) ->
                                subFields
                                |> List.choose(function ("OnEvent", event) -> Some event | _ -> None)
                                |> Some
                            | _ -> None)
                        |> List.concat
                    events
                    |> List.map (function
                        | Value.Composite ev ->
                            let typ = ev |> getIntField "Type"
                            let target = ev |> getIntField "TarId"
                            { Type = typ; TarId = target }
                        | _ -> failwith "Event connection is not a Composite")
                and set(xs) =
                    let evs =
                        xs
                        |> List.map (fun ev ->
                            ("OnEvent", Value.Composite [ ("Type", Value.Integer ev.Type); ("TarId", Value.Integer ev.TarId) ])
                            )
                        |> Value.Composite
                    state :=
                        !state |> setField ("OnEvents", evs)

            member this.MisObjID
                with get() =
                    !state |> getIntField "MisObjID"
                and set(idx) =
                    state := !state |> setField("MisObjID", Value.Integer idx)

        interface McuCommand with
            member this.Objects
                with get() = cmd.Objects
                and set(x) = cmd.Objects <- x
            member this.Targets
                with get() = cmd.Targets
                and set(x) = cmd.Targets <- x
        
        interface McuBase with
            member this.AsString() = baseImpl.AsString()                        
            member this.Ori
                with get() = baseImpl.Ori
                and set(x, y, z) = baseImpl.Ori <- (x, y, z)
            member this.Pos
                with get() = baseImpl.Pos
                and set(x, y, z) = baseImpl.Pos <- (x, y, z)
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.Name
                with get() = baseImpl.Name
                and set name = baseImpl.Name <- name
    }


let tryMkAsEntity (typ : ValueType) =
    match typ with
    | ValueType.Composite fields ->
        let required =
            [ ("MisObjID", ValueType.Integer)
              ("Objects", ValueType.IntVector)
              ("Targets", ValueType.IntVector) ] @ requiredForBase
        let hasRequired =
            required
            |> List.forall (hasField fields)
        let hasEvents =
            fields
            |> Seq.exists (fun kvp ->
                match kvp.Key, kvp.Value with
                | "OnEvents", (ValueType.Composite fields, _, _) ->
                    match Map.tryFind "OnEvent" fields with
                    | Some(ValueType.Composite(subFields), _, _) ->
                        [ ("Type", ValueType.Integer)
                          ("TarId", ValueType.Integer) ]
                        |> List.forall (hasField subFields)
                    | None
                    | _ ->
                        false
                | _ -> false
            )
        let hasItAll =
            hasEvents &&
            hasRequired
        if hasItAll then
            function
            | Value.Composite fields ->                
                let state = ref fields
                mkAsEntity state
            | _ ->
                invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let private mkAsHasEntity (state : (string * Value) list ref) =
    let baseImpl = mkAsBase state
    {
        new HasEntity with
            member this.LinkTrId
                with get() =
                    !state |> getIntField "LinkTrId"
                and set idx =
                    state := !state |> setField ("LinkTrId", Value.Integer idx)
                
        interface McuBase with
            member this.AsString() = baseImpl.AsString()                        
            member this.Ori
                with get() = baseImpl.Ori
                and set(x, y, z) = baseImpl.Ori <- (x, y, z)
            member this.Pos
                with get() = baseImpl.Pos
                and set(x, y, z) = baseImpl.Pos <- (x, y, z)
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.Name
                with get() = baseImpl.Name
                and set name = baseImpl.Name <- name
    }


let tryMkAsHasEntity (typ : ValueType) =
    match typ with
    | ValueType.Composite fields ->
        let required =
            [ ("LinkTrId", ValueType.Integer) ] @ requiredForBase
        let hasItAll =
            required
            |> List.forall (hasField fields)
        if hasItAll then
            function
            | Value.Composite fields ->                
                let state = ref fields
                mkAsHasEntity state
            | _ -> invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let upcastMaker (f : Ast.Value -> #McuBase) : (Ast.Value -> McuBase) =
    fun value ->
        upcast(f value)

let upcastMaybeMaker f =
    f |> Option.map upcastMaker

let upcastTryMaker (f : ValueType -> (Value -> #McuBase) option) =
    fun valueType ->
        upcastMaybeMaker(f valueType)

let makers =
    [
        upcastTryMaker tryMkAsEntity
        upcastTryMaker tryMkAsHasEntity
        upcastTryMaker tryMkAsCommand
    ]

let tryMakeMcu valueType =
    makers
    |> List.tryPick (fun f -> f valueType)