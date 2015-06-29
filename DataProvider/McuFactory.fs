module SturmovikMission.DataProvider.McuFactory

open SturmovikMission.DataProvider.Ast
open SturmovikMission.DataProvider.Mcu

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

let private mkVector(nx, ny, nz) state =
    {
        new Vec3 with
            member this.X
                with get() = !state |> getFloatField nx
                and set(x) = state := !state |> setField(nx, Value.Float x)
            member this.Y
                with get() = !state |> getFloatField ny
                and set(x) = state := !state |> setField(ny, Value.Float x)
            member this.Z
                with get() = !state |> getFloatField nz
                and set(x) = state := !state |> setField(nz, Value.Float x)
    }

let private mkIconLCData state =
    {
        new IconLCData with
            member this.LCDesc
                with get() =
                    !state |> getIntField "LCDesc"
                and set(x) =
                    state := !state |> setField("LCDesc", Value.Integer x)
            member this.LCName
                with get() =
                    !state |> getIntField "LCName"
                and set(x) =
                    state := !state |> setField("LCName", Value.Integer x)
    }

let private hasIconLCData (fields) =
    [ ("LCDesc", ValueType.Integer)
      ("LCName", ValueType.Integer)
    ]
    |> List.forall (hasField fields)

let private mkSubtitleData state = 
    { new SubtitleLCData with
          
          member this.LCText 
              with get () = 
                  !state
                  |> List.pick (function 
                         | ("SubtitleInfo", Value.Composite fields) -> Some fields
                         | _ -> None)
                  |> getIntField "LCText"
              and set (x) = 
                  state := !state |> List.map (function 
                                         | ("SubtitleInfo" as k, Value.Composite fields) -> 
                                             (k, 
                                              fields
                                              |> setField ("LCText", Value.Integer x)
                                              |> Value.Composite)
                                         | x -> x) }

let private hasSubtitleLCData (fields) = 
    match Map.tryFind "SubtitleInfo" fields with
    | Some(ValueType.Composite subFields, _, _) ->
        hasField subFields ("LCText", ValueType.Integer)
    | Some _
    | None -> false

let private mkLCData typeFields state =
    let iconLC =
        if hasIconLCData typeFields then
            Some(mkIconLCData state)
        else
            None
    let subtitleLC =
        if hasSubtitleLCData typeFields then
            Some(mkSubtitleData state)
        else
            None
    iconLC, subtitleLC

let private mkAsBase (state : (string * Value) list ref) iconImpl subtitleImpl =
    {                
        new McuBase with
            member this.AsString() =
                dump (Composite !state)
                        
            member this.Ori = mkVector ("XOri", "YOri", "ZOri") state

            member this.Pos = mkVector ("XPos", "YPos", "ZPos") state

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

            member this.IconLC = iconImpl

            member this.SubtitleLC = subtitleImpl
    }

let private mkAsCommand (state : (string * Value) list ref) iconImpl subtitleImpl =
    let baseImpl = mkAsBase state iconImpl subtitleImpl
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
            member this.Ori = baseImpl.Ori
            member this.Pos = baseImpl.Pos
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.Name
                with get() = baseImpl.Name
                and set name = baseImpl.Name <- name
            member this.IconLC = baseImpl.IconLC
            member this.SubtitleLC = baseImpl.SubtitleLC
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
            let typeFields = fields
            function
            | Value.Composite fields ->                
                let state = ref fields
                let iconLC, subtitleLC = mkLCData typeFields state
                mkAsCommand state iconLC subtitleLC
            | _ -> invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let private mkAsEntity (state : (string * Value) list ref) iconLC subtitleLC =
    let cmd = mkAsCommand state iconLC subtitleLC
    let baseImpl : McuBase = upcast cmd
    {
        new McuEntity with
            member this.OnEvents
                with get() =
                    let events =
                        !state
                        |> List.choose (function
                            | ("OnEvents", Value.Composite subFields) ->
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
            member this.Ori = baseImpl.Ori
            member this.Pos = baseImpl.Pos
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.Name
                with get() = baseImpl.Name
                and set name = baseImpl.Name <- name
            member this.IconLC = iconLC
            member this.SubtitleLC = subtitleLC
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
            let typeFields = fields
            function
            | Value.Composite fields ->                
                let state = ref fields
                let iconLC, subtitleLC = mkLCData typeFields state
                mkAsEntity state iconLC subtitleLC
            | _ ->
                invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let private mkAsHasEntity (state : (string * Value) list ref) iconLC subtitleLC =
    let baseImpl = mkAsBase state iconLC subtitleLC
    {
        new HasEntity with
            member this.LinkTrId
                with get() =
                    !state |> getIntField "LinkTrId"
                and set idx =
                    state := !state |> setField ("LinkTrId", Value.Integer idx)
                
        interface McuBase with
            member this.AsString() = baseImpl.AsString()                        
            member this.Ori = baseImpl.Ori
            member this.Pos = baseImpl.Pos
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.Name
                with get() = baseImpl.Name
                and set name = baseImpl.Name <- name
            member this.IconLC = iconLC
            member this.SubtitleLC = subtitleLC
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
            let typeFields = fields
            function
            | Value.Composite fields ->
                let state = ref fields
                let iconLC, subtitleLC = mkLCData typeFields state
                mkAsHasEntity state iconLC subtitleLC
            | _ -> invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let upcastMaker (f : Value -> #McuBase) : (Value -> McuBase) =
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

