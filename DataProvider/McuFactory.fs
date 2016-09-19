//    Copyright 2015 Johann Deneux
//
//    This file is part of SturmovikMission.
//
//    SturmovikMission is free software: you can redistribute it and/or modify
//    it under the terms of the GNU Lesser General Public License as published by
//    the Free Software Foundation, either version 3 of the License, or
//    (at your option) any later version.
//
//    SturmovikMission is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU Lesser General Public License for more details.
//
//    You should have received a copy of the GNU Lesser General Public License
//    along with SturmovikMission.  If not, see <http://www.gnu.org/licenses/>.


module SturmovikMission.DataProvider.McuFactory

open SturmovikMission.DataProvider.Ast
open SturmovikMission.DataProvider.Mcu

let private hasField (fields) (fieldName : string, fieldTyp : ValueType) =
    match Map.tryFind fieldName fields with
    | Some (typ, _, _) -> typ = fieldTyp
    | None -> false

let private requiredForBase =
    [ ("Index", ValueType.Integer)
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
    let rec work xs =
        match xs with
        | [] -> [(name, value)]
        | (name2, _) :: rest when name2 = name -> (name, value) :: rest
        | x :: rest -> x :: work rest
    work fields

let tryGetId =
    function
    | Ast.Value.Composite fields ->
        fields
        |> List.tryPick (function ("Index", Ast.Value.Integer n) -> Some n | _ -> None)
    | _ ->
        None

let tryGetTargets =
    function
    | Ast.Value.Composite fields ->
        fields
        |> List.tryPick (function ("Targets", Ast.Value.IntVector ns) -> Some ns | _ -> None)
    | _ ->
        None

let tryGetObjects =
    function
    | Ast.Value.Composite fields ->
        fields
        |> List.tryPick (function ("Objects", Ast.Value.IntVector ns) -> Some ns | _ -> None)
    | _ ->
        None

let tryGetEntity =
    function
    | Ast.Value.Composite fields ->
        fields
        |> List.tryPick (function ("LinkTrId", Ast.Value.Integer n) -> Some n | _ -> None)
    | _ ->
        None

let tryGetOwner =
    function
    | Ast.Value.Composite fields ->
        fields
        |> List.tryPick (function ("MisObjID", Ast.Value.Integer n) -> Some n | _ -> None)
    | _ ->
        None

let tryGetEvents =
    function
    | Ast.Value.Composite fields ->
        let events =
            fields
            |> List.choose (function
                | ("OnEvents", Value.Composite subFields) ->
                    subFields
                    |> List.choose(function ("OnEvent", event) -> Some event | _ -> None)
                    |> Some
                | _ -> None)
            |> List.concat
        events
        |> List.choose (function
            | Value.Composite ev ->
                let typ = ev |> getIntField "Type"
                let target = ev |> getIntField "TarId"
                Some (typ, target)
            | _ -> None)
        |> Some
    | _ ->
        None

let tryGetReports =
    function
    | Ast.Value.Composite fields ->
        let events =
            fields
            |> List.choose (function
                | ("OnReports", Value.Composite subFields) ->
                    subFields
                    |> List.choose(function ("OnReport", event) -> Some event | _ -> None)
                    |> Some
                | _ -> None)
            |> List.concat
        events
        |> List.choose (function
            | Value.Composite ev ->
                let typ = ev |> getIntField "Type"
                let target = ev |> getIntField "TarId"
                let cmd = ev |> getIntField "CmdId"
                Some (typ, target)
            | _ ->
                None)
        |> Some
    | _ ->
        None

let tryGetName =
    function
    | Ast.Value.Composite fields ->
        fields
        |> List.tryPick (function
            | ("Name", Value.String n) -> Some n
            | _ -> None)
    | _ ->
        None

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

let private mkAsBase (typeName : string) (path : (string * int) list ref) (state : (string * Value) list ref) iconImpl subtitleImpl =
    {                
        new McuBase with
            member this.AsString() =
                sprintf "%s %s" typeName (dump (Composite !state))
                        
            member this.Ori = mkVector ("XOri", "YOri", "ZOri") state

            member this.Pos = mkVector ("XPos", "YPos", "ZPos") state

            member this.Index
                with get() =
                    !state |> getIntField "Index"
                and set idx =
                    state := !state |> setField ("Index", Value.Integer idx)

            member this.IconLC = iconImpl

            member this.SubtitleLC = subtitleImpl

            member this.Path
                with get() =
                    !path
                and set(path2) =
                    path := path2
    }

let tryMkAsBase (typeName : string, typ : ValueType) =
    match typ with
    | ValueType.Composite fields ->
        let required = requiredForBase
        let hasItAll =
            required
            |> List.forall (hasField fields)
        if hasItAll then
            let typeFields = fields
            function
            | Value.Composite fields, path ->
                let state = ref fields
                let path = ref path
                let iconLC, subtitleLC = mkLCData typeFields state
                mkAsBase typeName path state iconLC subtitleLC
            | _ -> invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None

let private mkAsComplex path (typeName : string) (state : (string * Value) list ref) =
    let baseImpl = mkAsBase typeName path state None None
    {
        new McuComplex with
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
            member this.Name
                with get() =
                    !state |> getStringField "Name"
                and set name =
                    state := !state |> setField ("Name", Value.String name)
            
        interface McuBase with
            member this.AsString() = baseImpl.AsString()
            member this.Ori = baseImpl.Ori
            member this.Pos = baseImpl.Pos
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.IconLC = baseImpl.IconLC
            member this.SubtitleLC = baseImpl.SubtitleLC
            member this.Path
                with get() = baseImpl.Path
                and set(p) = baseImpl.Path <- p
    }

let tryMkAsComplex (typeName : string, typ : ValueType) =
    match typeName, typ with
    | "MCU_TR_ComplexTrigger", ValueType.Composite fields ->
        let typeFields = fields
        function
        | Value.Composite fields, path ->
            let state = ref fields
            let path = ref path
            mkAsComplex path typeName state
        | _ -> invalidArg "value" "Not a composite"
        |> Some
    | _ ->
        None


let private mkAsIcon (typeName : string) path (state : (string * Value) list ref) iconImpl subtitleImpl =
    let baseImpl = mkAsBase typeName path state iconImpl subtitleImpl
    {
        new McuIcon with
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
            member this.IconLC = baseImpl.IconLC
            member this.SubtitleLC = baseImpl.SubtitleLC
            member this.Path
                with get() = baseImpl.Path
                and set(p) = baseImpl.Path <- p
    }


let tryMkAsIcon (typeName : string, typ : ValueType) =
    match typ with
    | ValueType.Composite fields ->
        let required =
            [ ("Targets", ValueType.IntVector) ] @ requiredForBase
        let hasItAll =
            required
            |> List.forall (hasField fields)
        if hasItAll then
            let typeFields = fields
            function
            | Value.Composite fields, path ->
                let state = ref fields
                let path = ref path
                let iconLC, subtitleLC = mkLCData typeFields state
                mkAsIcon typeName path state iconLC subtitleLC
            | _ -> invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let private mkAsTrigger (typeName : string) path (state : (string * Value) list ref) iconImpl subtitleImpl =
    let baseImpl = mkAsBase typeName path state iconImpl subtitleImpl
    {
        new McuTrigger with
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

            member this.Name
                with get() =
                    !state |> getStringField "Name"
                and set name =
                    state := !state |> setField ("Name", Value.String name)

                
        interface McuBase with
            member this.AsString() = baseImpl.AsString()
            member this.Ori = baseImpl.Ori
            member this.Pos = baseImpl.Pos
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.IconLC = baseImpl.IconLC
            member this.SubtitleLC = baseImpl.SubtitleLC
            member this.Path
                with get() = baseImpl.Path
                and set(p) = baseImpl.Path <- p
    }


let tryMkAsTrigger (typeName : string, typ : ValueType) =
    match typ with
    | ValueType.Composite fields ->
        let required =
            [ ("Objects", ValueType.IntVector)
              ("Targets", ValueType.IntVector)
              ("Name", ValueType.String) ] @ requiredForBase
        let hasItAll =
            required
            |> List.forall (hasField fields)
        if hasItAll then
            let typeFields = fields
            function
            | Value.Composite fields, path ->
                let state = ref fields
                let path = ref path
                let iconLC, subtitleLC = mkLCData typeFields state
                mkAsTrigger typeName path state iconLC subtitleLC
            | _ -> invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let tryMkAsProximity (typeName : string, typ : ValueType) =
    match typeName with
    | "MCU_Proximity"
    | "MCU_CheckZone" ->
        match typ with
        | ValueType.Composite typeFields ->
            match tryMkAsTrigger (typeName, typ) with
            | Some _ ->
                function
                | Value.Composite fields as value, path ->
                    let state = ref fields
                    let path = ref path
                    let iconLC, subtitleLC = mkLCData typeFields state
                    let baseImpl = mkAsTrigger typeName path state iconLC subtitleLC
                    {
                        new McuProximity with
                            member this.PlaneCoalitions
                                with get() =
                                    !state |> getIntVecField "PlaneCoalitions"
                                and set(coalitions) =
                                    state := !state |> setField("PlaneCoalitions", IntVector coalitions)

                            member this.VehicleCoalitions
                                with get() =
                                    !state |> getIntVecField "VehicleCoalitions"
                                and set(coalitions) =
                                    state := !state |> setField("VehicleCoalitions", IntVector coalitions)

                        interface McuTrigger with
                            member this.AsString() = baseImpl.AsString()
                            member this.Ori = baseImpl.Ori
                            member this.Pos = baseImpl.Pos
                            member this.Index
                                with get() = baseImpl.Index
                                and set idx = baseImpl.Index <- idx
                            member this.IconLC = baseImpl.IconLC
                            member this.SubtitleLC = baseImpl.SubtitleLC
                            member this.Objects
                                with get() = baseImpl.Objects
                                and set xs = baseImpl.Objects <- xs
                            member this.Targets
                                with get() = baseImpl.Targets
                                and set xs = baseImpl.Targets <- xs
                            member this.Name
                                with get() = baseImpl.Name
                                and set name = baseImpl.Name <- name
                            member this.Path
                                with get() = baseImpl.Path
                                and set(p) = baseImpl.Path <- p
                    }
                | _ ->
                    invalidArg "value" "Not a composite"
                |> Some
            | None ->
                None
        | _ ->
            None
    | _ ->
        None


let tryMkAsWaypoint (typeName : string, typ : ValueType) =
    match typeName with
    | "MCU_Waypoint" ->
        match typ with
        | ValueType.Composite typeFields ->
            match tryMkAsTrigger (typeName, typ) with
            | Some _ ->
                function
                | Value.Composite fields as value, path ->
                    let state = ref fields
                    let path = ref path
                    let iconLC, subtitleLC = mkLCData typeFields state
                    let baseImpl = mkAsTrigger typeName path state iconLC subtitleLC
                    {
                        new McuWaypoint with
                            member this.Radius
                                with get() =
                                    !state |> getIntField "Area"
                                and set(coalitions) =
                                    state := !state |> setField("Area", Integer coalitions)

                            member this.Speed
                                with get() =
                                    !state |> getIntField "Speed"
                                and set(coalitions) =
                                    state := !state |> setField("Speed", Integer coalitions)

                            member this.Priority
                                with get() =
                                    !state |> getIntField "Priority"
                                and set(coalitions) =
                                    state := !state |> setField("Priority", Integer coalitions)

                        interface McuTrigger with
                            member this.AsString() = baseImpl.AsString()
                            member this.Ori = baseImpl.Ori
                            member this.Pos = baseImpl.Pos
                            member this.Index
                                with get() = baseImpl.Index
                                and set idx = baseImpl.Index <- idx
                            member this.IconLC = baseImpl.IconLC
                            member this.SubtitleLC = baseImpl.SubtitleLC
                            member this.Objects
                                with get() = baseImpl.Objects
                                and set xs = baseImpl.Objects <- xs
                            member this.Targets
                                with get() = baseImpl.Targets
                                and set xs = baseImpl.Targets <- xs
                            member this.Name
                                with get() = baseImpl.Name
                                and set name = baseImpl.Name <- name
                            member this.Path
                                with get() = baseImpl.Path
                                and set(p) = baseImpl.Path <- p
                    }
                | _ ->
                    invalidArg "value" "Not a composite"
                |> Some
            | None ->
                None
        | _ ->
            None
    | _ ->
        None

let tryMkAsTimer (typeName : string, typ : ValueType) =
    match typeName with
    | "MCU_Timer" ->
        match typ with
        | ValueType.Composite typeFields ->
            match tryMkAsTrigger (typeName, typ) with
            | Some _ ->
                function
                | Value.Composite fields as value, path ->
                    let state = ref fields
                    let path = ref path
                    let iconLC, subtitleLC = mkLCData typeFields state
                    let baseImpl = mkAsTrigger typeName path state iconLC subtitleLC
                    {
                        new McuTimer with
                            member this.Time
                                with get() =
                                    !state |> getIntField "Time"
                                and set(time) =
                                    state := !state |> setField("Time", Integer time)

                        interface McuTrigger with
                            member this.AsString() = baseImpl.AsString()
                            member this.Ori = baseImpl.Ori
                            member this.Pos = baseImpl.Pos
                            member this.Index
                                with get() = baseImpl.Index
                                and set idx = baseImpl.Index <- idx
                            member this.IconLC = baseImpl.IconLC
                            member this.SubtitleLC = baseImpl.SubtitleLC
                            member this.Objects
                                with get() = baseImpl.Objects
                                and set xs = baseImpl.Objects <- xs
                            member this.Targets
                                with get() = baseImpl.Targets
                                and set xs = baseImpl.Targets <- xs
                            member this.Name
                                with get() = baseImpl.Name
                                and set name = baseImpl.Name <- name
                            member this.Path
                                with get() = baseImpl.Path
                                and set(p) = baseImpl.Path <- p
                    }
                | _ ->
                    invalidArg "value" "Not a composite"
                |> Some
            | None ->
                None
        | _ ->
            None
    | _ ->
        None


let private mkAsEntity typeName path (state : (string * Value) list ref) iconLC subtitleLC =
    let cmd = mkAsTrigger typeName path state iconLC subtitleLC
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

            member this.Name
                with get() =
                    !state |> getStringField "Name"
                and set name =
                    state := !state |> setField ("Name", Value.String name)


            member this.OnReports
                with get() =
                    let events =
                        !state
                        |> List.choose (function
                            | ("OnReports", Value.Composite subFields) ->
                                subFields
                                |> List.choose(function ("OnReport", event) -> Some event | _ -> None)
                                |> Some
                            | _ -> None)
                        |> List.concat
                    events
                    |> List.map (function
                        | Value.Composite ev ->
                            let typ = ev |> getIntField "Type"
                            let target = ev |> getIntField "TarId"
                            let cmd = ev |> getIntField "CmdId"
                            { Type = typ; TarId = target; CmdId = cmd }
                        | _ -> failwith "Report connection is not a Composite")
                and set(xs) =
                    let evs =
                        xs
                        |> List.map (fun ev ->
                            ("OnReport", Value.Composite [ ("Type", Value.Integer ev.Type); ("CmdId", Value.Integer ev.CmdId); ("TarId", Value.Integer ev.TarId) ])
                            )
                        |> Value.Composite
                    state :=
                        !state |> setField ("OnReports", evs)

            member this.MisObjID
                with get() =
                    !state |> getIntField "MisObjID"
                and set(idx) =
                    state := !state |> setField("MisObjID", Value.Integer idx)

        interface McuTrigger with
            member this.Objects
                with get() = cmd.Objects
                and set(x) = cmd.Objects <- x
            member this.Targets
                with get() = cmd.Targets
                and set(x) = cmd.Targets <- x
            member this.Name
                with get() = cmd.Name
                and set name = cmd.Name <- name
        
        interface McuBase with
            member this.AsString() = baseImpl.AsString()
            member this.Ori = baseImpl.Ori
            member this.Pos = baseImpl.Pos
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.IconLC = iconLC
            member this.SubtitleLC = subtitleLC
            member this.Path
                with get() = baseImpl.Path
                and set(p) = baseImpl.Path <- p
    }


let tryMkAsEntity (typeName : string, typ : ValueType) =
    match typ with
    | ValueType.Composite fields ->
        let required =
            [ ("MisObjID", ValueType.Integer)
              ("Objects", ValueType.IntVector)
              ("Targets", ValueType.IntVector)
              ("Name", ValueType.String) ] @ requiredForBase
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
            | Value.Composite fields, path ->
                let state = ref fields
                let path = ref path
                let iconLC, subtitleLC = mkLCData typeFields state
                mkAsEntity typeName path state iconLC subtitleLC
            | _ ->
                invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let private mkAsHasEntity typeName path (state : (string * Value) list ref) iconLC subtitleLC formation =
    let baseImpl = mkAsBase typeName path state iconLC subtitleLC
    {
        new HasEntity with
            member this.LinkTrId
                with get() =
                    !state |> getIntField "LinkTrId"
                and set idx =
                    state := !state |> setField ("LinkTrId", Value.Integer idx)

            member this.Name
                with get() =
                    !state |> getStringField "Name"
                and set name =
                    state := !state |> setField ("Name", Value.String name)

            member this.Script
                with get() =
                    !state |> getStringField "Script"
                and set script =
                    state := !state |> setField ("Script", Value.String script)

            member this.Model
                with get() =
                    !state |> getStringField "Model"
                and set model =
                    state := !state |> setField ("Model", Value.String model)

            member this.Country
                with get() =
                    !state |> getIntField "Country"
                and set country =
                    state := !state |> setField ("Country", Value.Integer country)

            member this.NumberInFormation = formation

        interface McuBase with
            member this.AsString() = baseImpl.AsString()
            member this.Ori = baseImpl.Ori
            member this.Pos = baseImpl.Pos
            member this.Index
                with get() = baseImpl.Index
                and set idx = baseImpl.Index <- idx
            member this.IconLC = iconLC
            member this.SubtitleLC = subtitleLC
            member this.Path
                with get() = baseImpl.Path
                and set(p) = baseImpl.Path <- p
    }


let tryMkAsHasEntity (typeName : string, typ : ValueType) =
    match typ with
    | ValueType.Composite fields ->
        let required =
            [ ("LinkTrId", ValueType.Integer)
              ("Name", ValueType.String) ] @ requiredForBase
        let hasItAll =
            required
            |> List.forall (hasField fields)
        if hasItAll then
            let typeFields = fields
            function
            | Value.Composite fields, path ->
                let state = ref fields
                let path = ref path
                let iconLC, subtitleLC = mkLCData typeFields state
                let formation =
                    if hasField typeFields ("NumberInFormation", ValueType.Integer) then
                        { new NumberInFormationData with
                            member this.Number
                                with get() =
                                    !state |> getIntField "NumberInFormation"
                                and set number =
                                    state := !state |> setField ("NumberInFormation", Value.Integer number)
                        } |> Some
                    else
                        None
                mkAsHasEntity typeName path state iconLC subtitleLC formation
            | _ -> invalidArg "value" "Not a composite"
            |> Some
        else
            None
    | _ ->
        None


let upcastMaker (f : Value * (string * int) list -> #McuBase) : (Value * (string * int) list -> McuBase) =
    fun value ->
        upcast(f value)

let upcastMaybeMaker f =
    f |> Option.map upcastMaker

let upcastTryMaker (f :  string * ValueType -> (Value * (string * int) list -> #McuBase) option) =
    fun namedValueType ->
        upcastMaybeMaker(f namedValueType)

let makers =
    [
        upcastTryMaker tryMkAsComplex
        upcastTryMaker tryMkAsEntity
        upcastTryMaker tryMkAsHasEntity
        upcastTryMaker tryMkAsTimer
        upcastTryMaker tryMkAsWaypoint
        upcastTryMaker tryMkAsProximity
        upcastTryMaker tryMkAsTrigger
        upcastTryMaker tryMkAsIcon
        upcastTryMaker tryMkAsBase
    ]

let tryMakeMcu valueType =
    makers
    |> List.tryPick (fun f -> f valueType)

