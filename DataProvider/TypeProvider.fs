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


module SturmovikMission.DataProvider.TypeProvider

#nowarn "25" // Incomplete pattern matches, occurs a lot due to "fun [this] -> <@@ ... @@>"

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System
open System.IO
open SturmovikMission.DataProvider.Cached
open SturmovikMission.DataProvider.FileWithTime

/// <summary>
/// Build a function that provides new names that do not collide with earlier names.
/// </summary>
/// <param name="getValidNames">Function that produces a sequence of name candidates.</param>
let getNameStore getValidNames =
    let reserved =
        [ "Boolean"; "Float"; "Integer"; "String"; "VectorOfIntegers"; "Date" ]
    let store = ref <| Set reserved
    let isValid name =
        Set.contains name !store
        |> not
    fun baseName ->
        let name =
            getValidNames baseName
            |> Seq.find isValid
        store := Set.add name !store
        name

/// Build a function that provides names of the form "name", "name_1", "name_2"...
let mkNewName() =
    fun baseName ->
        Seq.initInfinite (fun i -> if i = 0 then baseName else sprintf "%s_%d" baseName (i + 1))
    |> getNameStore

type IProvidedDataBuilder =
    /// <summary>
    /// Build a ProvidedConstructor.
    /// </summary>
    /// <param name="args">Argument names and types.</param>
    /// <param name="body">The code to execute in the constructor.</param>
    abstract NewConstructor: args: ((string * Type) list) * body: (Expr list -> Expr) -> ProvidedConstructor
    /// <summary>
    /// Build a ProvidedProperty.
    /// </summary>
    /// <param name="name">Name of the property.</param>
    /// <param name="typ">Type of the property.</param>
    /// <param name="body">Code of the property.</param>
    abstract NewProperty: name: string * typ: Type * body: (Expr -> Expr) -> ProvidedProperty
    /// Build a static ProvidedProperty.
    abstract NewStaticProperty: name: string * typ: Type * body: Expr -> ProvidedProperty
    /// <summary>
    /// Build a ProvidedMethod.
    /// </summary>
    /// <param name="name">Name of the method.</param>
    /// <param name="typ">Type of returned value.</param>
    /// <param name="args">Arguments to the method with their respective types.</param>
    /// <param name="body">Body of the method.</param>
    abstract NewMethod: name: string * typ: Type * args: (string * Type) list * body: (Expr list -> Expr) -> ProvidedMethod
    /// Build a static ProvidedMethod.
    abstract NewStaticMethod: name: string * typ: Type * args : (string * Type) list * body: (Expr list -> Expr) -> ProvidedMethod

/// <summary>Controls the expression used for the InvokeCode of provided types</summary>
/// Visual Studio and possibly other IDEs process expressions in InvokeCodes rather often.
/// This can take enough time to render intellisense very sluggish.
/// Considering that IDEs typically do not need the bodies of provided types,
/// we let the user control whether such expressions should be assigned to InvokeCodes,
/// or "empty failwith shells" should be used instead.
/// There are three alternatives, two of which excplicitly specify what to do (FailWith and AsProvided),
/// and the third lets the implementation guess according to the entry assembly (FromAssembly).
type InvokeCodeImplementation =
    | FailWith = 0
    | AsProvided = 1
    | FromAssembly = 2

let mkProvidedDataBuilder (invokeImpl : InvokeCodeImplementation) =
    let bodyGate =
        match invokeImpl with
        | InvokeCodeImplementation.FailWith ->
            fun _ -> <@@ failwith "Bodies replaced by shells" @@>
        | InvokeCodeImplementation.AsProvided ->
            id
        | InvokeCodeImplementation.FromAssembly ->
            // Dirty method that guesses the correct thing to do from the entry assembly and process.
            let asm = System.Reflection.Assembly.GetEntryAssembly()
            if asm = null then
                let proc = System.Diagnostics.Process.GetCurrentProcess()
                match proc.ProcessName with
                | "Fsi" -> // Fsi running in an unmanaged process (such as Visual Studio)
                    id // Use the provided code.
                | _ -> // Some unmanaged process (such as Visual Studio)
                    fun _ -> <@@ failwith "Bodies replaced by shells" @@> // Replace by empty shells
            else // Some managed process (such as the F# compiler)
                id

    let funcGate =
        match invokeImpl with
        | InvokeCodeImplementation.FailWith ->
            fun _ -> fun _ -> <@@ failwith "Bodies replaced by shells" @@>
        | InvokeCodeImplementation.AsProvided ->
            id
        | InvokeCodeImplementation.FromAssembly ->
            // Dirty method that guesses the correct thing to do from the entry assembly and process.
            let asm = System.Reflection.Assembly.GetEntryAssembly()
            if asm = null then
                let proc = System.Diagnostics.Process.GetCurrentProcess()
                match proc.ProcessName with
                | "Fsi" -> // Fsi running in an unmanaged process (such as Visual Studio)
                    id // Use the provided code.
                | _ -> // Some unmanaged process (such as Visual Studio)
                    fun _ -> fun _ -> <@@ failwith "Bodies replaced by shells" @@> // Replace by empty shells
            else // Some managed process (such as the F# compiler)
                id
            
    let newConstructor (args : (string * Type) list) (body : Expr list -> Expr) =
        let args =
            args
            |> List.map (fun (n, t) -> ProvidedParameter(n, t))
        let cnstr =
            ProvidedConstructor(args)
        cnstr.InvokeCode <- funcGate body
        cnstr

    let newProperty (name : string, typ : Type) (body : Expr -> Expr) =
        let prop = ProvidedProperty(name, typ)
        prop.GetterCode <- fun [this] -> bodyGate(body this)
        prop

    let newStaticProperty (name : string, typ : Type) (body : Expr) =
        let prop = ProvidedProperty(name, typ)
        prop.IsStatic <- true
        prop.GetterCode <- fun [] -> bodyGate body
        prop

    let newMethod (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
        let args =
            args
            |> List.map (fun (n, t) -> ProvidedParameter(n, t))
        let m = ProvidedMethod(name, args, typ)
        m.InvokeCode <- funcGate body
        m

    let newStaticMethod (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
        let m = newMethod (name, typ) args body
        m.IsStaticMethod <- true
        m

    { new IProvidedDataBuilder with
        member __.NewConstructor(args, body) = newConstructor args body
        member __.NewProperty(name, typ, body) = newProperty (name, typ) body
        member __.NewStaticProperty(name, typ, body) = newStaticProperty (name, typ) body
        member __.NewMethod(name, typ, args, body) = newMethod (name, typ) args body
        member __.NewStaticMethod(name, typ, args, body) = newStaticMethod (name, typ) args body
    }

/// Add documentation to a provided method, constructor, property or type definition.
let inline addXmlDoc< ^T when ^T: (member AddXmlDoc : string -> unit)> (doc : string) (thing : ^T) : ^T =
    ( ^T : (member AddXmlDoc : string -> unit) (thing, doc))
    thing

/// <summary>
/// Get some name suitable for the type of a field (in a composite) of type composite, set or mapping, or None for other fields.
/// </summary>
/// <param name="fieldName">Name of the field.</param>
/// <param name="def">ValueType of the field.</param>
let private getNameOfField(fieldName : string, def) =
    match def with
    | Ast.ValueType.Set _
    | Ast.ValueType.Mapping _
    | Ast.ValueType.Composite _ -> Some fieldName
    | _ -> None

/// <summary>
/// Build the function that builds ProvidedTypeDefinitions for ValueTypes encountered in the sample mission file.
/// </summary>
/// <param name="top">Definition of the top type in the type provider.</param>
let mkProvidedTypeBuilder (pdb : IProvidedDataBuilder) (top : ProvidedTypeDefinition) =
    let newName = mkNewName()

    let cache = new Dictionary<string option * Ast.ValueType, ProvidedTypeDefinition>(HashIdentity.Structural)

    let rec buildProvidedType (name : string option, typ : Ast.ValueType) =
        let typExpr = typ.ToExpr()
        match typ with
        | Ast.ValueType.Boolean ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Boolean", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<bool>, fun this -> <@@ (%%this : Ast.Value).GetBool() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<bool>)], fun [value] -> <@@ Ast.Value.Boolean (%%value : bool) @@>))
            ptyp
        | Ast.ValueType.Float ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Float", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<float>, fun this -> <@@ (%%this : Ast.Value).GetFloat() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<float>)], fun [value] -> <@@ Ast.Value.Float (%%value : float) @@>))
            ptyp
        | Ast.ValueType.FloatPair ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "FloatPair", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<float * float>, fun this -> <@@ (%%this : Ast.Value).GetFloatPair() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<float * float>)], fun [value] -> <@@ let x, y = (%%value : float * float) in Ast.Value.FloatPair(x, y) @@>))
            ptyp
        | Ast.ValueType.Integer ->
            let ptyp =
                new ProvidedTypeDefinition("Integer", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<int>, fun this -> <@@ (%%this : Ast.Value).GetInteger() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<int>)], fun [value] -> <@@ Ast.Value.Integer (%%value : int) @@>))
            ptyp
        | Ast.ValueType.String ->
            let ptyp =
                new ProvidedTypeDefinition("String", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<string>, fun this -> <@@ (%%this : Ast.Value).GetString() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<string>)], fun [value] -> <@@ Ast.Value.String (%%value : string) @@>))
            ptyp
        | Ast.ValueType.IntVector ->
            let ptyp =
                new ProvidedTypeDefinition("VectorOfIntegers", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Value", typeof<int list>, fun this -> <@@ (%%this : Ast.Value).GetIntVector() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", typeof<int list>)], fun [value] -> <@@ Ast.Value.IntVector (%%value : int list) @@>))
            ptyp
        | Ast.ValueType.Pair (typ1, typ2) ->
            let ptyp1 = getProvidedType(None, typ1)
            let ptyp2 = getProvidedType(None, typ2)
            let ptyp =
                let name =
                    sprintf "PairOf%sAnd%s" ptyp1.Name ptyp2.Name
                    |> defaultArg name
                    |> newName
                new ProvidedTypeDefinition(name, Some (typeof<Ast.Value>))
            let propTyp = typedefof<_*_>.MakeGenericType(ptyp1, ptyp2)
            ptyp.AddMember(pdb.NewProperty("Value", propTyp, fun this -> <@@ (%%this : Ast.Value).GetPair() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", propTyp)], fun [value] -> <@@ Ast.Value.Pair (%%value : Ast.Value * Ast.Value) @@>))
            ptyp
        | Ast.ValueType.Triplet (typ1, typ2, typ3) ->
            let ptyp1 = getProvidedType(None, typ1)
            let ptyp2 = getProvidedType(None, typ2)
            let ptyp3 = getProvidedType(None, typ3)
            let ptyp =
                let name =
                    sprintf "TripletOf%sAnd%sAnd%s" ptyp1.Name ptyp2.Name ptyp3.Name
                    |> defaultArg name
                    |> newName
                new ProvidedTypeDefinition(name, Some (typeof<Ast.Value>))
            let propTyp = typedefof<_*_*_>.MakeGenericType(ptyp1, ptyp2, ptyp3)
            ptyp.AddMember(pdb.NewProperty("Value", propTyp, fun this -> <@@ (%%this : Ast.Value).GetTriplet() @@>))
            ptyp.AddMember(pdb.NewConstructor([("Value", propTyp)], fun [value] -> <@@ Ast.Value.Triplet (%%value : Ast.Value * Ast.Value * Ast.Value) @@>))
            ptyp
        | Ast.ValueType.Date ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Date", Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewProperty("Year", typeof<int>, fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, _, year = (%%e : int * int * int) in year @@>))
            ptyp.AddMember(pdb.NewProperty("Month", typeof<int>, fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, month, _ = (%%e : int * int * int) in month @@>))
            ptyp.AddMember(pdb.NewProperty("Day", typeof<int>, fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let day, _, _ = (%%e : int * int * int) in day @@>))
            ptyp.AddMember(pdb.NewConstructor([("Day", typeof<int>); ("Month", typeof<int>); ("Year", typeof<int>)], fun [day; month; year] ->
                <@@ Ast.Value.Date((%%day : int), (%%month : int), (%%year : int)) @@>))
            ptyp
        | Ast.ValueType.Composite fields ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Composite" |> newName, Some (typeof<Ast.Value>))
            let asList this = <@ (%%this : Ast.Value).GetItems() @>
            let getters() =
                fields
                |> Map.map (
                    fun fieldName (def, minMult, maxMult) ->
                        let fieldType =
                            let subName = getNameOfField(fieldName, def)
                            getProvidedType(subName, def)
                        match (minMult, maxMult) with
                        | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                            let prop = new ProvidedProperty(fieldName, fieldType)
                            prop.GetterCode <-
                                fun [this] ->
                                    let e = asList this
                                    <@@
                                        match List.tryFind (fun (name, _) -> name = fieldName) %e with
                                        | Some (_, value) -> value
                                        | None -> failwithf "Field '%s' is not set" fieldName
                                    @@>
                            prop
                        | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                            let optTyp =
                                typeof<option<_>>
                                    .GetGenericTypeDefinition()
                                    .MakeGenericType(fieldType)
                            let prop = new ProvidedProperty(fieldName, optTyp)
                            prop.GetterCode <-
                                fun [this] ->
                                    let e = asList this
                                    <@@
                                        match List.tryFind (fun (name, _) -> name = fieldName) %e with
                                        | Some (_, value) -> Some value
                                        | None -> None
                                    @@>
                            prop                                    
                        | _, Ast.MaxMultiplicity.Multiple ->
                            let listTyp =
                                typeof<List<_>>
                                    .GetGenericTypeDefinition()
                                    .MakeGenericType(fieldType)
                            let prop = new ProvidedProperty(fieldName, listTyp)
                            prop.GetterCode <-
                                fun [this] ->
                                    let e = asList this
                                    <@@
                                        List.filter (fun (name, _) -> name = fieldName) %e
                                    @@>
                            prop
                    )
                |> Map.toList
                |> List.sortBy fst
                |> List.map snd                
            ptyp.AddMembersDelayed(getters)
            // setters, using fluent interfaces
            let setters() =
                fields
                |> Seq.map(fun kvp ->
                    let fieldName = kvp.Key
                    let (def, minMult, maxMult) = kvp.Value
                    let fieldType =
                        let subName =
                            match def with
                            | Ast.ValueType.Set _
                            | Ast.ValueType.Mapping _
                            | Ast.ValueType.Composite _ -> Some fieldName
                            | _ -> None
                        getProvidedType(subName, def)
                    match (minMult, maxMult) with
                    | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                        pdb.NewMethod(
                            sprintf "Set%s" fieldName,
                            ptyp,
                            [("value", upcast fieldType)],
                            fun [this; value] ->
                                <@@
                                    let this = (%%this : Ast.Value)
                                    this.SetItem(fieldName, (%%value : Ast.Value))
                                @@>)
                    | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                        let optTyp =
                            typedefof<option<_>>
                                .MakeGenericType(fieldType)
                        pdb.NewMethod(
                            sprintf "Set%s" fieldName,
                            ptyp,
                            [("value", optTyp)],
                            fun [this; value] ->
                                <@@
                                    let this = (%%this : Ast.Value)
                                    this.SetItem(fieldName, (%%value : Ast.Value option))
                                @@>)
                    | _, Ast.MaxMultiplicity.Multiple ->
                        let listTyp =
                            typedefof<List<_>>
                                .MakeGenericType(fieldType)
                        pdb.NewMethod(
                            sprintf "Set%s" fieldName,
                            ptyp,
                            [("value", listTyp)],
                            fun [this; value] ->
                                <@@
                                    let this = (%%this : Ast.Value)
                                    this.ClearItems(fieldName).AddItems(fieldName, (%%value : Ast.Value list))
                                @@>)
                    |> addXmlDoc (sprintf """<summary>Create a copy of this, with the value of field '%s' changed to <paramref name="value" />.</summary>""" fieldName))
                |> List.ofSeq
            ptyp.AddMembersDelayed(setters)
            // Create Mcu instances
            let asMcu() =
                [
                    match name with
                    | Some name ->
                        match McuFactory.tryMkAsComplex(name, typ) with
                        | Some _ ->
                            yield
                                pdb.NewMethod(
                                    "CreateMcuComplex",
                                    typeof<Mcu.McuComplex>,
                                    [],
                                    fun [this] ->
                                        <@@
                                            match McuFactory.tryMkAsComplex(name, %typExpr) with
                                            | Some f -> f((%%this : Ast.Value), [])
                                            | None -> failwith "Unexpected error: could not build AsComplex"
                                        @@>)
                                |> addXmlDoc """<summary>Create a new mutable instance of a complex trigger.</summary>"""
                        | None -> ()
                        match McuFactory.tryMkAsTrigger(name, typ) with
                        | Some _ ->
                            yield
                                pdb.NewMethod(
                                    "CreateMcuCommand",
                                    typeof<Mcu.McuTrigger>,
                                    [],
                                    fun [this] ->
                                        <@@
                                            match McuFactory.tryMkAsTrigger(name, %typExpr) with
                                            | Some f -> f((%%this : Ast.Value), [])
                                            | None -> failwith "Unexpected error: could not build AsCommand"
                                        @@>)
                                |> addXmlDoc """<summary>Create a new mutable instance of an MCU command.</summary>"""
                        | None -> ()
                        match McuFactory.tryMkAsEntity(name, typ) with
                        | Some _ ->
                            yield
                                pdb.NewMethod(
                                    "CreateEntity",
                                    typeof<Mcu.McuEntity>,
                                    [],
                                    fun [this] ->
                                        <@@
                                            match McuFactory.tryMkAsEntity(name, %typExpr) with
                                            | Some f -> f((%%this : Ast.Value), [])
                                            | None -> failwith "Unexpected error: could not build AsEntity"
                                        @@>)
                                |> addXmlDoc """<summary>Create a new mutable instance of an entity.</summary>"""
                        | None -> ()
                        match McuFactory.tryMkAsHasEntity(name, typ) with
                        | Some _ ->
                            yield
                                pdb.NewMethod(
                                    "CreateHasEntity",
                                    typeof<Mcu.HasEntity>,
                                    [],
                                    fun [this] ->
                                        <@@
                                            match McuFactory.tryMkAsHasEntity(name, %typExpr) with
                                            | Some f -> f((%%this : Ast.Value), [])
                                            | None -> failwith "Unexpected error: could not build AsHasEntity"
                                        @@>)
                                |> addXmlDoc """<summary>Create a new mutable instance of a plane, vehicle, building...</summary>"""
                        | None -> ()
                    | None ->
                        ()
                ]
            ptyp.AddMembersDelayed(asMcu)
            // Dump to text
            match name with
            | Some name ->
                let meth = pdb.NewMethod("AsString", typeof<string>, [], fun [this] ->
                    <@@
                        name + Ast.dump (%%this : Ast.Value)
                    @@>)
                ptyp.AddMember(meth)
            | None ->
                ()
            ptyp
        | Ast.ValueType.Mapping itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Mapping" |> newName, Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewConstructor([], fun [] -> <@@ Ast.Value.Mapping [] @@>))
            let propTyp = typedefof<Map<_,_>>.MakeGenericType(typeof<int>, ptyp1)
            ptyp.AddMember(pdb.NewProperty("Value", propTyp, fun this -> <@@ (%%this : Ast.Value).GetMapping() |> Map.ofList @@>))
            ptyp.AddMember(pdb.NewMethod("SetItem", ptyp, [("Key", typeof<int>); ("Value", upcast ptyp1)], fun [this; key; value] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.SetItem((%%key : int), (%%value : Ast.Value))
                @@>))
            ptyp.AddMember(pdb.NewMethod("RemoveItem", ptyp, ["Key", typeof<int>], fun [this; key] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.RemoveItem(%%key : int)
                @@>))
            ptyp.AddMember(pdb.NewMethod("Clear", ptyp, [], fun [this] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.Clear()
                @@>))
            ptyp
        | Ast.ValueType.Set itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Set" |> newName, Some (typeof<Ast.Value>))
            ptyp.AddMember(pdb.NewConstructor([], fun [] -> <@@ Ast.Value.Set [] @@>))
            let propTyp = typedefof<Set<_>>.MakeGenericType(ptyp1)
            ptyp.AddMember(pdb.NewProperty("Value", propTyp, fun this -> <@@ (%%this : Ast.Value).GetSet() |> Set.ofList @@>))
            ptyp

    and getProvidedType (name, typ) : ProvidedTypeDefinition =
        cached cache (buildProvidedType) (name, typ)

    getProvidedType, cache

/// <summary>
/// Build the definition of the provided type that offers parsing of values.
/// </summary>
/// <param name="namedValueTypes">List of types, ValueTypes and their provided type definition.</param>
let buildParserType (pdb : IProvidedDataBuilder) (namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
    let parser =
        ProvidedTypeDefinition("Parser", Some typeof<IDictionary<Ast.ValueType, Parsing.ParserFun>>)
        |> addXmlDoc """<summary>Parser for the types found in the sample mission.</summary>"""
    let valueTypeExprs =
        namedValueTypes
        |> List.map (fun (_, x, _) -> x)
        |> List.fold (fun expr valueType ->
            let valueTypeExpr = valueType.ToExpr()
            <@ %valueTypeExpr :: %expr @>
            ) <@ [] @>
    // Default constructor: Populate the cache of parsers.
    parser.AddMember(
        pdb.NewConstructor([], fun [] ->
            <@@
                %valueTypeExprs
                |> List.map(fun valueType -> (valueType, Parsing.makeParser valueType))
                |> dict
            @@>)
        |> addXmlDoc """<summary>Build a new parser.</summary>""")
    // Parse methods for all top types.
    for name, valueType, ptyp in namedValueTypes do
        let vtExpr = valueType.ToExpr()
        let retType =
            typedefof<_*_>.MakeGenericType(ptyp, typeof<Parsing.Stream>)
        parser.AddMember(
            pdb.NewMethod(sprintf "Parse_%s" name, retType, [("s", typeof<Parsing.Stream>)], fun [this; s] ->
                <@@
                    let parsers = (%%this : IDictionary<Ast.ValueType, Parsing.ParserFun>)
                    let parser = parsers.[%vtExpr]
                    parser.Run(%%s : Parsing.Stream)
                @@>)
            |> addXmlDoc (sprintf """
<summary>Parse an instance of %s</summary>
<param name="s">Stream from which the instance is parsed</param>
<return>A pair of the stream after the parsed section and the data resulting from parsing.</return>
<exception cref="Parsing.ParseError">Parsing failed.</exception>
""" name))
    parser

/// Type controlling the kind of provided property returned by buildAsMcuList: instance-bound or static.
type DataListSource =
    | Instance of this: (Expr -> Expr<Ast.Data list>)
    | Static of code: Expr<Ast.Data list>

/// <summary>
/// Build the provided property that returns a list of objects implementing McuBase and its subtypes.
/// </summary>
/// <param name="dataListSource">Specifies where the data is retrieved from: From 'this' for an instance-bound property, from an expression for a static property.</param>
/// <param name="namedValueTypes">List of ValueTypes with their name and their provided type definition.</param>
let buildAsMcuList (pdb : IProvidedDataBuilder) (dataListSource : DataListSource) (namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
    let valueTypeOfName =
        namedValueTypes
        |> List.fold (fun expr (name, valueType, _) ->
            <@
                Map.add name %(valueType.ToExpr()) %expr
            @>
            ) <@ Map.empty @>
    let names =
        namedValueTypes
        |> List.map (fun (name, _, _) -> name)
        |> List.fold (fun expr name ->
            <@ name :: %expr @>) <@ [] @>

    let mkBody (dataList : Expr<Ast.Data list>) =
        <@@
            let this = %dataList
            let valueTypeOfName = %valueTypeOfName
            let mcuMakerOfName =
                %names
                |> List.map (fun name ->
                    let valueType = valueTypeOfName.[name]
                    (name, McuFactory.tryMakeMcu(name, valueType))
                )
                |> Map.ofList
            this
            |> List.map (fun data -> data.GetLeavesWithPath())
            |> List.concat
            |> List.choose (fun (path, name, value) ->
                match Map.tryFind name mcuMakerOfName with
                | Some(Some(make)) ->
                    make(value, path)
                    |> Some
                | _ -> // Cannot build an Mcu from that valueType
                    None
            )
        @@>

    match dataListSource with
    | Static expr ->
        pdb.NewStaticMethod("CreateMcuList", typeof<Mcu.McuBase list>, [], fun [] -> mkBody expr)
    | Instance getDataList ->
        pdb.NewMethod("CreateMcuList", typeof<Mcu.McuBase list>, [], fun [this] ->
            let dataList : Expr<Ast.Data list> = getDataList this
            mkBody dataList
        )

/// <summary>
/// Build the provided type definition of the type that offers parsing of mission files.
/// </summary>
/// <param name="namedValueTypes">ValueTypes with their name and provided type definition.</param>
let buildGroupParserType (pdb : IProvidedDataBuilder) (namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
    let parser =
        ProvidedTypeDefinition("GroupData", Some typeof<Ast.Data list>)
        |> addXmlDoc """Extraction of data from a mission or group file."""
    let valueTypeOfName =
        namedValueTypes
        |> List.fold (fun expr (name, valueType, _) ->
            <@
                Map.add name %(valueType.ToExpr()) %expr
            @>
            ) <@ Map.empty @>
    let names =
        namedValueTypes
        |> List.map (fun (name, _, _) -> name)
        |> List.fold (fun expr name ->
            <@ name :: %expr @>) <@ [] @>

    // Constructor: Parse a group or mission file
    parser.AddMemberDelayed(fun() ->
        pdb.NewConstructor([("s", typeof<Parsing.Stream>)], fun [s] ->
            <@@
                let parsers =
                    %valueTypeOfName
                    |> Map.map (fun name valueType -> Parsing.makeParser valueType)
                let getParser name = parsers.[name]
                let s = (%%s : Parsing.Stream)
                Parsing.parseFile getParser s
            @@>)
        |> addXmlDoc """
            <summary>Parse a mission or group file and store the extracted data.</summary>
            <param name="s">The stream that is parsed</param>
            <exception cref="Parsing.ParseError">Failed to parse the mission or group</exception>""")
    // Getters: list of objects of each type
    for (name, valueType, ptyp) in namedValueTypes do
        parser.AddMemberDelayed(fun() ->
            pdb.NewProperty(sprintf "ListOf%s" name, typedefof<_ list>.MakeGenericType(ptyp), fun this ->
                <@@
                    let this = (%%this : Ast.Data list)
                    let ret =
                        this
                        |> List.map (fun data -> data.GetLeaves())
                        |> List.concat
                        |> List.choose (function (name2, value) -> if name2 = name then Some value else None)
                    ret
                @@>)
            |> addXmlDoc (sprintf """<summary>Build a list of immutable instances of %s</summary>""" name))
    // Get the flattened list of objects as instances of McuBase and its subtypes, when appropriate
    parser.AddMemberDelayed(fun() ->
        buildAsMcuList pdb (Instance(fun this -> <@ (%%this : Ast.Data list) @>)) namedValueTypes
        |> addXmlDoc """<summary>Build a list of mutable instances of McuBase from the extracted data.</summary>""")
    // Return result
    parser

/// <summary>
/// Build definitions of provided types which expose the names of nodes in mission files using static properties.
/// This useful to use auto-completion and detect typos in node names in an IDE.
/// </summary>
/// <param name="namedValueTypes">ValueTypes with their names and provided type definitions.</param>
/// <param name="files">Semi-colon-separated list of mission file names.</param>
let buildLibraries (pdb : IProvidedDataBuilder) (namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) (files : string[]) =
    let parsers =
        namedValueTypes
        |> List.map (fun (name, typ, _) -> (name, Parsing.makeParser typ))
        |> Map.ofList
    let getParser name = parsers.[name]
    let types =
        namedValueTypes
        |> List.map (fun (name, _, ptyp) -> (name, ptyp))
        |> Map.ofList
    let importFile filename =
        let newName =
            let rand = new Random(0)
            fun baseName ->
                let baseName =
                    if String.IsNullOrEmpty baseName then "Unnamed" else baseName
                Seq.initInfinite (fun i ->
                    if i = 0 then
                        baseName
                    elif i < 10 then
                        sprintf "%s_%d" baseName (i + 1)
                    else
                        sprintf "%s_R%d" baseName (rand.Next())
                    )
            |> getNameStore
        let name = Path.GetFileNameWithoutExtension(filename)
        let lib = new ProvidedTypeDefinition(name, Some typeof<obj>)
        fun () ->
            let data =
                try
                    let s = Parsing.Stream.FromFile filename
                    Parsing.parseFile getParser s
                    |> Choice1Of2
                with
                | :? Parsing.ParseError as e ->
                    Parsing.printParseError e
                    |> String.concat "\n"
                    |> Choice2Of2
                | e ->
                    e.Message
                    |> Choice2Of2
            let staticMembers =
                match data with
                | Choice1Of2 data ->
                    let rec work (data : Ast.Data) : ((string * Type) * Quotations.Expr * string option) seq =
                        seq {
                            match data with
                            | Ast.Data.Leaf(typename, value) ->
                                match value with
                                | Ast.Value.Composite fields ->
                                    let name =
                                        fields
                                        |> Seq.tryPick (function ("Name", Ast.Value.String n) -> Some n | _ -> None)
                                    match name with
                                    | Some name ->
                                        let desc =
                                            fields
                                            |> Seq.tryPick (function ("Desc", Ast.Value.String s) -> Some s | _ -> None)
                                        let valueExpr = <@ name @>
                                        yield (newName name, typeof<string>), <@@ %valueExpr @@>, desc
                                    | None ->
                                        ()
                                | _ ->
                                    ()
                            | Ast.Data.Group groupData ->
                                yield!
                                    groupData.Data
                                    |> Seq.map work
                                    |> Seq.concat
                        }
                    data
                    |> List.map work
                    |> Seq.concat
                    |> List.ofSeq
                | Choice2Of2 msg ->
                // If something went wrong, the property is a string describing the error that occurred.
                    [ (("LoadingError", typeof<string>), <@@ msg @@>, Some msg) ]
            let staticProperties : Reflection.MemberInfo list =
                staticMembers
                |> List.map (fun (prop, expr, doc) ->
                    let p = pdb.NewStaticProperty(fst prop, snd prop, expr)
                    let p =
                        match doc with
                        | Some doc -> addXmlDoc (sprintf "<summary>%s</summary>" doc) p
                        | None -> p
                    upcast p)
            staticProperties
        |> lib.AddMembersDelayed
        lib

    files
    |> Array.map importFile
    |> List.ofArray


[<TypeProvider>]
/// Entry point of the type provider.
type MissionTypes(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "SturmovikMissionTypes"

    let provider = ProvidedTypeDefinition(asm, ns, "Provider", Some(typeof<obj>))
    do provider.AddXmlDoc("""
    <summary>
    Exposes typed data from "IL2 Sturmovik: Battle of Stalingrad" mission files.
    </summary>
    <param name="sample">
    Name of a mission file from which the structure of missions is infered.
    </param>
    <param name="library">
    List of mission or group files from which to read data, separated by semi-colons.
    </param>
    """)
    let sampleParam = ProvidedStaticParameter("sample", typeof<string>)
    let libraryParam = ProvidedStaticParameter("library", typeof<string>)
    let invokeCodeImplParam = ProvidedStaticParameter("invokeCodeImpl", typeof<InvokeCodeImplementation>, parameterDefaultValue = InvokeCodeImplementation.FromAssembly)
    let buildProvider(typeName : string, sample : string, libs : string[], invokeImpl : InvokeCodeImplementation) =
        let pdb = mkProvidedDataBuilder invokeImpl
        let ty = new ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        // The types corresponding to the ValueTypes extracted from the sample file
        let getProvidedType, cache = mkProvidedTypeBuilder pdb ty
        let types, _ = AutoSchema.getTopTypes(Parsing.Stream.FromFile(sample))
        for t in types do
            ignore <| getProvidedType(Some t.Key, t.Value)
            match t.Value with
            | Ast.Composite fields ->
                for kvp in fields do
                    let subT, _, _ = kvp.Value
                    let subName = getNameOfField(kvp.Key, subT)
                    ignore <| getProvidedType(subName, subT)
        let types =
            cache
            |> Seq.map (fun kvp -> kvp.Value)
            |> List.ofSeq
        ty.AddMembers(types)
        // The type of the parser.
        let namedTypes =
            cache
            |> Seq.choose (fun kvp ->
                match fst kvp.Key with
                | Some name -> Some (name, snd kvp.Key, kvp.Value)
                | None -> None)
            |> List.ofSeq
        let parserType = buildParserType pdb namedTypes
        ty.AddMember(parserType)
        // The type of the file parser.
        let parserType = buildGroupParserType pdb namedTypes
        ty.AddMember(parserType)
        // The libraries
        let plibs = buildLibraries pdb namedTypes libs
        ty.AddMembers(plibs)
        // Resolution folder
        let resFolder = pdb.NewStaticProperty("ResolutionFolder", typeof<string>, Expr.Value(config.ResolutionFolder))
        resFolder.AddXmlDoc("""
        <summary>
        Location of the resolution folder, the folder used to root relative paths provided in the type provider's constructor.
        </summary>
        """)
        ty.AddMember(resFolder)
        // File modification times
        let modifs =
            [
                yield FileWithTime.File.FromFile sample
                for lib in libs do
                    yield FileWithTime.File.FromFile lib
            ]
        // Result
        ty, modifs

    // Cache the top provided type definitions.
    let cache = new Dictionary<(string * string * string[] * InvokeCodeImplementation), ProvidedTypeDefinition * (FileWithTime.File list)>(HashIdentity.Structural)
    let getProvider = cached cache buildProvider
    do provider.DefineStaticParameters([sampleParam; libraryParam; invokeCodeImplParam], fun typeName [| sample; libs; invokeCodeImpl |] ->
        let resolve path =
            if Path.IsPathRooted(path) then
                path
            else
                Path.Combine(config.ResolutionFolder, path)
        let sample = sample :?> string |> resolve
        let libs = libs :?> string
        let libs = libs.Split(';') |> Array.map resolve
        let invokeCodeImpl = invokeCodeImpl :?> InvokeCodeImplementation

        if not(System.IO.File.Exists(sample)) then
            failwithf "Cannot open sample file '%s' for reading" sample
        // Check if modifications were made to input files
        let ty, modifs = getProvider(typeName, sample, libs, invokeCodeImpl)
        let modifs2 =
            [
                yield FileWithTime.File.FromFile sample
                for lib in libs do
                    yield FileWithTime.File.FromFile lib
            ]
        // If so, remove the entry from the cache, invalidate the top provided and build it again.
        if modifs <> modifs2 then
            cache.Remove((typeName, sample, libs, invokeCodeImpl)) |> ignore
            this.Invalidate()
            let ty, _ = getProvider(typeName, sample, libs, invokeCodeImpl)
            ty
        else
            ty
    )

    do this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do()
