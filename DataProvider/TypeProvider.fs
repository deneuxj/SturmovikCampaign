module SturmovikMission.DataProvider.TypeProvider

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System

let cached (cache : IDictionary<'a, 'b>) f x =
    match cache.TryGetValue(x) with
    | true, y -> y
    | false, _ ->
        let y = f x
        cache.Add(x, y)
        y

let getNameStore getValidNames =
    let store = ref <| Set []
    let isValid name =
        Set.contains name !store
        |> not
    fun baseName ->
        let name =
            getValidNames baseName
            |> Seq.find isValid
        store := Set.add name !store
        name

let mkProvidedTypeBuilder()=
    let newName =
        fun baseName ->
            Seq.initInfinite (fun i -> sprintf "%s_%d" baseName i)
        |> getNameStore

    let cache = new Dictionary<Ast.ValueType, ProvidedTypeDefinition>(HashIdentity.Structural)

    let rec buildProvidedTypeNonCached (typ : Ast.ValueType) =
        let rec work (typ : Ast.ValueType) =
            match typ with
            | Ast.ValueType.Boolean ->
                let ptyp =
                    new ProvidedTypeDefinition("Boolean", Some (typeof<Ast.Value>))
                let asBool this =
                    <@@
                        match (%%this : obj) :?> Ast.Value with
                        | Ast.Value.Boolean b -> b
                        | _ -> failwith "Underlying type is not Boolean"
                    @@>
                let value =
                    new ProvidedProperty("Value", typeof<bool>)
                value.GetterCode <-
                    fun [this] -> asBool this
                ptyp.AddMember(value)
                ptyp
            | Ast.ValueType.Float ->
                let ptyp =
                    new ProvidedTypeDefinition("Float", Some (typeof<Ast.Value>))
                let asFloat this =
                    <@@
                        match (%%this : obj) :?> Ast.Value with
                        | Ast.Value.Float f -> f
                        | _ -> failwith "Underlying type is not Float"
                    @@>
                let value =
                    new ProvidedProperty("Value", typeof<float>)
                value.GetterCode <-
                    fun [this] -> asFloat this
                ptyp.AddMember(value)
                ptyp
            | Ast.ValueType.Integer ->
                let ptyp =
                    new ProvidedTypeDefinition("Integer", Some (typeof<Ast.Value>))
                let asInt this =
                    <@@
                        match (%%this : obj) :?> Ast.Value with
                        | Ast.Value.Integer n -> n
                        | _ -> failwith "Underlying type is not Integer"
                    @@>
                let value =
                    new ProvidedProperty("Value", typeof<int>)
                value.GetterCode <-
                    fun [this] -> asInt this
                ptyp.AddMember(value)
                ptyp
            | Ast.ValueType.String ->
                let ptyp =
                    new ProvidedTypeDefinition("String", Some (typeof<Ast.Value>))
                let asString this =
                    <@@
                        match (%%this : obj) :?> Ast.Value with
                        | Ast.Value.String s -> s
                        | _ -> failwith "Underlying type is not String"
                    @@>
                let value =
                    new ProvidedProperty("Value", typeof<string>)
                value.GetterCode <-
                    fun [this] -> asString this
                ptyp.AddMember(value)
                ptyp
            | Ast.ValueType.IntVector ->
                let ptyp =
                    new ProvidedTypeDefinition("VectorOfIntegers", Some (typeof<Ast.Value>))
                let asIntList this =
                    <@@
                        match (%%this : obj) :?> Ast.Value with
                        | Ast.Value.IntVector xs -> xs
                        | _ -> failwith "Underlying type is not IntVector"
                    @@>
                let value =
                    new ProvidedProperty("Value", typeof<int list>)
                value.GetterCode <-
                    fun [this] -> asIntList this
                ptyp.AddMember(value)
                ptyp
            | Ast.ValueType.Pair (typ1, typ2) ->
                let ptyp =
                    new ProvidedTypeDefinition("Pair", Some (typeof<Ast.Value>))
                let unwrap this =
                    <@@
                        match (%%this : obj) :?> Ast.Value with
                        | Ast.Value.Pair (item1, item2) -> (item1, item2)
                        | _ -> failwith "Underlying type is not Pair"
                    @@>
                let value =
                    let ptyp1 = getProvidedType typ1
                    let ptyp2 = getProvidedType typ2
                    let propTyp =
                        typeof<_*_>
                            .GetGenericTypeDefinition()
                            .MakeGenericType(ptyp1, ptyp2)
                    new ProvidedProperty("Value", propTyp)
                value.GetterCode <-
                    fun [this] -> unwrap this
                ptyp.AddMember(value)
                ptyp
            | Ast.ValueType.Triplet (typ1, typ2, typ3) ->
                let ptyp =
                    new ProvidedTypeDefinition("Triplet", Some (typeof<Ast.Value>))
                let unwrap this =
                    <@@
                        match (%%this : obj) :?> Ast.Value with
                        | Ast.Value.Triplet (item1, item2, item3) -> (item1, item2, item3)
                        | _ -> failwith "Underlying type is not Triplet"
                    @@>
                let value =
                    let ptyp1 = getProvidedType typ1
                    let ptyp2 = getProvidedType typ2
                    let ptyp3 = getProvidedType typ3
                    let propTyp =
                        typeof<_*_*_>
                            .GetGenericTypeDefinition()
                            .MakeGenericType(ptyp1, ptyp2, ptyp3)
                    new ProvidedProperty("Value", propTyp)
                value.GetterCode <-
                    fun [this] -> unwrap this
                ptyp.AddMember(value)
                ptyp
            | Ast.ValueType.Date ->
                let ptyp =
                    new ProvidedTypeDefinition("Date", Some (typeof<Ast.Value>))
                let year =
                    new ProvidedProperty("Year", typeof<int>)
                let month =
                    new ProvidedProperty("Month", typeof<int>)
                let day =
                    new ProvidedProperty("Day", typeof<int>)
                let asTuple this =
                    <@@
                        match (%%this : obj) with
                        | :? Ast.Value as v ->
                            match v with
                            | Ast.Value.Date (year, month, day) -> (year, month, day)
                            | _ -> failwith "Erased type is a Value, but not a Date"
                        | _ ->
                            failwith "Erased type is not a Value"
                    @@>                        
                year.GetterCode <-
                    function
                    | [this] ->
                        let e = asTuple this
                        <@@ let year, _, _ = %%e in year @@>
                    | _ ->
                        failwith "Unexpected signature of getter for Date"
                month.GetterCode <-
                    function
                    | [this] ->
                        let e = asTuple this
                        <@@ let _, month, _ = %%e in month @@>
                    | _ ->
                        failwith "Unexpected signature of getter for Date"
                day.GetterCode <-
                    function
                    | [this] ->
                        let e = asTuple this
                        <@@ let _, _, day = %%e in day @@>
                    | _ ->
                        failwith "Unexpected signature of getter for Date"
                ptyp.AddMembers([year; month; day])
                ptyp
            | Ast.ValueType.Composite fields ->
                let ptyp =
                    new ProvidedTypeDefinition(newName "Composite", Some (typeof<Ast.Value>))
                let asList this =
                    <@@
                        match (%%this : obj) with
                        | :? Ast.Value as v ->
                            match v with
                            | Ast.Value.Composite content -> content
                            | _ -> failwith "Erased type is a Value, but not a Composite"
                        | _ ->
                            failwith "Erased type is not a Value"
                    @@>
                let members =
                    fields
                    |> Map.map (
                        fun fieldName (def, minMult, maxMult) ->
                            let fieldType =
                                getProvidedType def
                            match (minMult, maxMult) with
                            | Ast.MinMultiplicity.MinOne, Ast.MaxMultiplicity.MaxOne ->
                                let prop = new ProvidedProperty(fieldName, fieldType)
                                prop.GetterCode <-
                                    fun [this] ->
                                        let e = asList this
                                        <@@
                                            match List.tryFind (fun (name, _) -> name = fieldName) %%e with
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
                                            match List.tryFind (fun (name, _) -> name = fieldName) %%e with
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
                                            List.filter (fun (name, _) -> name = fieldName) %%e
                                        @@>
                                prop
                        )
                    |> Map.toList
                    |> List.sortBy fst
                    |> List.map snd
                ptyp.AddMembers(members)
                ptyp
            | Ast.ValueType.Mapping itemTyp ->
                let ptyp1 = getProvidedType itemTyp
                let ptyp =
                    new ProvidedTypeDefinition(newName "Mapping", Some (typeof<Ast.Value>))
                let content =
                    new ProvidedField(
                        "Content",
                        typeof<Map<_,_>>
                            .GetGenericTypeDefinition()
                            .MakeGenericType(typeof<int>, ptyp1))
                ptyp.AddMember(content)
                ptyp
            | Ast.ValueType.Set itemTyp ->
                let ptyp1 = getProvidedType itemTyp
                let ptyp =
                    new ProvidedTypeDefinition(newName "Set", Some (typeof<Ast.Value>))
                let content =
                    new ProvidedField(
                        "Content",
                        typeof<Set<_>>
                            .GetGenericTypeDefinition()
                            .MakeGenericType(ptyp1))
                ptyp.AddMember(content)
                ptyp
        work typ

    and getProvidedType typ =
        cached cache (buildProvidedTypeNonCached) typ

    getProvidedType


[<TypeProvider>]
type MissionTypes(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "SturmovikMission"
    let getProvidedType = mkProvidedTypeBuilder()

    let provider = ProvidedTypeDefinition(asm, ns, "Provider", Some(typeof<obj>))
    let sampleParam = ProvidedStaticParameter("sample", typeof<string>)
    do sampleParam.AddXmlDoc("<brief>Name of a mission file from which the structure of missions is infered</brief>")

    do provider.DefineStaticParameters([sampleParam], fun typeName [| sample |] ->
        let sample = sample :?> string
        if not(System.IO.File.Exists(sample)) then
            failwithf "Cannot open sample file '%s' for reading" sample
        let types, _ = AutoSchema.getTopTypes(Parsing.Stream.FromFile(sample))
        let types =
            types
            |> Map.map (fun name typ -> getProvidedType typ)
            |> Map.toSeq
            |> Seq.map snd
            |> List.ofSeq
        let ty = new ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        ty.AddMembers(types)
        ty
    )

    do this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do()
