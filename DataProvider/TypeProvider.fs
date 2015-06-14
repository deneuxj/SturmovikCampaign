module SturmovikMission.DataProvider.TypeProvider

#nowarn "25"

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
    let reserved =
        [ "Boolean"; "Float"; "Integer"; "String"; "VectorOfIntefers"; "Date" ]
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

type PairParser(typ1, typ2) =
    let parse = Parsing.parsePair (typ1, typ2)
    member this.Run(s) = parse s

let mkProvidedTypeBuilder()=
    let newName =
        fun baseName ->
            Seq.initInfinite (fun i -> if i = 0 then baseName else sprintf "%s_%d" baseName (i + 1))
        |> getNameStore

    let cache = new Dictionary<string option * Ast.ValueType, ProvidedTypeDefinition>(HashIdentity.Structural)

    let newParserMethod (ptyp : ProvidedTypeDefinition) =
        let x =
            new ProvidedMethod(
                "Parse",
                [ ProvidedParameter("s", typeof<Parsing.Stream>) ],
                Microsoft.FSharp.Reflection.FSharpType.MakeTupleType([|ptyp; typeof<Parsing.Stream>|]))
        x.IsStaticMethod <- true
        x

    let rec buildProvidedType (name : string option, typ : Ast.ValueType) =
        match typ with
        | Ast.ValueType.Boolean ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Boolean", Some (typeof<Ast.Value>))
            let value =
                new ProvidedProperty("Value", typeof<bool>)
            value.GetterCode <-
                fun [this] -> <@@ (%%this : Ast.Value).GetBool() @@>
            ptyp.AddMember(value)
            let parseM = newParserMethod ptyp
            parseM.InvokeCode <-
                fun [s] -> <@@ Parsing.parseBool %%s @@>
            ptyp.AddMember(parseM)
            ptyp
        | Ast.ValueType.Float ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Float", Some (typeof<Ast.Value>))
            let value =
                new ProvidedProperty("Value", typeof<float>)
            value.GetterCode <-
                fun [this] -> <@@ (%%this : Ast.Value).GetFloat() @@>
            ptyp.AddMember(value)
            let parseM = newParserMethod ptyp
            parseM.InvokeCode <-
                fun [s] -> <@@ Parsing.parseFloat %%s @@>
            ptyp.AddMember(parseM)
            ptyp
        | Ast.ValueType.Integer ->
            let ptyp =
                new ProvidedTypeDefinition("Integer", Some (typeof<Ast.Value>))
            let value =
                new ProvidedProperty("Value", typeof<int>)
            value.GetterCode <-
                fun [this] -> <@@ (%%this : Ast.Value).GetInteger() @@>
            ptyp.AddMember(value)
            let parseM = newParserMethod ptyp
            parseM.InvokeCode <-
                fun [s] -> <@@ Parsing.parseInteger %%s @@>
            ptyp.AddMember(parseM)
            ptyp
        | Ast.ValueType.String ->
            let ptyp =
                new ProvidedTypeDefinition("String", Some (typeof<Ast.Value>))
            let value =
                new ProvidedProperty("Value", typeof<string>)
            value.GetterCode <-
                fun [this] -> <@@ (%%this : Ast.Value).GetString() @@>
            ptyp.AddMember(value)
            let parseM = newParserMethod ptyp
            parseM.InvokeCode <-
                fun [s] -> <@@ Parsing.parseString %%s @@>
            ptyp.AddMember(parseM)
            ptyp
        | Ast.ValueType.IntVector ->
            let ptyp =
                new ProvidedTypeDefinition("VectorOfIntegers", Some (typeof<Ast.Value>))
            let value =
                new ProvidedProperty("Value", typeof<int list>)
            value.GetterCode <-
                fun [this] -> <@@ (%%this : Ast.Value).GetIntVector() @@>
            ptyp.AddMember(value)
            let parseM = newParserMethod ptyp
            parseM.InvokeCode <-
                fun [s] -> <@@ Parsing.parseIntVector %%s @@>
            ptyp.AddMember(parseM)
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
            let unwrap this = <@@ (%%this : Ast.Value).GetPair() @@>
            let value =
                let propTyp =
                    typeof<_*_>
                        .GetGenericTypeDefinition()
                        .MakeGenericType(ptyp1, ptyp2)
                new ProvidedProperty("Value", propTyp)
            value.GetterCode <-
                fun [this] -> unwrap this
            ptyp.AddMember(value)
            let parseM = newParserMethod ptyp
            parseM.InvokeCode <-                
                let parser = Parsing.makeParser typ
                let f s = parser.Run(s)
                fun [s] ->
                    <@@
                        f %%s
                    @@>
            ptyp.AddMember(parseM)
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
            let value =
                let propTyp =
                    typeof<_*_*_>
                        .GetGenericTypeDefinition()
                        .MakeGenericType(ptyp1, ptyp2, ptyp3)
                new ProvidedProperty("Value", propTyp)
            value.GetterCode <-
                fun [this] -> <@@ (%%this : Ast.Value).GetTriplet() @@>
            ptyp.AddMember(value)
            ptyp
        | Ast.ValueType.Date ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Date", Some (typeof<Ast.Value>))
            let year =
                new ProvidedProperty("Year", typeof<int>)
            let month =
                new ProvidedProperty("Month", typeof<int>)
            let day =
                new ProvidedProperty("Day", typeof<int>)
            year.GetterCode <-
                function
                | [this] ->
                    let e = <@@ (%%this : Ast.Value).GetDate() @@>
                    <@@ let _, _, year = (%%e : int * int * int) in year @@>
                | _ ->
                    failwith "Unexpected signature of getter for Date"
            month.GetterCode <-
                function
                | [this] ->
                    let e = <@@ (%%this : Ast.Value).GetDate() @@>
                    <@@ let _, month, _ = (%%e : int * int * int) in month @@>
                | _ ->
                    failwith "Unexpected signature of getter for Date"
            day.GetterCode <-
                function
                | [this] ->
                    let e = <@@ (%%this : Ast.Value).GetDate() @@>
                    <@@ let day, _, _ = (%%e : int * int * int) in day @@>
                | _ ->
                    failwith "Unexpected signature of getter for Date"
            ptyp.AddMembers([year; month; day])
            let parseM = newParserMethod ptyp
            parseM.InvokeCode <-
                fun [s] -> <@@ Parsing.parseDate %%s @@>
            ptyp.AddMember(parseM)
            ptyp
        | Ast.ValueType.Composite fields ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Composite" |> newName, Some (typeof<Ast.Value>))
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
                            let subName =
                                match def with
                                | Ast.ValueType.Set _
                                | Ast.ValueType.Mapping _
                                | Ast.ValueType.Composite _ -> Some fieldName
                                | _ -> None
                            getProvidedType(subName, def)
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
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Mapping" |> newName, Some (typeof<Ast.Value>))
            let content =
                new ProvidedProperty(
                    "Content",
                    typeof<Map<_,_>>
                        .GetGenericTypeDefinition()
                        .MakeGenericType(typeof<string>, ptyp1))
            let unwrap this =
                <@@
                    match (%%this : obj) :?> Ast.Value with
                    | Ast.Value.Mapping items -> items
                    | _ -> failwith "Underlying type is not Mapping"
                @@>
            content.GetterCode <-
                fun [this] ->
                    let e = unwrap this
                    <@@
                        Map.ofList %%e
                    @@>
            ptyp.AddMember(content)
            ptyp
        | Ast.ValueType.Set itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Set" |> newName, Some (typeof<Ast.Value>))
            let content =
                new ProvidedProperty(
                    "Content",
                    typeof<Set<_>>
                        .GetGenericTypeDefinition()
                        .MakeGenericType(ptyp1))
            let unwrap this =
                <@@
                    match (%%this : obj) :?> Ast.Value with
                    | Ast.Value.Set items -> items
                    | _ -> failwith "Underlying type is not Set"
                @@>
            content.GetterCode <-
                fun [this] ->
                    let e = unwrap this
                    <@@
                        Set.ofList %%e
                    @@>
            ptyp.AddMember(content)
            ptyp

    and getProvidedType (name, typ) : ProvidedTypeDefinition =
        cached cache (buildProvidedType) (name, typ)

    getProvidedType, cache


[<TypeProvider>]
type MissionTypes(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "SturmovikMissionTypes"

    let provider = ProvidedTypeDefinition(asm, ns, "Provider", Some(typeof<obj>))
    let sampleParam = ProvidedStaticParameter("sample", typeof<string>)
    do sampleParam.AddXmlDoc("<brief>Name of a mission file from which the structure of missions is infered</brief>")

    do provider.DefineStaticParameters([sampleParam], fun typeName [| sample |] ->
        let sample = sample :?> string
        if not(System.IO.File.Exists(sample)) then
            failwithf "Cannot open sample file '%s' for reading" sample
        let getProvidedType, cache = mkProvidedTypeBuilder()
        let types, _ = AutoSchema.getTopTypes(Parsing.Stream.FromFile(sample))
        for t in types do
            ignore <| getProvidedType(Some t.Key, t.Value)
        let types =
            cache
            |> Seq.map (fun kvp -> kvp.Value)
            |> List.ofSeq
        let ty = new ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        ty.AddMembers(types)
        //let parserType = new ProvidedTypeDefinition("Parser", Some(typeof<Parsing.ParserFun>))
        //ty.AddMember(parserType)
        ty.AddMember(typeof<Parsing.ParserFun>)
        ty
    )

    do this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do()
