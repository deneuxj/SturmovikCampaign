module SturmovikMission.DataProvider.TypeProvider

#nowarn "25" // Incomplete pattern matches, occurs a lot due to "fun [this] -> <@@ ... @@>"

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

let addConstructor (ptyp : ProvidedTypeDefinition) (args : (string * Type) list) (body : Expr list -> Expr) =
    let args =
        args
        |> List.map (fun (n, t) -> ProvidedParameter(n, t))
    let cnstr =
        ProvidedConstructor(args)
    cnstr.InvokeCode <- body
    ptyp.AddMember(cnstr)

let addProperty (ptyp : ProvidedTypeDefinition) (name : string, typ : Type) (body : Expr -> Expr) =
    let prop = ProvidedProperty(name, typ)
    prop.GetterCode <- fun [this] -> body this
    ptyp.AddMember(prop)

let addMethod (ptyp : ProvidedTypeDefinition) (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
    let args =
        args
        |> List.map (fun (n, t) -> ProvidedParameter(n, t))
    let m = ProvidedMethod(name, args, typ)
    m.InvokeCode <- body
    ptyp.AddMember(m)

let addStaticMethod (ptyp : ProvidedTypeDefinition) (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
    let args =
        args
        |> List.map (fun (n, t) -> ProvidedParameter(n, t))
    let m = ProvidedMethod(name, args, typ)
    m.IsStaticMethod <- true
    m.InvokeCode <- body
    ptyp.AddMember(m)

    
let mkProvidedTypeBuilder(top : ProvidedTypeDefinition) =
    let newName =
        fun baseName ->
            Seq.initInfinite (fun i -> if i = 0 then baseName else sprintf "%s_%d" baseName (i + 1))
        |> getNameStore

    let cache = new Dictionary<string option * Ast.ValueType, ProvidedTypeDefinition>(HashIdentity.Structural)

    let rec buildProvidedType (name : string option, typ : Ast.ValueType) =
        let typExpr = typ.ToExpr()
        match typ with
        | Ast.ValueType.Boolean ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Boolean", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<bool>) (fun this -> <@@ (%%this : Ast.Value).GetBool() @@>)
            ptyp
        | Ast.ValueType.Float ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Float", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<float>) (fun this -> <@@ (%%this : Ast.Value).GetFloat() @@>)
            ptyp
        | Ast.ValueType.Integer ->
            let ptyp =
                new ProvidedTypeDefinition("Integer", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<int>) (fun this -> <@@ (%%this : Ast.Value).GetInteger() @@>)
            ptyp
        | Ast.ValueType.String ->
            let ptyp =
                new ProvidedTypeDefinition("String", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<string>) (fun this -> <@@ (%%this : Ast.Value).GetString() @@>)
            ptyp
        | Ast.ValueType.IntVector ->
            let ptyp =
                new ProvidedTypeDefinition("VectorOfIntegers", Some (typeof<Ast.Value>))
            addProperty ptyp ("Value", typeof<int list>) (fun this -> <@@ (%%this : Ast.Value).GetIntVector() @@>)
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
            addProperty ptyp ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetPair() @@>)
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
            addProperty ptyp ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetTriplet() @@>)
            ptyp
        | Ast.ValueType.Date ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Date", Some (typeof<Ast.Value>))
            addProperty ptyp ("Year", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, _, year = (%%e : int * int * int) in year @@>)
            addProperty ptyp ("Month", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, month, _ = (%%e : int * int * int) in month @@>)
            addProperty ptyp ("Day", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let day, _, _ = (%%e : int * int * int) in day @@>)
            ptyp
        | Ast.ValueType.Composite fields ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Composite" |> newName, Some (typeof<Ast.Value>))
            let asList this = <@ (%%this : Ast.Value).GetItems() @>
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
            ptyp.AddMembers(members)
            ptyp
        | Ast.ValueType.Mapping itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Mapping" |> newName, Some (typeof<Ast.Value>))
            let propTyp = typedefof<Map<_,_>>.MakeGenericType(typeof<int>, ptyp1)
            addProperty ptyp ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetMapping() |> Map.ofList @@>)
            ptyp
        | Ast.ValueType.Set itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Set" |> newName, Some (typeof<Ast.Value>))
            let propTyp = typedefof<Set<_>>.MakeGenericType(ptyp1)
            addProperty ptyp ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetSet() |> Set.ofList @@>)
            ptyp

    and getProvidedType (name, typ) : ProvidedTypeDefinition =
        cached cache (buildProvidedType) (name, typ)

    getProvidedType, cache


let buildParserType(namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
    let parser = ProvidedTypeDefinition("Parser", Some typeof<IDictionary<Ast.ValueType, Parsing.ParserFun>>)
    let valueTypeExprs =
        namedValueTypes
        |> List.map (fun (_, x, _) -> x)
        |> List.fold (fun expr valueType ->
            let valueTypeExpr = valueType.ToExpr()
            <@ %valueTypeExpr :: %expr @>
            ) <@ [] @>
    addConstructor parser [] (fun [] ->
        <@@
            %valueTypeExprs
            |> List.map(fun valueType -> (valueType, Parsing.makeParser valueType))
            |> dict
        @@>)
    for name, valueType, ptyp in namedValueTypes do
        let vtExpr = valueType.ToExpr()
        let retType =
            typedefof<_*_>.MakeGenericType(ptyp, typeof<Parsing.Stream>)
        addMethod parser (sprintf "Parse_%s" name, retType) [("s", typeof<Parsing.Stream>)] (fun [this; s] ->
            <@@
                let parsers = (%%this : IDictionary<Ast.ValueType, Parsing.ParserFun>)
                let parser = parsers.[%vtExpr]
                parser.Run(%%s : Parsing.Stream)
            @@>
        )
    parser


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
        let ty = new ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        // The types corresponding to the ValueTypes extracted from the sample file
        let getProvidedType, cache = mkProvidedTypeBuilder(ty)
        let types, _ = AutoSchema.getTopTypes(Parsing.Stream.FromFile(sample))
        for t in types do
            ignore <| getProvidedType(Some t.Key, t.Value)
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
        let parserType = buildParserType namedTypes
        ty.AddMember(parserType)
        ty
    )

    do this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do()
