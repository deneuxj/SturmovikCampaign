module SturmovikMission.DataProvider.TypeProvider

#nowarn "25" // Incomplete pattern matches, occurs a lot due to "fun [this] -> <@@ ... @@>"

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Quotations
open System.Collections.Generic
open System
open System.IO

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

let newConstructor (args : (string * Type) list) (body : Expr list -> Expr) =
    let args =
        args
        |> List.map (fun (n, t) -> ProvidedParameter(n, t))
    let cnstr =
        ProvidedConstructor(args)
    cnstr.InvokeCode <- body
    cnstr

let newProperty (name : string, typ : Type) (body : Expr -> Expr) =
    let prop = ProvidedProperty(name, typ)
    prop.GetterCode <- fun [this] -> body this
    prop

let newStaticProperty (name : string, typ : Type) (body : Expr) =
    let prop = ProvidedProperty(name, typ)
    prop.IsStatic <- true
    prop.GetterCode <- fun [] -> body
    prop

let newMethod (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
    let args =
        args
        |> List.map (fun (n, t) -> ProvidedParameter(n, t))
    let m = ProvidedMethod(name, args, typ)
    m.InvokeCode <- body
    m

let newStaticMethod (name : string, typ : Type) (args : (string * Type) list) (body : Expr list -> Expr) =
    let m = newMethod (name, typ) args body
    m.IsStaticMethod <- true
    m

let getNameOfField(fieldName : string, def) =
    match def with
    | Ast.ValueType.Set _
    | Ast.ValueType.Mapping _
    | Ast.ValueType.Composite _ -> Some fieldName
    | _ -> None
    
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
            ptyp.AddMember(newProperty ("Value", typeof<bool>) (fun this -> <@@ (%%this : Ast.Value).GetBool() @@>))
            ptyp.AddMember(newConstructor [("Value", typeof<bool>)] (fun [value] -> <@@ Ast.Value.Boolean (%%value : bool) @@>))
            ptyp
        | Ast.ValueType.Float ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Float", Some (typeof<Ast.Value>))
            ptyp.AddMember(newProperty ("Value", typeof<float>) (fun this -> <@@ (%%this : Ast.Value).GetFloat() @@>))
            ptyp.AddMember(newConstructor [("Value", typeof<float>)] (fun [value] -> <@@ Ast.Value.Float (%%value : float) @@>))
            ptyp
        | Ast.ValueType.Integer ->
            let ptyp =
                new ProvidedTypeDefinition("Integer", Some (typeof<Ast.Value>))
            ptyp.AddMember(newProperty ("Value", typeof<int>) (fun this -> <@@ (%%this : Ast.Value).GetInteger() @@>))
            ptyp.AddMember(newConstructor [("Value", typeof<int>)] (fun [value] -> <@@ Ast.Value.Integer (%%value : int) @@>))
            ptyp
        | Ast.ValueType.String ->
            let ptyp =
                new ProvidedTypeDefinition("String", Some (typeof<Ast.Value>))
            ptyp.AddMember(newProperty ("Value", typeof<string>) (fun this -> <@@ (%%this : Ast.Value).GetString() @@>))
            ptyp.AddMember(newConstructor [("Value", typeof<string>)] (fun [value] -> <@@ Ast.Value.String (%%value : string) @@>))
            ptyp
        | Ast.ValueType.IntVector ->
            let ptyp =
                new ProvidedTypeDefinition("VectorOfIntegers", Some (typeof<Ast.Value>))
            ptyp.AddMember(newProperty ("Value", typeof<int list>) (fun this -> <@@ (%%this : Ast.Value).GetIntVector() @@>))
            ptyp.AddMember(newConstructor [("Value", typeof<int list>)] (fun [value] -> <@@ Ast.Value.IntVector (%%value : int list) @@>))
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
            ptyp.AddMember(newProperty ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetPair() @@>))
            ptyp.AddMember(newConstructor [("Value", propTyp)] (fun [value] -> <@@ Ast.Value.Pair (%%value : Ast.Value * Ast.Value) @@>))
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
            ptyp.AddMember(newProperty ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetTriplet() @@>))
            ptyp.AddMember(newConstructor [("Value", propTyp)] (fun [value] -> <@@ Ast.Value.Triplet (%%value : Ast.Value * Ast.Value * Ast.Value) @@>))
            ptyp
        | Ast.ValueType.Date ->
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Date", Some (typeof<Ast.Value>))
            ptyp.AddMember(newProperty ("Year", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, _, year = (%%e : int * int * int) in year @@>))
            ptyp.AddMember(newProperty ("Month", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let _, month, _ = (%%e : int * int * int) in month @@>))
            ptyp.AddMember(newProperty ("Day", typeof<int>) (fun this ->
                let e = <@@ (%%this : Ast.Value).GetDate() @@>
                <@@ let day, _, _ = (%%e : int * int * int) in day @@>))
            ptyp.AddMember(newConstructor [("Day", typeof<int>); ("Month", typeof<int>); ("Year", typeof<int>)] (fun [day; month; year] ->
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
                        newMethod
                            ((sprintf "Set%s" fieldName), ptyp)
                            [("value", upcast fieldType)]
                            (fun [this; value] ->
                                <@@
                                    let this = (%%this : Ast.Value)
                                    this.SetItem(fieldName, (%%value : Ast.Value))
                                @@>)
                    | Ast.MinMultiplicity.Zero, Ast.MaxOne ->
                        let optTyp =
                            typedefof<option<_>>
                                .MakeGenericType(fieldType)
                        newMethod
                            ((sprintf "Set%s" fieldName), ptyp)
                            [("value", optTyp)]
                            (fun [this; value] ->
                                <@@
                                    let this = (%%this : Ast.Value)
                                    this.SetItem(fieldName, (%%value : Ast.Value option))
                                @@>)
                    | _, Ast.MaxMultiplicity.Multiple ->
                        let listTyp =
                            typedefof<List<_>>
                                .MakeGenericType(fieldType)
                        newMethod
                            ((sprintf "Set%s" fieldName), ptyp)
                            [("value", listTyp)]
                            (fun [this; value] ->
                                <@@
                                    let this = (%%this : Ast.Value)
                                    this.ClearItems(fieldName).AddItems(fieldName, (%%value : Ast.Value list))
                                @@>))
                |> List.ofSeq
            ptyp.AddMembersDelayed(setters)
            // Create Mcu instances
            let asMcu() =
                [
                    match Mcu.tryMkAsCommand typ with
                    | Some _ ->
                        yield newMethod
                            ("AsCommand", typeof<Mcu.McuCommand>)
                            []
                            (fun [this] ->
                                <@@
                                    match Mcu.tryMkAsCommand %typExpr with
                                    | Some f -> f (%%this : Ast.Value)
                                    | None -> failwith "Unexpected error: could not build AsCommand"
                                @@>)
                    | None -> ()
                    match Mcu.tryMkAsEntity typ with
                    | Some _ ->
                        yield newMethod
                            ("AsEntity", typeof<Mcu.McuEntity>)
                            []
                            (fun [this] ->
                                <@@
                                    match Mcu.tryMkAsEntity %typExpr with
                                    | Some f -> f (%%this : Ast.Value)
                                    | None -> failwith "Unexpected error: could not build AsEntity"
                                @@>)
                    | None -> ()
                    match Mcu.tryMkAsHasEntity typ with
                    | Some _ ->
                        yield newMethod
                            ("AsHasEntity", typeof<Mcu.HasEntity>)
                            []
                            (fun [this] ->
                                <@@
                                    match Mcu.tryMkAsHasEntity %typExpr with
                                    | Some f -> f (%%this : Ast.Value)
                                    | None -> failwith "Unexpected error: could not build AsHasEntity"
                                @@>)
                    | None -> ()
                ]
            ptyp.AddMembersDelayed(asMcu)
            ptyp
        | Ast.ValueType.Mapping itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Mapping" |> newName, Some (typeof<Ast.Value>))
            ptyp.AddMember(newConstructor [] (fun [] -> <@@ Ast.Value.Mapping [] @@>))
            let propTyp = typedefof<Map<_,_>>.MakeGenericType(typeof<int>, ptyp1)
            ptyp.AddMember(newProperty ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetMapping() |> Map.ofList @@>))
            ptyp.AddMember(newMethod ("SetItem", ptyp) [("Key", typeof<int>); ("Value", upcast ptyp1)] (fun [this; key; value] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.SetItem((%%key : int), (%%value : Ast.Value))
                @@>))
            ptyp.AddMember(newMethod ("RemoveItem", ptyp) (["Key", typeof<int>]) (fun [this; key] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.RemoveItem(%%key : int)
                @@>))
            ptyp.AddMember(newMethod ("Clear", ptyp) [] (fun [this] ->
                <@@
                    let this = (%%this : Ast.Value)
                    this.Clear()
                @@>))
            ptyp
        | Ast.ValueType.Set itemTyp ->
            let ptyp1 = getProvidedType(None, itemTyp)
            let ptyp =
                new ProvidedTypeDefinition(defaultArg name "Set" |> newName, Some (typeof<Ast.Value>))
            ptyp.AddMember(newConstructor [] (fun [] -> <@@ Ast.Value.Set [] @@>))
            let propTyp = typedefof<Set<_>>.MakeGenericType(ptyp1)
            ptyp.AddMember(newProperty ("Value", propTyp) (fun this -> <@@ (%%this : Ast.Value).GetSet() |> Set.ofList @@>))
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
    // Default constructor: Populate the cache of parsers.
    parser.AddMember(newConstructor [] (fun [] ->
        <@@
            %valueTypeExprs
            |> List.map(fun valueType -> (valueType, Parsing.makeParser valueType))
            |> dict
        @@>))
    // Parse methods for all top types.
    for name, valueType, ptyp in namedValueTypes do
        let vtExpr = valueType.ToExpr()
        let retType =
            typedefof<_*_>.MakeGenericType(ptyp, typeof<Parsing.Stream>)
        parser.AddMember(newMethod (sprintf "Parse_%s" name, retType) [("s", typeof<Parsing.Stream>)] (fun [this; s] ->
            <@@
                let parsers = (%%this : IDictionary<Ast.ValueType, Parsing.ParserFun>)
                let parser = parsers.[%vtExpr]
                parser.Run(%%s : Parsing.Stream)
            @@>
        ))
    parser


let buildGroupParserType(namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) =
    let parser = ProvidedTypeDefinition("GroupData", Some typeof<Ast.Data list>)
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
    parser.AddMember(newConstructor [("s", typeof<Parsing.Stream>)] (fun [s] ->
        <@@
            let parsers =
                %valueTypeOfName
                |> Map.map (fun name valueType -> Parsing.makeParser valueType)
            let getParser name = parsers.[name]
            let s = (%%s : Parsing.Stream)
            Parsing.parseFile getParser s
        @@>))
    // Getters: list of objects of each type
    for (name, valueType, ptyp) in namedValueTypes do
        parser.AddMember(newProperty ((sprintf "ListOf%s" name), typedefof<_ list>.MakeGenericType(ptyp)) (fun this ->
            <@@
                let this = (%%this : Ast.Data list)
                let ret =
                    this
                    |> List.map (fun data -> data.GetLeaves())
                    |> List.concat
                    |> List.choose (function (name2, value) -> if name2 = name then Some value else None)
                ret
            @@>))
    // Get the flattened list of objects as instances of McuBase and its subtypes, when appropriate
    parser.AddMember(newProperty ("AsMcuList", typeof<Mcu.McuBase list>) (fun this ->
        <@@
            let this = (%%this : Ast.Data list)
            let valueTypeOfName = %valueTypeOfName
            let mcuMakerOfName =
                %names
                |> List.map (fun name ->
                    let valueType = valueTypeOfName.[name]
                    (name, Mcu.tryMakeMcu valueType)
                )
                |> Map.ofList
            this
            |> List.map (fun data -> data.GetLeaves())
            |> List.concat
            |> List.choose (fun (name, value) ->
                match Map.tryFind name mcuMakerOfName with
                | Some(Some(make)) ->
                    make value
                    |> Some
                | _ -> // Cannot build an Mcu from that valueType
                    None
            )
        @@>
    ))
    // Return result
    parser


let buildLibraries(namedValueTypes : (string * Ast.ValueType * ProvidedTypeDefinition) list) (files : string) =
    let parsers =
        namedValueTypes
        |> List.map (fun (name, typ, _) -> (name, Parsing.makeParser typ))
        |> Map.ofList
    let getParser name = parsers.[name]
    let types =
        namedValueTypes
        |> List.map (fun (name, _, ptyp) -> (name, ptyp))
        |> Map.ofList
    let newName =
        let rand = new Random(0)
        fun baseName ->
            Seq.initInfinite (fun i ->
                if i = 0 then
                    baseName
                elif i < 10 then
                    sprintf "%s_%d" baseName (i + 1)
                else
                    sprintf "%s_R%d" baseName (rand.Next())
                )
        |> getNameStore
    let importFile filename =
        let name = Path.GetFileNameWithoutExtension(filename)
        let lib = new ProvidedTypeDefinition(name, Some typeof<obj>)
        fun () ->
            let staticMembers =
                try
                    let s = Parsing.Stream.FromFile filename
                    let data = Parsing.parseFile getParser s
                    let rec work (data : Ast.Data) : ((string * Type) * Quotations.Expr) seq =
                        seq {
                            match data with
                            | Ast.Data.Leaf(typename, value) ->
                                match value with
                                | Ast.Value.Composite fields ->
                                    let name =
                                        fields
                                        |> Seq.tryPick (function ("Name", Ast.Value.String n) -> Some n | _ -> None)
                                        |> fun x -> defaultArg x "Unnamed"
                                        |> newName
                                    match Map.tryFind typename types with
                                    | Some ptyp ->
                                        let valueExpr = value.ToExpr()
                                        yield (name, upcast ptyp), <@@ %valueExpr @@>
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
                with
                | :? Parsing.ParseError as e ->
                    let msg =
                        Parsing.printParseError e
                        |> String.concat "\n"
                    [ (("LoadingError", typeof<string>), <@@ msg @@>) ]
                | e ->
                    let msg = e.Message
                    [ (("LoadingError", typeof<string>), <@@ msg @@>) ]
            staticMembers
            |> List.map (fun (prop, expr) -> newStaticProperty prop expr)
        |> lib.AddMembersDelayed
        lib

    files.Split(';')
    |> Array.map importFile
    |> List.ofArray


[<TypeProvider>]
type MissionTypes(config: TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces()

    let asm = System.Reflection.Assembly.GetExecutingAssembly()
    let ns = "SturmovikMissionTypes"

    let provider = ProvidedTypeDefinition(asm, ns, "Provider", Some(typeof<obj>))
    let sampleParam = ProvidedStaticParameter("sample", typeof<string>)
    do sampleParam.AddXmlDoc("<summary>Name of a mission file from which the structure of missions is infered</summary>")
    let libraryParam = ProvidedStaticParameter("library", typeof<string>)
    do libraryParam.AddXmlDoc("<summary>List of mission or group files from which to read data, separated by semi-colons</summary>")
    
    let buildProvider(typeName : string, sample : string, libs : string) =
        let ty = new ProvidedTypeDefinition(asm, ns, typeName, Some(typeof<obj>))
        // The types corresponding to the ValueTypes extracted from the sample file
        let getProvidedType, cache = mkProvidedTypeBuilder(ty)
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
        let parserType = buildParserType namedTypes
        ty.AddMember(parserType)
        // The type of the file parser.
        let parserType = buildGroupParserType namedTypes
        ty.AddMember(parserType)
        // The libraries
        let plibs = buildLibraries namedTypes libs
        ty.AddMembers(plibs)
        // Result
        ty

    let cache = new Dictionary<(string * string * string), ProvidedTypeDefinition>(HashIdentity.Structural)
    let getProvider = cached cache buildProvider
    do provider.DefineStaticParameters([sampleParam; libraryParam], fun typeName [| sample; libs |] ->
        let sample = sample :?> string
        let libs = libs :?> string
        let sample =
            if Path.IsPathRooted(sample) then
                sample
            else
                let dllLocation =
                    System.Reflection.Assembly.GetExecutingAssembly().Location
                    |> Path.GetDirectoryName
                Path.Combine(dllLocation, sample)
        if not(System.IO.File.Exists(sample)) then
            failwithf "Cannot open sample file '%s' for reading" sample
        getProvider(typeName, sample, libs)
    )

    do this.AddNamespace(ns, [provider])

[<assembly:TypeProviderAssembly>]
do()
