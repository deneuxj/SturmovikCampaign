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


module SturmovikMission.DataProvider.Ast

open System.Collections.Generic
open Cached


type MinMultiplicity = Zero | MinOne
with
    member this.ToExpr() =
        match this with
        | Zero -> <@ Zero @>
        | MinOne -> <@ MinOne @>

type MaxMultiplicity = MaxOne | Multiple
with
    member this.ToExpr() =
        match this with
        | MaxOne -> <@ MaxOne @>
        | Multiple -> <@ Multiple @>
            
let least =
    function
    | Zero, _
    | _, Zero -> Zero
    | MinOne, MinOne -> MinOne

let most =
    function
    | Multiple, _
    | _, Multiple -> Multiple
    | MaxOne, MaxOne -> MaxOne

type ValueType =
    | Boolean
    | Integer
    | String
    | Float
    | Composite of Map<string, ValueType * MinMultiplicity * MaxMultiplicity>
    | Mapping of ValueType // { 1 = XXX1; 2 = XXX2 }
    | List of ValueType // { entries }
    | IntVector // [1, 2, 3]
    | Pair of ValueType * ValueType
    | Triplet of ValueType * ValueType * ValueType
    | Date
    | FloatPair

let private valueTypeToExprCache = new Dictionary<ValueType, Quotations.Expr<ValueType>>(HashIdentity.Structural)

let rec buildExprFromValueType expr =
    match expr with
    | Boolean -> <@ Boolean @>
    | Integer -> <@ Integer @>
    | String -> <@ String @>
    | Float  -> <@ Float @>
    | Date -> <@ Date @>
    | IntVector -> <@ IntVector @>
    | Mapping vt -> <@ Mapping %(getExprOfValueType vt) @>
    | List vt -> <@ List %(getExprOfValueType vt) @>
    | Pair (p1, p2) -> <@ Pair(%(getExprOfValueType p1), %(getExprOfValueType p2)) @>
    | Triplet (p1, p2, p3) -> <@ Triplet(%(getExprOfValueType p1), %(getExprOfValueType p2), %(getExprOfValueType p3)) @>
    | FloatPair -> <@ FloatPair @>
    | Composite defs ->
        let defs =
            defs
            |> Map.toList
            |> List.fold (
                fun e (name, (t, m, M)) ->
                    let item = <@ name, (%(getExprOfValueType t), %(m.ToExpr()), %(M.ToExpr())) @>
                    <@ Map.add (fst %item) (snd %item) %e @>) <@ Map.empty @>                            
        <@ Composite %defs @>

and getExprOfValueType expr =
    cached valueTypeToExprCache buildExprFromValueType expr

type ValueType
    with member this.ToExpr() = getExprOfValueType this

type Value =
    | Boolean of bool
    | Integer of int
    | String of string
    | Float of float
    | FloatPair of float * float
    | Composite of (string * Value) list
    | Mapping of (int * Value) list
    | List of Value list
    | IntVector of int list
    | Pair of Value * Value
    | Triplet of Value * Value * Value
    | Date of int * int * int // Day, month, year
with
    member this.GetBool() =
        match this with
        | Boolean b -> b
        | _ -> invalidOp "Not a Boolean"
    member this.GetInteger() =
        match this with
        | Integer n -> n
        | _ -> invalidOp "Not an Integer"
    member this.GetString() =
        match this with
        | String s -> s
        | _ -> invalidOp "Not a String"
    member this.GetFloat() =
        match this with
        | Float x -> x
        | _ -> invalidOp "Not a Float"
    member this.GetFloatPair() =
        match this with
        | FloatPair(x, y) -> x, y
        | _ -> invalidOp "Not a FloatPair"
    member this.GetIntVector() =
        match this with
        | IntVector xs -> xs
        | _ -> invalidOp "Not an IntVector"
    member this.GetDate() =
        match this with
        | Date (d, m, y) -> (d, m, y)
        | _ -> invalidOp "Not a Date"
    member this.GetItems() =
        match this with
        | Composite items -> items
        | _ -> invalidOp "Not a Composite"
    member this.GetMapping() =
        match this with
        | Mapping items -> items
        | _ -> invalidOp "Not a Mapping"
    member this.GetList() =
        match this with
        | List items -> items
        | _ -> invalidOp "Not a Set"
    member this.GetPair() =
        match this with
        | Pair(x1, x2) -> x1, x2
        | _ -> invalidOp "Not a Pair"
    member this.GetTriplet() =
        match this with
        | Triplet(x1, x2, x3) -> x1, x2, x3
        | _ -> invalidOp "Not a Triplet"
    member this.SetItem(name, value) =
        match this with
        | Composite items ->
            let content =
                items
                |> List.filter(fun (name2, _) -> name2 <> name)
            Composite ((name, value) :: content)
        | _ -> invalidOp "Not a Composite"
    member this.SetItem(name, value : Value option) =
        match this with
        | Composite items ->
            let content =
                items
                |> List.filter(fun (name2, _) -> name2 <> name)
            match value with
            | None ->
                Composite content
            | Some value ->
                Composite ((name, value) :: content)
        | _ -> invalidOp "Not a Composite"
    member this.AddItems(name, values : Value list) =
        match this with
        | Composite items ->
            let values =
                values
                |> List.map (fun x -> (name, x))
            Composite (values @ items)
        | _ -> invalidOp "Not a Composite"
    member this.ClearItems(name) =
        match this with
        | Composite items ->
            let content =
                items
                |> List.filter(fun (name2, _) -> name2 <> name)
            Composite content
        | _ -> invalidOp "Not a Composite"
    member this.SetItem(key : int, value) =
        match this with
        | Mapping items ->
            let content =
                items
                |> List.filter(fun (key2, _) -> key2 <> key)
            Mapping ((key, value) :: content)
        | _ -> invalidOp "Not a Mapping"
    member this.RemoveItem(key : int) =
        match this with
        | Mapping items ->
            let content =
                items
                |> List.filter(fun (key2, _) -> key2 <> key)
            Mapping content
        | IntVector items ->
            items
            |> List.filter((<>) key)
            |> IntVector
        | _ -> invalidOp "Not a Mapping or IntVector"
    member this.AddItem(key : int) =
        match this with
        | IntVector items ->
            let content =
                items
                |> List.filter((<>) key)
            IntVector (key :: content)
        | _ -> invalidOp "Not anIntVector"
    member this.Clear() =
        match this with
        | Mapping _ -> Mapping []
        | List _ -> List []
        | IntVector _ -> IntVector []
        | _ -> invalidOp "Not a Mapping, Set or IntVector"
    
    member this.ToExpr() =
        match this with
        | Boolean x -> <@ Boolean x @>
        | Integer x -> <@ Integer x @>
        | Float x -> <@ Float x @>
        | FloatPair(x, y) -> <@ FloatPair(x, y) @>
        | String x -> <@ String x @>
        | Date(x, y, z) -> <@ Date(x, y, z) @>
        | IntVector x ->
            x
            |> List.rev
            |> List.fold (fun expr n -> <@ n :: %expr @>) <@ [] @>
            |> fun xs -> <@ IntVector %xs @>
        | Mapping x ->
            x
            |> List.fold (fun expr (n, value) -> <@ (n, %(value.ToExpr())) :: %expr @>) <@ [] @>
            |> fun xs -> <@ Mapping %xs @>
        | List x ->
            x
            |> List.fold (fun expr value -> <@ %(value.ToExpr()) :: %expr @>) <@ [] @>
            |> fun xs -> <@ List %xs @>
        | Pair(x1, x2) ->
            <@ Pair(%x1.ToExpr(), %x2.ToExpr()) @>
        | Triplet(x1, x2, x3) ->
            <@ Triplet(%x1.ToExpr(), %x2.ToExpr(), %x3.ToExpr()) @>
        | Composite fields ->
            fields
            |> List.rev
            |> List.fold (fun expr (name, value) -> <@ (name, %value.ToExpr()) :: %expr @>) <@ [] @>
            |> fun xs -> <@ Composite %xs @>
            

let rec dump (value : Value) : string =
    match value with
    | Boolean b -> if b then "1" else "0"
    | Integer i -> sprintf "%d" i
    | String s -> sprintf "\"%s\"" s
    | Float f -> sprintf "%f" f
    | FloatPair(x, y) -> sprintf "%f, %f" x y
    | Composite content ->
        let content =
            content
            |> List.sortBy (function
                | _, Mapping _ -> 2
                | _ -> 1
            )
        seq {
            yield sprintf "{\n"
            for (k, v) in content do
                match v with
                | List _
                | Mapping _
                | Composite _ ->
                    yield sprintf "%s\n%s" k (dump v)
                | _ ->
                    yield sprintf "%s = %s;\n" k (dump v)
            yield "}\n"
        }
        |> String.concat ""
    | Date (day, month, year) ->
        sprintf "%d.%d.%d" day month year
    | Pair (x1, x2) ->
        sprintf "%s : %s" (dump x1) (dump x2)
    | Triplet (x1, x2, x3) ->
        sprintf "%s : %s : %s" (dump x1) (dump x2) (dump x3)
    | IntVector xs ->
        let content = 
            seq {
                for x in xs do
                    yield sprintf "%d" x
            }
            |> String.concat ", "
        sprintf "[%s]" content
    | List xs ->
        seq {
            yield "{\n"
            for x in xs do
                yield dump x
                yield ";\n"
            yield "}\n"
        }
        |> String.concat ""
    | Mapping content ->
        seq {
            yield sprintf "{\n"
            for (k, v) in content do
                yield sprintf "%d = %s;\n" k (dump v)
            yield "}\n"
        }
        |> String.concat ""


type Data =
    | Leaf of string * Value
    | Group of GroupData

and GroupData =
    { Name : string
      Index : int
      Description : string
      Data : Data list
    }

type Data with
    member this.GetLeaves() =
        match this with
        | Leaf(name, value) -> [(name, value)]
        | Group group ->
            group.Data
            |> List.map (fun data -> data.GetLeaves())
            |> List.concat

    /// <summary>
    /// Return the leaves with their path (from the leaf to the root).
    /// </summary>
    /// <param name="path">
    /// Path from the group that contains that node to the root.
    /// One item in the path is composed of the name and the index of the corresponding group node.
    /// </param>
    member this.GetLeavesWithPath(path : (string * int) list) =
        match this with
        | Leaf(name, value) -> [(path, name, value)]
        | Group group ->
            group.Data
            |> List.map (fun data -> data.GetLeavesWithPath((group.Name, group.Index) :: path))
            |> List.concat

    member this.GetLeavesWithPath() = this.GetLeavesWithPath([])

    member this.FindByPath(path : string list) =
        match path with
        | [] -> [this]
        | current :: rest ->
            match this with
            | Leaf _ -> []
            | Group group ->
                if group.Name = current then
                    group.Data
                    |> List.map (fun node -> node.FindByPath(rest))
                    |> List.concat
                else
                    []

    member this.ToExpr() =
        match this with
        | Leaf(name, value) ->
            let e = value.ToExpr()
            <@ Leaf(name, %e) @>
        | Group(data) ->
            let subs =
                data.Data
                |> List.rev
                |> List.fold(fun expr data -> <@ %(data.ToExpr()) :: %expr @>) <@ [] @>
            let name = data.Name
            let index = data.Index
            let desc = data.Description
            <@ Group
                { Name = name
                  Index = index
                  Description = desc
                  Data = %subs
                } @>

