module Tests

open System
open Xunit
open FsCheck
open FsCheck.Xunit
open System.Numerics

module ClusterTests =
    open Campaign.Cluster

    let genFloat32 =
        Gen.choose(-100000000, 100000000)
        |> Gen.map float32
        |> Gen.map (fun x -> x / 100.0f)

    let genIntItem =
        Gen.zip3 (Gen.choose(0, 1 <<< 31)) genFloat32 genFloat32

    let genIntItems =
        Gen.listOf genIntItem

    let genCluster =
        Gen.listOf genIntItem
        |> Gen.map (ClusterPartition.create (fun (_, x, y) -> Vector2(x, y)))

    let arbIntItems = Arb.fromGen genIntItems
    let arbCluster = Arb.fromGen genCluster

    [<Property>]
    let ``Content of created clusters is identical to list of items`` () =
        Prop.forAll arbIntItems (fun items -> 
            let cluster = items |> ClusterPartition.create (fun (_, x, y) -> Vector2(x, y))
            let fromCluster =
                cluster.Clusters
                |> List.collect (fun c -> c.Items)
                |> List.map (fun (x, _) -> x)
                |> Set
            fromCluster = Set items
        )

    [<Property>]
    let ``Content of refined clusters is identical to unrefined clusters`` () =
        let arb =
            Arb.fromGen (
                Gen.zip
                    (Gen.choose(1, 100) |> Gen.map float32 |> Gen.map (fun x -> x / 100.0f))
                    genCluster
            )
        Prop.forAll arb (fun (k, unrefined) ->
            let refined = ClusterPartition.refine k 5000.0f 10 unrefined
            let fromUnrefined =
                unrefined.Clusters
                |> List.collect (fun c -> c.Items)
                |> List.map fst
                |> Set
            let fromRefined =
                refined.Clusters
                |> List.collect (fun c -> c.Items)
                |> List.map fst
                |> Set
            fromRefined = fromUnrefined
        )