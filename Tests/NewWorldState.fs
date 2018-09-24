/// Tests for the functions that compute a new world state from an old world state and mission results
module Campaign.Tests.NewWorldState

open NUnit.Framework
open Campaign.BasicTypes
open System.Numerics
open Campaign.NewWorldState

[<Test>]
let ``healing does not exceed limit``() =
    let specs = [{ Pattern = "test_building"; SubBlocks = [| 0; 1; 2 |]; Production = 25.0f<E/H>; Storage = 0.0f<E>; IsAirfield = false; Durability = 25000 }]
    let groups = [{ Model = "test_building"; Script = ""; Pos = { Pos = Vector2.Zero; Rotation = 0.0f; Altitude = 0.0f } }]
    let healLimit = 30.0f<E>
    let healths0 = [| 0.0f; 1.0f; 0.5f |]
    let healths, energy = computeHealing(specs, [healths0], groups, 100.0f<E>, healLimit)
    Assert.That(healths.Length = 1, "List of arrays of healths is a singleton")
    let healths = healths.[0]
    let repairCosts =
        Array.zip healths0 healths
        |> Array.sumBy (fun (h0, h) -> (h - h0) * specs.[0].RepairCost / 3.0f)
    Assert.That(100.0f<E> - energy = repairCosts, "Energy consumption matches repair costs")
    Assert.That(healths.Length = 3, "Array of healths has correct length")
    Assert.That(healths.[0] > 0.0f, "First building got repairs")
    Assert.That(healths.[0] < 1.0f, "First building not entirely repaired")
    Assert.That(healths.[1] = 1.0f, "Second building still undamaged")
    Assert.That(healths.[2] = 0.5f, "Last building got no repairs")
    Assert.LessOrEqual(100.0f<E> - energy, healLimit, "Energy spent on repairs does not excess heal limit")

