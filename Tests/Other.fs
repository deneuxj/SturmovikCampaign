/// Tests for the functions that compute a new world state from an old world state and mission results
module Campaign.Tests.Other

open NUnit.Framework
open Campaign.BasicTypes
open System.Numerics
open Campaign.Airfield

[<Test>]
let ``loadout constraints: empty loadouts gives full constraint``() =
    let s = mkLoadoutString 1000.0f<E> []
    Assert.AreEqual("0..999", s)

[<Test>]
let ``loadout constraints: two singleton loadouts``() =
    let s = mkLoadoutString 10.0f<E> [(1, 100.0f<E>); (3, 100.0f<E>)]
    Assert.AreEqual("0/2/4..999", s)

[<Test>]
let ``loadout constraints: with a range``() =
    let s = mkLoadoutString 10.0f<E> [(1, 100.0f<E>); (2, 1.0f<E>); (3, 1.0f<E>); (4, 100.0f<E>)]
    Assert.AreEqual("0/2..3/5..999", s)