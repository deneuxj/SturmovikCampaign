/// Tests for the misc functions
module Campaign.Tests.Other

open NUnit.Framework
open Campaign.BasicTypes
open System.Numerics
open Campaign.Airfield
open Util

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

[<Test>]
let ``Seq.mergeOrdered works as expected``() =
    let res = Seq.mergeOrdered id id [1; 3; 0; 5; 0; 7] [2; 0; 4; 6; 8; 10; 0]
    let A = Choice1Of2
    let B = Choice2Of2
    let expected =
        [ A 1; B 2; B 0; A 3; A 0; B 4; A 5; A 0; B 6; A 7; B 8; B 10; B 0 ]
    Assert.AreEqual(expected, res)