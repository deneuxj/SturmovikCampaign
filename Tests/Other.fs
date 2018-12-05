/// Tests for the misc functions
module Campaign.Tests.Other

open NUnit.Framework
open Campaign.BasicTypes
open System.Numerics
open FSharp.Control
open Campaign.WatchLogs
open Campaign.Airfield
open Util
open Campaign.Commenting
open ploggy

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

[<Test>]
let ``Campaign.Commenting.InjectOldAndFresh works as expected``() =
    let oldEntries =
        asyncSeq {
            yield LogEntry.Parse("T:0 AType:15 VER:17")
            yield LogEntry.Parse("T:0 AType:20 USERID:b8bafd78-159f-4d68-8219-b8b9c1fcccf8 USERNICKID:bfa2dc72-2066-4ba5-aff2-ee9467edaeba")
            yield LogEntry.Parse("T:0 AType:0 GDate:1941.12.26 GTime:10:0:0 MFile:Multiplayer/dogfight\AutoGenMission2_1.msnbin MID: GType:2 CNTRS:0:0,101:1,201:2 SETTS:010000000010000100000000110 MODS:0 PRESET:0 AQMID:0 ROUNDS: 1 POINTS: 500")
            yield LogEntry.Parse("T:1 AType:11 GID:905216 IDS:260096,266240,272384,278528,284672,290816,300032 LID:260096")
        }
        |> AsyncSeq.map Old
    let newEntries =
        asyncSeq {
            do! Async.Sleep(530)
            yield LogEntry.Parse("T:53 AType:12 ID:114688 TYPE:fake_block[21282,1] COUNTRY:201 NAME:CNV-R-2-0-D-0 PID:-1 POS(0.0000,74.1304,0.0000)")
            yield LogEntry.Parse("T:53 AType:3 AID:-1 TID:114688 POS(0.0000,74.1304,0.0000)")
            do! Async.Sleep(530)
            yield LogEntry.Parse("T:106 AType:12 ID:114688 TYPE:fake_block[21282,1] COUNTRY:201 NAME:CNV-R-2-1-D-0 PID:-1 POS(0.0000,74.1304,0.0000)")
            yield LogEntry.Parse("T:106 AType:3 AID:-1 TID:114688 POS(0.0000,74.1304,0.0000)")
        }
        |> AsyncSeq.map Fresh
    let entries = AsyncSeq.append oldEntries newEntries
    let oldInjected =
        seq {
            yield ArtificialEntry(24L, "1") :> LogEntry
        }
    let newInjected =
        asyncSeq {
            do! Async.Sleep(600)
            yield "2"
        }
    let takeNewInjected =
        let it = newInjected.GetEnumerator()
        it.MoveNext
    let result =
        injectOldAndFresh entries oldInjected takeNewInjected ignore
        |> AsyncSeq.truncate 10
        |> AsyncSeq.iter (fun data ->
            printfn "%A" data)
        |> Async.RunSynchronously
    ()
    //Assert.AreEqual(10, result.Length)