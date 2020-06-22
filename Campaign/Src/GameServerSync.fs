// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2020 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

/// Control game server start and stop
namespace Campaign.GameServerSync

open System
open MBrace.FsPickler

type Settings =
    {
        Address : string
        Port : int
        Password : string
        SdsFile : string
        WorkDir : string
        GameDir : string
        MissionBaseName : string
        MissionDuration : int
    }

type SyncState =
    | PreparingMission
    | RunningMission of EndTime: DateTime
with
    static member FileName = "sync.xml"

    member this.Save(workDir : string) =
        let serializer = FsPickler.CreateXmlSerializer()
        let tmpFile = IO.Path.Combine(workDir, SyncState.FileName + ".tmp")
        use file = IO.File.CreateText(tmpFile)
        serializer.Serialize(file, this)
        IO.File.Copy(tmpFile, IO.Path.Combine(workDir, "sync.xml"), true)
        IO.File.Delete(tmpFile)

    static member TryLoad(workDir) =
        let serializer = FsPickler.CreateXmlSerializer()
        try
            use file = IO.File.OpenText(IO.Path.Combine(workDir, SyncState.FileName))
            serializer.Deserialize<SyncState>(file)
            |> Some
        with _ -> None


type IGameServerControl =
    abstract IsRunning : obj option -> bool
    abstract StartProcessWithSds : string -> Async<Result<obj, string>>
    abstract KillProcess : obj option -> Result<unit, string>
    abstract AttachRcon : address: string * port: int * pwd: string -> Async<Result<unit, string>>
    abstract IsAttached : bool
    abstract SignalMissionEnd : unit -> Async<Result<unit, string>>
    abstract RotateMission : unit -> Async<Result<unit, string>>
    abstract LoadSds : string -> Async<Result<unit, string>>
    abstract ResaveMission : string -> Async<Result<unit, string>>
    abstract CopyRenameMission : string * string -> Async<Result<unit, string>>


type Sync(settings : Settings, gameServer : IGameServerControl) =
    let mutable serverProcess = None
    let mutable state = SyncState.TryLoad(settings.WorkDir) |> Option.defaultValue PreparingMission

    let missionCompletedEvent = Event<string>()

    member this.MissionCompleted = missionCompletedEvent.Publish

    member this.MissionPath = IO.Path.Combine(settings.WorkDir, "Multiplayer", "Dogfight", sprintf "%s_1.Mission" settings.MissionBaseName)

    member this.Die(msg) =
        async {
            printfn "Game server sync terminated: %s" msg
            return()
        }

    member this.StartServer() =
        async {
            let! s = gameServer.StartProcessWithSds(settings.SdsFile)
            match s with
            | Ok proc ->
                serverProcess <- Some proc
                let! _ = gameServer.AttachRcon(settings.Address, settings.Port, settings.Password)
                return! this.Resume()
            | Error msg ->
                serverProcess <- None
                return! this.Die(msg)
        }

    member this.Resume() =
        async {
            match state with
            | PreparingMission ->
                let rec waitMissionReady() =
                    async {
                        do! Async.Sleep(15000)
                        if IO.File.Exists(IO.Path.Combine(settings.WorkDir)) then
                            do! Async.Sleep(5000)
                        else
                            return! waitMissionReady()
                    }
                do! waitMissionReady()
                if gameServer.IsRunning serverProcess then
                    let! s = gameServer.RotateMission()
                    match s with
                    | Ok() ->
                        state <- RunningMission (DateTime.UtcNow + TimeSpan.FromMinutes(float settings.MissionDuration))
                        return! this.Resume()
                    | Error msg ->
                        return! this.Die(msg)
                else
                    return! this.StartServer()

            | RunningMission deadline ->
                if not(gameServer.IsRunning serverProcess) then
                    return! this.StartServer()
                else
                let untilDeadLine = (deadline - DateTime.UtcNow).TotalMilliseconds
                if untilDeadLine > 0.0 then
                    do! Async.Sleep(int untilDeadLine)
                let! _ = gameServer.SignalMissionEnd()
                missionCompletedEvent.Trigger(settings.MissionBaseName)
                state <- PreparingMission
                return! this.Resume()
        }