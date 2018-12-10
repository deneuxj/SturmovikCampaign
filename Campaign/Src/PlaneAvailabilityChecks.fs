// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
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

module Campaign.PlaneAvailabilityChecks

open System
open NLog
open ploggy
open FSharp.Control
open Campaign.BasicTypes
open Campaign.WorldDescription
open Campaign.PlayerHangar
open Campaign.WorldState
open Campaign.WatchLogs
open Campaign.PlaneChecksContext
open Campaign.PlaneChecksStatus

let private logger = LogManager.GetCurrentClassLogger()


/// Monitor events and check that players don't take off in planes they are not allowed to fly according to player hangars.
/// Also send information about player hangars.
let checkPlaneAvailability (missionLength : float32<H>) (limits : Limits) (world : World) (state : WorldState) (hangars : Map<string * CoalitionId, PlayerHangar>) (entries : AsyncSeq<LogData<LogEntry>>) =

    asyncSeq {
        let mutable context = Context.Create(missionLength, world, state, hangars, limits)
        let mutable players : Map<int, PlayerFlightData> = Map.empty
        let mutable muted = true

        yield Status(context.Hangars , context.Airfields)

        for entry in entries do
            logger.Debug(sprintf "checkPlaneAvailability sees entry %A" entry)
            let mutable cmds0 = []

            // Signal to unmute when we encounter the first fresh log data
            match muted, entry with
            | true, Fresh _ ->
                muted <- false
                yield Unmute
            | _ -> ()

            // Handle special entries
            match entry.Data with
            | :? JoinEntry as joined ->
                for coalition in [Axis; Allies] do
                    match context.Hangars.TryFind((string joined.UserId, coalition)) with
                    | Some hangar ->
                        yield! AsyncSeq.ofSeq(showHangar(hangar, 15))
                    | None ->
                        ()
            | :? ObjectSpawnedEntry as spawned ->
                context <- context.HandleBinding(spawned)
            | :? DamageEntry as damage ->
                context <- context.ResolveBindings(damage)
            | :? PlayerPlaneEntry as plane ->
                match PlayerFlightData.TryCreate(context, plane) with
                | Some data, cmds ->
                    players <- players.Add(data.Vehicle, data)
                    cmds0 <- cmds
                | None, cmds ->
                    cmds0 <- cmds
            | :? ArtificialEntry as artificial ->
                match PlaneGift.TryFromString artificial.Data with
                | Some gift ->
                    cmds0 <- PlaneGifted gift :: cmds0
                | None ->
                    ()
            | _ -> ()

            // Execute commands gathered from player data creation
            for cmd in cmds0 do
                let ctx, msgs = context.Execute(cmd)
                yield! AsyncSeq.ofSeq msgs
                context <- ctx

            // Update player data and context
            let players2 = players |> Map.map(fun vehicleId data -> data.HandleEntry(context, entry.Data))
            players <-
                players2
                |> Map.map (fun _ (player, _) -> player)
                |> Map.filter (fun _ player ->
                    match player.State with
                    | MissionEnded -> false
                    | _ -> true)

            let context2, msgs =
                players2
                |> Map.toSeq
                |> Seq.collect (snd >> snd) // Turn into a command seq
                |> Seq.fold (fun (context : Context, msgs) cmd ->
                    let context, msgs2 = context.Execute(cmd)
                    context, msgs @ msgs2) (context, [])

            // Update context with kill entries
            let context2 =
                match entry.Data with
                | :? KillEntry as kill -> context2.HandleKill(kill)
                | _ -> context2

            context <- context2

            // Yield messages generated while updating player data
            yield! AsyncSeq.ofSeq msgs
    }
