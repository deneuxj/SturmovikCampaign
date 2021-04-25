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

/// Extract results from game logs and generate war state update commands.
module Campaign.MissionResults

open FSharp.Control
open System.Numerics

open Util
open Util.RegexActivePatterns
open VectorExtension

open Campaign.Common.BasicTypes
open Campaign.Common.Targets

open Campaign.GameLogEvents
open Campaign.NewWorldDescription
open Campaign.WarState
open Campaign.WarStateUpdate
open Campaign.WarStateUpdate.CommandExecution
open Campaign.Pilots
open Campaign.MissionResultsExtensions
open Campaign.MissionResultsStateMachines

/// A modified asyncSeq builder that update a war state whenever a command is yielded.
type ImpAsyncSeq(warState : IWarState, logger : NLog.Logger) =
    member this.Yield(v) =
        match v with
        | { Command = Some (cmd : Commands) }->
            try
                cmd.Execute(warState) |> ignore
            with
            | e ->
                logger.Error(sprintf "Execution of command in ImpAsyncSeq failed: %s" e.Message)
                logger.Debug(e)
        | _ -> ()
        asyncSeq.Yield(v)

    member this.YieldFrom(source) =
        asyncSeq.YieldFrom(source)

    member this.For(source : AsyncSeq<'T>, action) =
        asyncSeq.For(source, action)

    member this.For(source : 'T seq, action) =
        asyncSeq.For(source, action)

    member this.Zero() = asyncSeq.Zero()

    member this.Combine(seq1, seq2) = asyncSeq.Combine(seq1, seq2)

    member this.TryWith(body, handler) = asyncSeq.TryWith(body, handler)

    member this.Delay(f) = asyncSeq.Delay(f)

/// Extract war state updade commands from the game logs and update the war state.
let processLogs (state : WarState) (logs : AsyncSeq<string>) =
    let logger = NLog.LogManager.GetCurrentClassLogger()

    // We'll be mutating a clone of the state to avoid potential read/write race conditions with consumers (i.e. the live notifier) of the commands.
    let state = state.Clone()

    let impAsyncSeq =
        ImpAsyncSeq(state, logger)

    impAsyncSeq {
        let mutable mappings = Mappings.Empty
        let mappingsController = Mappings.Controller

        let mutable healths = HealthTracker.Empty
        let healthController = HealthTracker.Controller

        let mutable pilots = PlayerFlights.Empty
        let pilotsController = PlayerFlights.Controller

        let handleLine line =
            impAsyncSeq {
                // Controllers PRE
                let newMappings = handlePre(mappingsController, mappings, line, ())
                let newHealths = handlePre(healthController, healths, line, (mappings, upcast state))
                let newPilots = handlePre(pilotsController, pilots, line, (mappings, healths, upcast state))

                // Emit timestamp, needed to inform players of time left in mission
                match line with
                | ObjectEvent(timeStamp, _) | MissionEvent(timeStamp, _) | PlayerEvent(timeStamp, _) | BotEvent(timeStamp, _) ->
                    yield AnnotatedCommand.Create("Timestamp", timeStamp)

                | _ ->
                    ()

                // Controllers POST
                let newMappings, cmds = handlePost(mappingsController, mappings, newMappings, ())
                let newHealths, cmds2 = handlePost(healthController, healths, newHealths, (mappings, upcast state))
                let newPilots, cmds3 = handlePost(pilotsController, pilots, newPilots, (mappings, healths, upcast state))

                for cmd in Seq.concat [ cmds; cmds2; cmds3 ] do
                    yield cmd

                mappings <- newMappings
                healths <- newHealths
                pilots <- newPilots
            }

        let mutable finalTimeStamp = System.TimeSpan(0L)
        for line in logs do
            try
                yield! handleLine line
                match line with
                | MatchesRegex reBase (GroupList (AsInt ticks :: _)) when ticks > 0 ->
                    finalTimeStamp <- System.TimeSpan.OfGameTicks(ticks)
                | _ ->
                    ()
            with exc ->
                logger.Error("Exception while processing log")
                logger.Error(exc)

        // Update health status of players still in the air after the log ends.
        // Do not register their flight, that'll teach them to get back in time on the ground.
        for x in pilots.FlightOfPilot.Values do
            let healthStatus = state.HealthStatusFromHealthLevel(finalTimeStamp, x.FlightRecord.PilotHealth)
            yield
                AnnotatedCommand.Create(
                    sprintf "%s is still in the air" x.PilotData.FullName,
                    System.TimeSpan(System.Int64.MaxValue),
                    UpdatePilot { x.PilotData with Health = healthStatus })
    }