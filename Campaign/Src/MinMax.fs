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

module Campaign.MinMax

open System.Threading

open Util
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.BasicTypes
open NLog
open Campaign.Orders

let private logger = LogManager.GetCurrentClassLogger()

type Move =
    { Start : int
      Destination : int
      Force : float32<E>
      Transport : ColumnTransportType
    }

type CombinedMove =
    { Axis : Move option
      Allies : Move option
    }

let private depart (arr : float32<E>[]) start force =
    arr.[start] <- arr.[start] - force

let private arrive (arr : float32<E>[]) dest force =
    arr.[dest] <- arr.[dest] + force

type ScoreComponents =
    { mutable Territory : float32<E>
      mutable NumAxisFactories : int
      mutable NumAlliesFactories : int
      mutable TotalAxisForces : float32<E>
      mutable TotalAlliesForces : float32<E>
    }
with
    member this.Value =
        let logBound = 3.0f
        let forcesRatio =
            if this.TotalAxisForces = 0.0f<E> then
                logBound
            else if this.TotalAlliesForces = 0.0f<E> then
                -logBound
            else
                log(this.TotalAlliesForces / this.TotalAxisForces)
                |> min logBound
                |> max -logBound
        let ret =
            float32 this.Territory + 500000.0f * forcesRatio
        assert(not(System.Single.IsNaN ret))
        ret

    member this.CopyFrom(score : ScoreComponents) =
        this.Territory <- score.Territory
        this.NumAxisFactories <- score.NumAxisFactories
        this.NumAlliesFactories <- score.NumAlliesFactories
        this.TotalAxisForces <- score.TotalAxisForces
        this.TotalAlliesForces <- score.TotalAlliesForces

    member this.Clone() =
        let ret =
            { this with Territory = this.Territory }
        assert(not <| obj.ReferenceEquals(this, ret))
        ret

type BoardState =
    { Names : string[]
      Owners : CoalitionId option []
      AxisForces : float32<E> []
      AlliesForces : float32<E> []
      Score : ScoreComponents
      ValueOfRegion : int -> float32<E>
      HasRegionFactory : int -> bool
    }
with
    static member Create(world : World, state : WorldState) =
        let owners =
            state.Regions
            |> List.map (fun region -> region.Owner)
            |> Array.ofList
        let axisForces =
            state.Regions
            |> List.map (fun region ->
                if region.Owner = Some Axis then
                    GroundAttackVehicle.AllVehicles
                    |> List.sumBy (fun vehicle -> float32(region.GetNumVehicles(vehicle)) * vehicle.Cost)
                else
                    0.0f<E>)
            |> Array.ofList
        let alliesForces =
            state.Regions
            |> List.map (fun region ->
                if region.Owner = Some Allies then
                    GroundAttackVehicle.AllVehicles
                    |> List.sumBy (fun vehicle -> float32(region.GetNumVehicles(vehicle)) * vehicle.Cost)
                else
                    0.0f<E>)
            |> Array.ofList
        let names =
            state.Regions
            |> List.map (fun r -> r.RegionId)
            |> List.map (function (RegionId name) -> name)
            |> Array.ofList
        let indexOfRegion =
            let m =
                world.Regions
                |> Seq.mapi (fun i region -> region.RegionId, i)
                |> dict
            fun i -> m.[i]
        let getSuccessors =
            let mk t =
                world.Regions
                |> List.map (fun region ->
                    region.Neighbours
                    |> List.filter (fun ngh -> world.PathsFor t |> List.exists (fun road -> road.MatchesEndpoints(region.RegionId, ngh).IsSome))
                    |> List.map indexOfRegion
                    |> Array.ofList)
                |> List.indexed
                |> dict
            let roads = mk ColByRoad
            let trains = mk ColByTrain
            let rivers = mk ColByRiverShip
            let sea = mk ColBySeaShip
            fun t i ->
                match t with
                | ColByRoad -> roads.[i]
                | ColByTrain -> trains.[i]
                | ColByRiverShip -> rivers.[i]
                | ColBySeaShip -> sea.[i]
        let valueOfRegion =
            let roots =
                [
                    // Base value
                    for region in world.Regions do
                        yield indexOfRegion region.RegionId, 5000.0f<E>
                    // Production
                    for region, regState in List.zip world.Regions state.Regions do
                        let value = regState.ProductionCapacity(region, world.SubBlockSpecs, world.ProductionFactor) * 48.0f<H>
                        yield indexOfRegion region.RegionId, value
                    // Planes
                    for af, afState in List.zip world.Airfields state.Airfields do
                        yield indexOfRegion af.Region, afState.TotalPlaneValue + 10000.0f<E>
                ]
                |> List.groupBy fst
                |> List.map (fun (i, values) -> i, values |> List.sumBy snd)
            let update i j oldValue (newValue : float32<E>) =
                let n = world.Regions.[i].Neighbours.Length
                let newValue = newValue / 1.1f
                match oldValue with
                | None ->
                    Some newValue
                | Some oldValue ->
                    if oldValue < newValue then
                        Some newValue
                    else
                        None
            let values =
                let getSuccessors =
                    fun i ->
                        [ColByTrain; ColByRoad; ColByRiverShip; ColBySeaShip]
                        |> Seq.collect(fun t -> getSuccessors t i)
                        |> Seq.distinct
                        |> List.ofSeq
                Algo.propagate getSuccessors update roots
            for kvp in values do
                logger.Info(sprintf "%2d %5.2f" kvp.Key kvp.Value)
            fun i -> values.[i]
        let hasFactory =
            let m =
                world.Regions
                |> List.map (fun region -> not region.Production.IsEmpty)
                |> Array.ofList
            fun i -> m.[i]
        let initialValue =
            owners
            |> Seq.indexed
            |> Seq.sumBy (fun (i, owner) ->
                match owner with
                | None -> 0.0f<E>
                | Some Axis -> -valueOfRegion i
                | Some Allies -> valueOfRegion i)
        let numAxisFactories =
            owners
            |> Array.mapi (fun i coalition -> if coalition = Some Axis && hasFactory i then 1 else 0)
            |> Array.sum
        let numAlliesFactories =
            owners
            |> Array.mapi (fun i coalition -> if coalition = Some Allies && hasFactory i then 1 else 0)
            |> Array.sum
        { Names = names
          Owners = owners
          AxisForces = axisForces
          AlliesForces = alliesForces
          Score =
            {
                Territory = initialValue
                NumAxisFactories = numAxisFactories
                NumAlliesFactories = numAlliesFactories
                TotalAxisForces = Array.sum axisForces
                TotalAlliesForces = Array.sum alliesForces
            }
          ValueOfRegion = valueOfRegion
          HasRegionFactory = hasFactory
        }, getSuccessors

    member this.Clone() =
        { this with
            Owners = this.Owners |> Array.copy
            AxisForces = this.AxisForces |> Array.copy
            AlliesForces = this.AlliesForces |> Array.copy
            Score = this.Score.Clone()
        }

    member this.DoMove(combined : CombinedMove) =
        let axis = combined.Axis
        let allies = combined.Allies
        let restore = ref []
        axis
        |> Option.iter (fun order ->
            let force = order.Force
            depart this.AxisForces order.Start force
            arrive this.AxisForces order.Destination force)
        allies
        |> Option.iter(fun order ->
            let force = order.Force
            depart this.AlliesForces order.Start force
            arrive this.AlliesForces order.Destination force)
        let destinations =
            match axis, allies with
            | None, None -> []
            | None, Some order
            | Some order, None ->
                [order.Destination]
            | Some axis, Some allies ->
                if axis.Destination <> allies.Destination then
                    [axis.Destination; allies.Destination]
                else
                    [axis.Destination]
        let oldScore = this.Score.Clone()
        for i in destinations do
            let axisForce = this.AxisForces.[i]
            let alliesForce = this.AlliesForces.[i]
            let oldOwner = this.Owners.[i]
            let regionValue = this.ValueOfRegion i
            if axisForce > alliesForce then
                restore := (i, axisForce, alliesForce, oldOwner) :: !restore
                this.AxisForces.[i] <- axisForce - alliesForce
                this.AlliesForces.[i] <- 0.0f<E>
                this.Owners.[i] <- Some Axis
                if this.HasRegionFactory i then
                    if oldOwner = Some Allies then
                        this.Score.NumAlliesFactories <- this.Score.NumAlliesFactories - 1
                    if oldOwner <> Some Axis then
                        this.Score.NumAxisFactories <- this.Score.NumAxisFactories + 1
            elif axisForce < alliesForce then
                restore := (i, axisForce, alliesForce, oldOwner) :: !restore
                this.AxisForces.[i] <- 0.0f<E>
                this.AlliesForces.[i] <- alliesForce - axisForce
                this.Owners.[i] <- Some Allies
                if this.HasRegionFactory i then
                    if oldOwner <> Some Allies then
                        this.Score.NumAlliesFactories <- this.Score.NumAlliesFactories + 1
                    if oldOwner = Some Axis then
                        this.Score.NumAxisFactories <- this.Score.NumAxisFactories - 1
            elif axisForce > 0.0f<E> || alliesForce > 0.0f<E> then // Equal forces, it's a draw
                restore := (i, axisForce, alliesForce, oldOwner) :: !restore
                this.AxisForces.[i] <- 0.0f<E>
                this.AlliesForces.[i] <- 0.0f<E>
            else
                failwith "Encountered move without forces"
            this.Score.TotalAxisForces <- this.Score.TotalAxisForces - min axisForce alliesForce
            this.Score.TotalAlliesForces <- this.Score.TotalAlliesForces - min axisForce alliesForce
            this.Score.Territory <-
                match oldOwner, this.Owners.[i] with
                | None, Some Axis -> this.Score.Territory - regionValue
                | None, Some Allies -> this.Score.Territory + regionValue
                | Some Allies, Some Axis -> this.Score.Territory - 2.0f * regionValue
                | Some Axis, Some Allies -> this.Score.Territory + 2.0f * regionValue
                | None, None
                | Some Axis, Some Axis
                | Some Allies, Some Allies -> this.Score.Territory
                | Some _, None -> failwith "Region cannot be captured by neutral coalition"
        !restore, oldScore

    member this.UndoMove(combined : CombinedMove, restore : _ list, oldScore : ScoreComponents) =
        let axis = combined.Axis
        let allies = combined.Allies
        for r in List.rev restore do
            let i, axis, allies, owner = r
            this.AxisForces.[i] <- axis
            this.AlliesForces.[i] <- allies
            this.Owners.[i] <- owner
        allies
        |> Option.iter(fun order ->
            let force = order.Force
            arrive this.AlliesForces order.Start force
            depart this.AlliesForces order.Destination force)
        axis
        |> Option.iter(fun order ->
            let force = order.Force
            arrive this.AxisForces order.Start force
            depart this.AxisForces order.Destination force)
        this.Score.CopyFrom(oldScore)

    member this.DisplayString =
        seq {
            for i, (name, (owner, allies, axis)) in Seq.indexed (Seq.zip this.Names (Seq.zip3 this.Owners this.AlliesForces this.AxisForces)) do
                let owner =
                    match owner with
                    | None -> "---"
                    | Some Axis -> "GER"
                    | Some Allies -> "RUS"
                yield sprintf "%2d %-20s %s %6.0f %6.0f" i name owner -axis allies
        }
        |> String.concat "\n"

let allMoves (neighboursOf : ColumnTransportType * int -> int[]) (state : BoardState) (coalition : CoalitionId) =
    let someCoalition = Some coalition
    let tankForceThreshold = 10.0f * MediumTank.Cost
    let defensiveForceThreshold = 5.0f * MediumTank.Cost
    let aggressiveTransport = ColumnTransportType.All |> List.filter ((<>) ColByTrain)
    let moves i aggressive =
        seq {
            assert(state.AxisForces.[i] = 0.0f<E> || state.AlliesForces.[i] = 0.0f<E>)
            let hasForces = state.AxisForces.[i] > 0.0f<E> || state.AlliesForces.[i] > 0.0f<E>
            let means = if aggressive then aggressiveTransport else ColumnTransportType.All
            if state.Owners.[i] = someCoalition && hasForces then
                let force =
                    match coalition with
                    | Axis -> state.AxisForces.[i]
                    | Allies -> state.AlliesForces.[i]
                for transport in means do
                    let moveableForce =
                        force
                        |> min (float32 transport.MaxNumVehicles * MediumTank.Cost)
                    for j in neighboursOf(transport, i) do
                        if aggressive && state.Owners.[j] <> someCoalition then
                            // Invade in full force, leaving home undefended.
                            yield { Start = i; Destination = j; Force = moveableForce; Transport = transport }
                            let opposing =
                                match coalition with
                                | Allies -> state.AxisForces.[j]
                                | Axis -> state.AlliesForces.[j]
                            // Invade with 25% advantage, leave rest to defend home.
                            if moveableForce > 1.25f * opposing && opposing > defensiveForceThreshold then
                                yield { Start = i; Destination = j; Force = 1.25f * opposing; Transport = transport }
                        if not aggressive && state.Owners.[j] = someCoalition then
                            yield { Start = i; Destination = j; Force = moveableForce; Transport = transport }
        }
    seq {
        for i in 0 .. state.Owners.Length - 1 do
            yield! moves i true
        for i in 0 .. state.Owners.Length - 1 do
            yield! moves i false
    }

type BoardEvaluation =
    | Defeat of CoalitionId * int * string
    | Ongoing of float32
with
    static member Lt(a : BoardEvaluation, b : BoardEvaluation) =
        match a, b with
        | Ongoing u, Ongoing v -> u < v
        | Ongoing _, Defeat(Allies, _, _)
        | Defeat(Axis, _, _), Ongoing _ -> false
        | Ongoing _, Defeat(Axis, _, _)
        | Defeat(Allies, _, _), Ongoing _ -> true
        | Defeat(Axis, _, _), Defeat(Allies, _, _) -> false
        | Defeat(Allies, _, _), Defeat(Axis, _, _) -> true
        | Defeat(Axis, d1, _), Defeat(Axis, d2, _) -> d1 > d2
        | Defeat(Allies, d1, _), Defeat(Allies, d2, _) -> d1 < d2

    static member Min a b =
        if BoardEvaluation.Lt(snd a, snd b) then
            a
        else
            b

    static member Max a b =
        if BoardEvaluation.Lt(snd a, snd b) then
            b
        else
            a

let minMax (cancel : CancellationToken) maxDepth (neighboursOf) (board : BoardState) =
    let positionCompare =
        { new System.Collections.Generic.IEqualityComparer<CoalitionId option[] * float32<E>[] * float32<E>[]> with
              member x.Equals(x1, y) =
                let ownersX, axisX, alliesX = x1
                let ownersY, axisY, alliesY = y
                Array.zip ownersX ownersY |> Array.forall (fun (x, y) -> x = y) &&
                    Array.zip axisX axisY |> Array.forall (fun (x, y) -> x = y) &&
                    Array.zip alliesX alliesY |> Array.forall (fun (x, y) -> x = y)
              member x.GetHashCode(x1) =
                let ownersX, axisX, alliesX = x1
                let h1 = List.ofArray ownersX
                let h2 = List.ofArray axisX
                let h3 = List.ofArray alliesX
                (h1, h2, h3).GetHashCode()
        }
    let positionCache =
        System.Collections.Generic.Dictionary<CoalitionId option[] * float32<E>[] * float32<E>[], BoardEvaluation * CombinedMove list * int>(positionCompare)
    let mutable hits = 0
    let rec bestMoveAtDepth beta depth =
        let axisMoves =
            allMoves neighboursOf board Axis
        let alliesMoves =
            allMoves neighboursOf board Allies
            |> List.ofSeq
        let axisMovesIt = axisMoves.GetEnumerator()
        let ((axis, allies), deepMoves), value =
            let rec workAxis soFar =
                let axisMove =
                    if axisMovesIt.MoveNext() then
                        Some axisMovesIt.Current
                    else
                        None
                if board.Score.NumAlliesFactories = 0 then
                    (((axisMove, None), []), Defeat(Allies, depth, "No factories")) |> BoardEvaluation.Min soFar
                elif board.Score.TotalAlliesForces = 0.0f<E> then
                    (((axisMove, None), []), Defeat(Allies, depth, "No forces")) |> BoardEvaluation.Min soFar
                else if cancel.IsCancellationRequested then
                    soFar
                else if BoardEvaluation.Lt(snd soFar, beta) then
                    soFar
                else
                    let _, alpha = soFar
                    let (alliesResponse, deepMoves), value =
                        workAllies (axisMove, alpha, alliesMoves) ((None, []), Defeat(Allies, 0, "Avoid allies no moves"))
                    let cont =
                        match axisMove with
                        | Some _ ->
                            workAxis
                        | None ->
                            id
                    (((axisMove, alliesResponse), deepMoves), value)
                    |> BoardEvaluation.Min soFar
                    |> cont
            and workAllies(axisMove : Move option, alpha, alliesMoves) soFar =
                let alliesMove, alliesMoves =
                    match alliesMoves with
                    | [] ->
                        None, []
                    | alliesMove :: alliesMoves ->
                        Some alliesMove, alliesMoves
                let skip =
                    match alliesMove, axisMove with
                    | Some alliesMove, Some axisMove ->
                        // Skip the following moves
                        // - A -> B; B -> A (armies swapping positions)
                        // - A -> B; C -> B (reinforcements into battle, not properly handled by campaign update)
                        alliesMove.Destination = axisMove.Start && alliesMove.Start = axisMove.Destination ||
                        alliesMove.Destination = axisMove.Destination
                    | _ ->
                        false
                if skip then
                    soFar
                    |> workAllies(axisMove, alpha, alliesMoves)
                else
                    if board.Score.NumAxisFactories = 0 then
                        ((alliesMove, []), Defeat(Axis, depth, "No factories")) |> BoardEvaluation.Max soFar
                    elif board.Score.TotalAxisForces = 0.0f<E> then
                        ((alliesMove, []), Defeat(Axis, depth, "No forces")) |> BoardEvaluation.Max soFar
                    else if BoardEvaluation.Lt(alpha, snd soFar) then
                        soFar
                    else if cancel.IsCancellationRequested then
                        soFar
                    else
//                            let saved = board.Clone()
                        let combined = { Axis = axisMove; Allies = alliesMove }
                        let restore, oldValue = board.DoMove(combined)
                        let deepMoves, value =
                            if depth >= maxDepth then
                                [], Ongoing board.Score.Value
                            else
                                let entry = (board.Owners, board.AxisForces, board.AlliesForces)
                                match positionCache.TryGetValue(entry) with
                                | true, (cachedScore, deepMoves, cachedDepth) when cachedDepth <= depth ->
                                    hits <- hits + 1
                                    deepMoves, cachedScore
                                | _ ->
                                    let deepMoves, value = bestMoveAtDepth (snd soFar) (depth + 1)
                                    positionCache.[entry] <- (value, deepMoves, depth)
                                    deepMoves, value
                        board.UndoMove(combined, restore, oldValue)
//                            assert(saved.DisplayString = board.DisplayString)
                        let cont =
                            match alliesMove with
                            | Some _ -> workAllies(axisMove, alpha, alliesMoves)
                            | None -> id
                        ((alliesMove, deepMoves), value)
                        |> BoardEvaluation.Max soFar
                        |> cont
            workAxis (((None, None), []), Defeat(Axis, 0, "Avoid axis no moves"))
        { Axis = axis; Allies = allies } :: deepMoves, value
    let moves, score = bestMoveAtDepth (Defeat(Allies, 0, "Initial beta")) 0
    if cancel.IsCancellationRequested then
        logger.Info("Cancelled")
        List.head moves, score
    else
        logger.Trace(sprintf "Hits: %d, Score: %A" hits score)
        match moves with
        | topMove :: followingMoves ->
            logger.Debug(sprintf "DBG: %A\nfollowed by\n%A" topMove followingMoves)
            topMove, score
        | [] ->
            failwith "Empty combined move list"


let rec play minMax (board : BoardState) =
    seq {
        let move, value = minMax board
        board.DoMove(move) |> ignore
        yield sprintf "Balance: %A" value
        yield sprintf "GER moves: %A" (move.Axis)
        yield sprintf "RUS moves: %A" (move.Allies)
        yield board.DisplayString
        yield! play minMax board
    }