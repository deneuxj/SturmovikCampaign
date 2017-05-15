module Campaign.MinMax

open System.Threading

open Campaign.Util
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.BasicTypes

type Move =
    { Start : int
      Destination : int
      Force : float32<E>
    }

type CombinedMove =
    { Axis : Move list
      Allies : Move list
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
        { this with Territory = this.Territory }

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
            let m =
                world.Regions
                |> List.map (fun region ->
                    region.Neighbours
                    |> List.filter (fun ngh -> world.Roads |> List.exists (fun road -> road.MatchesEndpoints(region.RegionId, ngh).IsSome))
                    |> List.map indexOfRegion
                    |> Array.ofList)
                |> List.indexed
                |> dict
            fun i -> m.[i]
        let valueOfRegion =
            let roots =
                [
                    for region, regState in List.zip world.Regions state.Regions do
                        let value = regState.ProductionCapacity(region) * 48.0f<H>
                        yield indexOfRegion region.RegionId, value
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
                Algo.propagate (getSuccessors >> List.ofArray) update roots
            for kvp in values do
                printfn "%2d %5.2f" kvp.Key kvp.Value
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
            axisForces
            |> Array.mapi (fun i qty -> if qty > 0.0f<E> && hasFactory i then 1 else 0)
            |> Array.sum
        let numAlliesFactories =
            alliesForces
            |> Array.mapi (fun i qty -> if qty > 0.0f<E> && hasFactory i then 1 else 0)
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
        for order in axis do
            let force = order.Force
            depart this.AxisForces order.Start force
            arrive this.AxisForces order.Destination force
        for order in allies do
            let force = order.Force
            depart this.AlliesForces order.Start force
            arrive this.AlliesForces order.Destination force
        let destinations =
            axis @ allies
            |> List.map (fun order -> order.Destination)
            |> List.distinct
        let oldScore = { this.Score with Territory = this.Score.Territory } // Make a new copy of Score
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
        for order in List.rev allies do
            let force = order.Force
            arrive this.AlliesForces order.Start force
            depart this.AlliesForces order.Destination force
        for order in List.rev axis do
            let force = order.Force
            arrive this.AxisForces order.Start force
            depart this.AxisForces order.Destination force
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

let allMoves (neighboursOf : int -> int[]) (state : BoardState) coalition =
    let someCoalition = Some coalition
    [|
        for i in 0 .. state.Owners.Length - 1 do
            assert(state.AxisForces.[i] = 0.0f<E> || state.AlliesForces.[i] = 0.0f<E>)
            let hasForces = state.AxisForces.[i] > 0.0f<E> || state.AlliesForces.[i] > 0.0f<E>
            if state.Owners.[i] = someCoalition && hasForces then
                let force =
                    match coalition with
                    | Axis -> state.AxisForces.[i]
                    | Allies -> state.AlliesForces.[i]
                for j in neighboursOf i do
                    if force > 5.0f * MediumTank.Cost then
                        for k in neighboursOf i do
                            if j < k then
                                yield [
                                    { Start = i; Destination = j; Force = 0.5f * force }
                                    { Start = i; Destination = k; Force = 0.5f * force }
                                ]
                    yield [
                            { Start = i; Destination = j; Force = force }
                    ]
    |]

type BoardEvaluation =
    | Defeat of CoalitionId * int
    | Ongoing of float32
with
    static member Lt(a : BoardEvaluation, b : BoardEvaluation) =
        match a, b with
        | Ongoing u, Ongoing v -> u < v
        | Ongoing _, Defeat(Allies, _)
        | Defeat(Axis, _), Ongoing _ -> false
        | Ongoing _, Defeat(Axis, _)
        | Defeat(Allies, _), Ongoing _ -> true
        | Defeat(Axis, _), Defeat(Allies, _) -> false
        | Defeat(Allies, _), Defeat(Axis, _) -> false
        | Defeat(Axis, d1), Defeat(Axis, d2) -> d1 > d2
        | Defeat(Allies, d1), Defeat(Allies, d2) -> d1 < d2

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
        System.Collections.Generic.Dictionary<CoalitionId option[] * float32<E>[] * float32<E>[], BoardEvaluation * int>(positionCompare)
    let mutable hits = 0
    let rec bestMoveAtDepth beta depth =
        let axisMoves =
            allMoves neighboursOf board Axis
            |> Seq.append [[]]
        let alliesMoves =
            allMoves neighboursOf board Allies
            |> Seq.append [[]]
        let (axis, allies), value =
            axisMoves
            |> Seq.fold (fun soFar axisMove ->
                if board.Score.NumAlliesFactories = 0 && board.Score.TotalAlliesForces = 0.0f<E> then
                    ((axisMove, []), Defeat(Allies, depth)) |> BoardEvaluation.Min soFar
                else if board.Score.NumAxisFactories = 0 && board.Score.TotalAxisForces = 0.0f<E> then
                    ((axisMove, []), Defeat(Axis, depth)) |> BoardEvaluation.Max soFar
                else if cancel.IsCancellationRequested then
                    soFar
                else if BoardEvaluation.Lt(snd soFar, beta) then
                    soFar
                else
                    let _, alpha = soFar
                    let alliesResponse, value =
                        alliesMoves
                        // Do not allow enemy columns to cross eachother on the road,
                        |> Seq.filter (fun move ->
                            move
                            |> List.exists (fun allyMove ->
                                axisMove
                                |> List.exists (fun axisMove ->
                                    allyMove.Destination = axisMove.Start && allyMove.Start = axisMove.Destination))
                            |> not)
                        |> Seq.fold (fun soFar alliesMove ->
                            if board.Score.NumAxisFactories = 0 && board.Score.TotalAxisForces = 0.0f<E> then
                                (alliesMove, Defeat(Axis, depth)) |> BoardEvaluation.Max soFar
                            else if board.Score.NumAlliesFactories = 0 && board.Score.TotalAlliesForces = 0.0f<E> then
                                (alliesMove, Defeat(Allies, depth)) |> BoardEvaluation.Min soFar
                            else if BoardEvaluation.Lt(alpha, snd soFar) then
                                soFar
                            else if cancel.IsCancellationRequested then
                                soFar
                            else
                                //let saved = board.Clone()
                                let combined = { Axis = axisMove; Allies = alliesMove }
                                let restore, oldValue = board.DoMove(combined)
                                let value =
                                    if depth >= maxDepth then
                                        Ongoing board.Score.Value
                                    else
                                        let entry = (board.Owners, board.AxisForces, board.AlliesForces)
                                        match positionCache.TryGetValue(entry) with
                                        | true, (cachedScore, cachedDepth) when cachedDepth <= depth ->
                                            hits <- hits + 1
                                            cachedScore
                                        | _ ->
                                            let _, value = bestMoveAtDepth (snd soFar) (depth + 1)
                                            positionCache.[entry] <- (value, depth)
                                            value
                                board.UndoMove(combined, restore, oldValue)
                                //assert(saved = board)
                                (alliesMove, value) |> BoardEvaluation.Max soFar
                        ) ([], Defeat(Allies, 0))
                    ((axisMove, alliesResponse), value) |> BoardEvaluation.Min soFar
            ) (([], []), Defeat(Axis, 0))
        { Axis = axis; Allies = allies }, value
    let x = bestMoveAtDepth (Defeat(Allies, 0)) 0
    printfn "Hits: %d" hits
    printfn "DBG: %A" x
    x


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