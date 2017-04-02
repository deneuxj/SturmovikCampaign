module Campaign.MinMax

open Campaign.Util
open Campaign.WorldDescription
open Campaign.WorldState
open System.Threading

type Move =
    { Start : int
      Destination : int
      Force : float32<E>
    }

let private depart (arr : float32<E>[]) start force =
    arr.[start] <- arr.[start] - force

let private arrive (arr : float32<E>[]) dest force =
    arr.[dest] <- arr.[dest] + force

type BoardState =
    { Names : string[]
      Owners : CoalitionId option []
      AxisForces : float32<E> []
      AlliesForces : float32<E> []
    }
with
    static member Create(state : WorldState) =
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
        { Names = names
          Owners = owners
          AxisForces = axisForces
          AlliesForces = alliesForces
        }

    member this.Clone() =
        { Names = this.Names
          Owners = this.Owners |> Array.copy
          AxisForces = this.AxisForces |> Array.copy
          AlliesForces = this.AlliesForces |> Array.copy
        }

    member this.DoMove(axis : Move list, allies : Move list) =
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
        for i in destinations do
            let axisForce = this.AxisForces.[i]
            let alliesForce = this.AlliesForces.[i]
            let oldOwner = this.Owners.[i]
            if axisForce > alliesForce then
                restore := (i, axisForce, alliesForce, oldOwner) :: !restore
                this.AxisForces.[i] <- axisForce - alliesForce
                this.AlliesForces.[i] <- 0.0f<E>
                this.Owners.[i] <- Some Axis
            elif axisForce < alliesForce then
                restore := (i, axisForce, alliesForce, oldOwner) :: !restore
                this.AxisForces.[i] <- 0.0f<E>
                this.AlliesForces.[i] <- alliesForce - axisForce
                this.Owners.[i] <- Some Allies
            elif axisForce > 0.0f<E> || alliesForce > 0.0f<E> then // Equal forces, it's a draw
                restore := (i, axisForce, alliesForce, oldOwner) :: !restore
                this.AxisForces.[i] <- 0.0f<E>
                this.AlliesForces.[i] <- 0.0f<E>
        !restore

    member this.UndoMove(axis : Move list, allies : Move list, restore : _ list) =
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

let allInvasions (neighboursOf : int -> int[]) (state : BoardState) coalition =
    let someCoalition = Some coalition
    [|
        for i in 0 .. state.Owners.Length - 1 do
            assert(state.AxisForces.[i] = 0.0f<E> || state.AlliesForces.[i] = 0.0f<E>)
            let hasForces = state.AxisForces.[i] > 0.0f<E> || state.AlliesForces.[i] > 0.0f<E>
            if state.Owners.[i] = someCoalition && hasForces then
                for j in neighboursOf i do
                    if state.Owners.[j] <> someCoalition then
                        let force =
                            match coalition with
                            | Axis -> state.AxisForces.[i]
                            | Allies -> state.AlliesForces.[i]
                        yield { Start = i; Destination = j; Force = force }
                
    |]

let allReinforcements (neighboursOf : int -> int[]) (state : BoardState) coalition =
    let someCoalition = Some coalition
    [|
        for i in 0 .. state.Owners.Length - 1 do
            assert(state.AxisForces.[i] = 0.0f<E> || state.AlliesForces.[i] = 0.0f<E>)
            let hasForces = state.AxisForces.[i] > 0.0f<E> || state.AlliesForces.[i] > 0.0f<E>
            if state.Owners.[i] = someCoalition && hasForces then
                for j in neighboursOf i do
                    if state.Owners.[j] = someCoalition then
                        let force =
                            match coalition with
                            | Axis -> state.AxisForces.[i]
                            | Allies -> state.AlliesForces.[i]
                        yield { Start = i; Destination = j; Force = force }
    |]

let evalState (valueOfRegion : int -> float32<E>) (state : BoardState) =
    let territory =
        state.Owners
        |> Seq.indexed
        |> Seq.sumBy (fun (i, owner) ->
            match owner with
            | None -> 0.0f<E>
            | Some Axis -> -valueOfRegion i
            | Some Allies -> valueOfRegion i)
    let alliesArmies =
        state.AlliesForces
        |> Array.sum
    let axisArmies =
        state.AxisForces
        |> Array.sum
    territory + alliesArmies - axisArmies

let prepareEval (world : World) (state : WorldState) =
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
                    yield indexOfRegion af.Region, afState.TotalPlaneValue + 1000.0f<E>
            ]
            |> List.groupBy fst
            |> List.map (fun (i, values) -> i, values |> List.sumBy snd)
        for k, v in roots do
            printfn "R%2d %5.2f" k v
        let update i j oldValue (newValue : float32<E>) =
            let n = world.Regions.[i].Neighbours.Length
            let newValue = newValue / (float32 n)
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
    (getSuccessors, valueOfRegion), BoardState.Create state

let minMax (cancel : CancellationToken) maxDepth (neighboursOf, valueOfRegion) (board : BoardState) =
    let rec bestMoveAtDepth depth =
        let axisMoves =
            let axisInvasions = allInvasions neighboursOf board Axis
            let axisReinforcements = allReinforcements neighboursOf board Axis
            if axisInvasions.Length = 0 then
                axisReinforcements |> Seq.map (List.singleton)
            elif axisReinforcements.Length = 0 then
                axisInvasions |> Seq.map (List.singleton)
            else
                Array.cross2 axisInvasions axisReinforcements
                |> Seq.map (fun (x, y) -> if x.Start <> y.Start then [x; y] else [x])
            |> Seq.append [[]]
        let alliesMoves =
            let alliesInvasions = allInvasions neighboursOf board Allies
            let alliesReinforcements = allReinforcements neighboursOf board Allies
            if alliesInvasions.Length = 0 then
                alliesReinforcements |> Seq.map (List.singleton)
            elif alliesReinforcements.Length = 0 then
                alliesInvasions |> Seq.map (List.singleton)
            else
                Array.cross2 alliesInvasions alliesReinforcements
                |> Seq.map (fun (x, y) -> if x.Start <> y.Start then [x; y] else [x])
            |> Seq.append [[]]
        let bestMove =
            axisMoves
            |> Seq.fold (fun soFar axisMove ->
                let alliesResponse, value =
                    alliesMoves
                    |> Seq.fold (fun soFar alliesMove ->
                        //let saved = board.Clone()
                        let restore = board.DoMove(axisMove, alliesMove)
                        let value =
                            if depth >= maxDepth || cancel.IsCancellationRequested then
                                evalState valueOfRegion board
                            else
                                let _, value = bestMoveAtDepth (depth + 1)
                                value
                        board.UndoMove(axisMove, alliesMove, restore)
                        //assert(saved = board)
                        let _, refValue = soFar
                        if value >= refValue then
                            (alliesMove, value)
                        else
                            soFar
                    ) ([], 1.0f<E> * System.Single.NegativeInfinity)
                let _, refValue = soFar
                if value <= refValue then
                    ((axisMove, alliesResponse), value)
                else
                    soFar
            ) (([], []), 1.0f<E> * System.Single.PositiveInfinity)
        bestMove
    bestMoveAtDepth 0

let rec play minMax (board : BoardState) =
    seq {
        let move, (value : float32<E>) = minMax board
        board.DoMove(move) |> ignore
        yield sprintf "Balance: %5.2f" value
        yield sprintf "GER moves: %A" (fst move)
        yield sprintf "RUS moves: %A" (snd move)
        yield board.DisplayString
        yield! play minMax board
    }