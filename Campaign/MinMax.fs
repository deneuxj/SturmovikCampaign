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

type CombinedMove =
    { Axis : Move list
      Allies : Move list
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
      mutable Value : float32
      ValueOf : int -> float32
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
//            for k, v in roots do
//                printfn "R%2d %5.2f" k v
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
            fun i -> values.[i] |> float32
        let initialValue =
            owners
            |> Seq.indexed
            |> Seq.sumBy (fun (i, owner) ->
                match owner with
                | None -> 0.0f
                | Some Axis -> -valueOfRegion i
                | Some Allies -> valueOfRegion i)
        { Names = names
          Owners = owners
          AxisForces = axisForces
          AlliesForces = alliesForces
          Value = float32 initialValue
          ValueOf = valueOfRegion
        }, getSuccessors

    member this.Clone() =
        { Names = this.Names
          Owners = this.Owners |> Array.copy
          AxisForces = this.AxisForces |> Array.copy
          AlliesForces = this.AlliesForces |> Array.copy
          Value = this.Value
          ValueOf = this.ValueOf
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
        let oldValue = this.Value
        for i in destinations do
            let axisForce = this.AxisForces.[i]
            let alliesForce = this.AlliesForces.[i]
            let oldOwner = this.Owners.[i]
            let regionValue = this.ValueOf i
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
            this.Value <-
                match oldOwner, this.Owners.[i] with
                | None, Some Axis -> this.Value - regionValue
                | None, Some Allies -> this.Value + regionValue
                | Some Allies, Some Axis -> this.Value - 2.0f * regionValue
                | Some Axis, Some Allies -> this.Value + 2.0f * regionValue
                | None, None
                | Some Axis, Some Axis
                | Some Allies, Some Allies -> this.Value
                | Some _, None -> failwith "Region cannot be captured by neutral coalition"
        !restore, oldValue

    member this.UndoMove(combined : CombinedMove, restore : _ list, oldValue : float32) =
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
        this.Value <- oldValue

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


let minMax (cancel : CancellationToken) maxDepth (neighboursOf) (board : BoardState) =
    let rec bestMoveAtDepth depth =
        let axisMoves =
            allMoves neighboursOf board Axis
            |> Seq.append [[]]
        let alliesMoves =
            allMoves neighboursOf board Allies
//            |> Seq.append [[]]
        let (axis, allies), value =
            axisMoves
            |> Seq.fold (fun soFar axisMove ->
                if cancel.IsCancellationRequested then
                    soFar
                else
                    let alliesResponse, value =
                        alliesMoves
                        |> Seq.fold (fun soFar alliesMove ->
                            if cancel.IsCancellationRequested then
                                soFar
                            else
                                //let saved = board.Clone()
                                let combined = { Axis = axisMove; Allies = alliesMove }
                                let restore, oldValue = board.DoMove(combined)
                                let value =
                                    if depth >= maxDepth then
                                        board.Value
                                    else
                                        let _, value = bestMoveAtDepth (depth + 1)
                                        value
                                board.UndoMove(combined, restore, oldValue)
                                //assert(saved = board)
                                let _, refValue = soFar
                                if value >= refValue then
                                    (alliesMove, value)
                                else
                                    soFar
                        ) ([], System.Single.NegativeInfinity)
                    let _, refValue = soFar
                    if value <= refValue then
                        ((axisMove, alliesResponse), value)
                    else
                        soFar
            ) (([], []), System.Single.PositiveInfinity)
        { Axis = axis; Allies = allies }, value
    let x = bestMoveAtDepth 0
    printfn "DBG: %A" x
    x


let rec play minMax (board : BoardState) =
    seq {
        let move, (value : float32) = minMax board
        board.DoMove(move) |> ignore
        yield sprintf "Balance: %5.2f" value
        yield sprintf "GER moves: %A" (move.Axis)
        yield sprintf "RUS moves: %A" (move.Allies)
        yield board.DisplayString
        yield! play minMax board
    }