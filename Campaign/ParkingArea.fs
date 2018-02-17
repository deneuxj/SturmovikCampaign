module Campaign.ParkingArea

open System.Numerics
open VectorExtension

let serpentine() =
    let rec up n (x, y) =
        seq {
            for i in 1 .. n do
                yield (x, y + i)
            yield! right n (x, y + n)
        }
    and right n (x, y) =
        seq {
            for i in 1 .. n do
                yield (x + i, y)
            yield! down (n + 1) (x + n, y)
        }
    and down n (x, y) =
        seq {
            for i in 1 .. n do
                yield (x, y - i)
            yield! left n (x, y - n)
        }
    and left n (x, y) =
        seq {
            for i in 1 .. n do
                yield (x - i, y)
            yield! up (n + 1) (x - n, y)
        }
    seq {
        yield (0, 0)
        yield! up 1 (0, 0)
    }

let minParkingSpacing = 10.0f
let maxParkingSpacing = 35.0f

/// Compute positions in an area where a given numbers of vehicles can be parked. Uses deterministic serpentine.
let computeParkingPositions (area : Vector2 list) (numVehicles : int) =
    let center =
        let n = float32(List.length area)
        (List.sum area) / n
    let surface = Vector2.ConvexPolygonArea area
    let surfacePerVehicle = surface / float32 numVehicles
    let side = sqrt surfacePerVehicle |> max minParkingSpacing |> min maxParkingSpacing
    serpentine()
    |> Seq.map (fun (x, y) -> center + side * Vector2(float32 x, float32 y))
    |> Seq.take numVehicles
    |> Seq.filter (fun v -> v.IsInConvexPolygon area)
    |> List.ofSeq

/// Compute positions in area where a given numbers of vehicles can be parked. Uses randomly located fixed patterns of positions (e.g. positions under nets in a static net group)
let computeRandomParkingPositions (pattern : Vector2[]) (area : Vector2 list) (numVehicles : int) =
    let random = System.Random()
    let center =
        let n = float32(List.length area)
        (List.sum area) / n
    let maxRadius =
        area
        |> Seq.maxBy (fun v -> (center - v).Length())
    let patterned center =
        seq {
            for rel in pattern do
                yield center + rel
        }
    let patternInArea center =
        patterned center
        |> Seq.forall (fun v2 -> v2.IsInConvexPolygon area)
    let rec work numTriesLeft (positions : Vector2[]) =
        let indexed = Seq.indexed positions
        // Positions that don't overlap with other positions
        let filtered =
            [|
                for (i, orig) in indexed do
                    let tooClose =
                        positions
                        |> Seq.take i
                        |> Seq.collect patterned
                        |> Seq.exists (fun v ->
                            patterned orig
                            |> Seq.exists(fun v2 ->
                                (v - v2).Length() < 10.0f))
                    if not tooClose then
                        yield orig
            |]
        if filtered.Length * pattern.Length >= numVehicles || numTriesLeft = 0 then
            positions
        else
        // New random positions
            let extra =
                Seq.initInfinite (fun _ ->
                    let r = float32(random.NextDouble()) * maxRadius
                    let angle = 2.0 * System.Math.PI * random.NextDouble()
                    let dx = float32(cos angle)
                    let dz = float32(sin angle)
                    center + r * Vector2(dx, dz))
                |> Seq.truncate 1000
                |> Seq.filter patternInArea
            let positions =
                Seq.append filtered extra
                |> Seq.truncate (1 + numVehicles / pattern.Length)
                |> Array.ofSeq
            work (numTriesLeft - 1) positions
    work 50 [||]
