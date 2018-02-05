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

/// Compute positions in area where a given numbers of vehicles can be parked. Uses random positions
let computeRandomParkingPositions (area : Vector2 list) (numVehicles : int) =
    let random = System.Random()
    let center =
        let n = float32(List.length area)
        (List.sum area) / n
    let maxRadius =
        area
        |> Seq.maxBy (fun v -> (center - v).Length())
    let rec work numTriesLeft (positions : Vector2[]) =
        let indexed = Seq.indexed positions
        let filtered =
            [|
                for (i, v) in indexed do
                    let tooClose =
                        indexed
                        |> Seq.exists (fun (j, v2) -> i <> j && (v - v2).Length() < 10.0f)
                    if not tooClose then
                        yield v
            |]
        let extra =
            Seq.initInfinite (fun _ ->
                let r = float32(random.NextDouble()) * maxRadius
                let angle = 2.0 * System.Math.PI * random.NextDouble()
                let dx = float32(cos angle)
                let dz = float32(sin angle)
                center + r * Vector2(dx, dz))
            |> Seq.truncate 1000
            |> Seq.filter (fun v -> v.IsInConvexPolygon area)
            |> Seq.truncate (numVehicles - filtered.Length)
            |> Array.ofSeq
        let positions = Array.append filtered extra
        if extra.Length = 0 || numTriesLeft = 0 then
            positions
        else
            work (numTriesLeft - 1) positions
    work 100 [||]
