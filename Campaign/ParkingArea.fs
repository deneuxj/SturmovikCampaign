module Campaign.ParkingArea

open System.Numerics
open Vector

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
let maxParkingSpacing = 20.0f

/// Compute positions in an area where a given numbers of vehicles can be parked.
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