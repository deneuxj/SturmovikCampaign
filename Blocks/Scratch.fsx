#I @"bin\Debug\netstandard2.0"
#I @"C:\Users\johann\Documents\SturmovikMission-git\DataProvider\src\DataProvider.DesignTime\bin\Debug\netstandard2.0"
#r "Blocks.dll"
#r "System.Numerics.Vectors"

open System.Numerics
open VectorExtension

let poly1 =
    [
        (0, 0)
        (10, 0)
        (10, 10)
        (0, 10)
    ]
    |> List.map (fun (x, y) -> Vector2(float32 x, float32 y))

let poly2 =
    poly1
    |> List.map (fun v -> v + Vector2(5.0f, 5.0f))

let poly12 = intersectConvexPolygons(poly1, poly2)

let poly3 =
    poly1
    |> List.map (fun v -> v + Vector2(15.0f, 15.0f))

let poly13 = intersectConvexPolygons(poly1, poly3)

let poly21 = intersectConvexPolygons(poly2, poly1)

let poly11 = intersectConvexPolygons(poly1, poly1)

let poly112 = intersectConvexPolygons(poly1, poly12 |> Option.get)

let poly4 =
    [
        (-1, 1)
        (11, 1)
        (11, 2)
        (-1, 2)
    ]
    |> List.map (fun (x, y) -> Vector2(float32 x, float32 y))

let poly14 = intersectConvexPolygons(poly1, poly4)
