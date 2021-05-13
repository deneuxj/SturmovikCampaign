#r "nuget: FSharp.Charting"
#r "nuget: SturmovikMission.DataProvider"
#r "System.Windows.Forms.DataVisualization"

open FSharp.Charting
open SturmovikMission.DataProvider.Parsing

let (|AsVector2|_|) s =
    match s with
    | ReLit "<" (ReFloat(x, ReLit "," (ReFloat(y, ReLit ">" s)))) ->
        Some((x, y), s)
    | _ ->
        None

let (|AsList|_|) inner s =
    match s with
    | ReLit "[" s ->
        let rec work s =
            match inner s with
            | Some(v, s) ->
                match s with
                | ReLit "]" s ->
                    Some([v], s)
                | ReLit ";" s ->
                    match work s with
                    | Some(vs, s) -> Some (v :: vs, s)
                    | _ -> None
                | _ ->
                    None
            | _ -> None
        match s with
        | ReLit "]" s ->
            Some([], s)
        | _ ->
            work s
    | _ ->
        None

let (|AsVectors|_|) =
    let inner = function AsVector2(v, s) -> Some(v, s) | _ -> None
    function
    | AsList inner (vs, s) -> Some (vs, s)
    | _ -> None

let (|AsPolygon|_|) s =
    match s with
    | AsVectors(vs, s) ->
        match vs with
        | [] -> None
        | _ -> Some(List.last vs :: vs, s)
    | _ ->
        None

let (|AsPolygons|_|) =
    let inner = function AsPolygon(poly, s) -> Some(poly, s) | _ -> None
    function
    | AsList inner (polygons, s) -> Some(polygons, s)
    | _ -> None

let (|AsTestData|_|) s =
    match s with
    | AsPolygons(polygons, ReLit "," (ReInt(seed, ReLit "," (AsPolygon(subShape, s))))) ->
        Some((polygons, seed, subShape), s)
    | _ ->
        None

let s =
    Stream.FromString """
[[<81.78999, 13.75>; <88.409996, 14.65>; <85.63, 19.13>; <80.729996, 18.23>];
[<10.38, 18.6>; <17.0, 22.85>; <13.16, 24.130001>; <9.320001, 24.8>];
[<11.65, 26.16>; <16.55, 27.06>; <15.49, 31.54>; <7.81, 33.49>; <8.87, 29.01>];
[<91.520004, 64.03>; <94.91, 69.79>; <95.36, 73.37>; <88.740005, 72.47>;
 <87.68, 66.94>]], -915135539,
    [<44.69, 4.54>; <45.75, 6.1099997>; <42.36, 12.6>; <38.52, 13.27>;
     <39.58, 8.79>; <40.85, 5.21>]
    """

match s with
| AsTestData(x, _) -> printfn "%A" x
| _ -> printfn "BOO"

match s with
| AsTestData((polys, seed, shape), _) ->
    let polys =
        polys
        |> List.map (fun poly -> Chart.Line(poly, Color=System.Drawing.Color.Blue))
    let shape =
        Chart.Line(shape, Color=System.Drawing.Color.Red)
    Chart.Combine(shape :: polys).ShowChart()
| _ ->
    failwith "Failed to parse test data"