open System
open System.IO

let args =
    fsi.CommandLineArgs
    |> Seq.skipWhile ((<>) "--")
    |> Seq.skip(1)
    |> List.ofSeq

printfn "%s" (args |> Seq.map (sprintf "'%s'") |> String.concat ", ")
for path in args do
    match File.ReadAllLines(path) |> List.ofArray with
    | xs :: ys :: _ ->
        use transposed = File.CreateText(Path.Combine(Path.GetDirectoryName(path), Path.GetFileName(path) + "-t.csv"))
        for x, y in Seq.zip (xs.Split ',') (ys.Split ',') do
            transposed.WriteLine(sprintf "%s, %s" x y)
    | _ -> failwith "Input file must have two lines"

