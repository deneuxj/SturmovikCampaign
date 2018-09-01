// List dates in campaign dir, select one, remove all other dates, make remaining date current
// Use that to e.g. test extracting results from a specific date.

#I "../bin/Debug"
#r "Campaign.dll"
#r "DataProvider.dll"

#load "Configuration.fsx" 

open System

let pageSize = 5
let dates =
    let re = Text.RegularExpressions.Regex("results_(.*).xml")
    [
        for file in IO.Directory.EnumerateFiles(Configuration.config.OutputDir, "results_*.xml") do
            let file = IO.Path.GetFileName(file)
            let m = re.Match(file)
            if m.Success then
                yield string(m.Groups.[1])
    ]
    |> Seq.chunkBySize pageSize

let rec work (iter : System.Collections.Generic.IEnumerator<string[]>) (retry : bool) =
    if retry || iter.MoveNext() then
        iter.Current
        |> Seq.mapi (fun i date -> sprintf "%2d  %s" (i + 1) date)
        |> String.concat "\n"
        |> printfn "%s"
        printfn "1..N -> select, Q -> exit, Space -> next"
        let cmd = Console.ReadLine().Trim().ToLower()
        match cmd with
        | "q" -> None
        | "" -> work iter false
        | _ ->
            match System.Int32.TryParse(cmd) with
            | (true, n) ->
                if 1 <= n && n <= iter.Current.Length then
                    Some iter.Current.[n - 1]
                else
                    work iter true
            | _ ->
                work iter true
    else
        work (dates.GetEnumerator()) false

let date =
    if not(Seq.isEmpty dates) then
        work (dates.GetEnumerator()) false
    else
        None

match date with
| Some date ->
    for file in IO.Directory.EnumerateFiles(Configuration.config.OutputDir) do
        if file.Contains(date) || file.EndsWith("world.xml") then
            printfn "Keep %s" file
        else
            IO.File.Delete(file)
    for file in IO.Directory.EnumerateFiles(Configuration.config.OutputDir) do
        let dst = file.Replace("_" + date, "")
        if dst <> file then
            IO.File.Move(file, dst)
| None ->
    ()