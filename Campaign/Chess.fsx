#I "bin/Debug"
#r "Campaign.dll"

#load "Configuration.fsx" 

open Configuration

let lines = Campaign.Run.PlayChess.run config
let en = lines.GetEnumerator()
let step() =
    if en.MoveNext() then
        printfn "%s" en.Current
    else
        printfn "DONE"
