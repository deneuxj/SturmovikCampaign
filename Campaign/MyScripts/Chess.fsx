#I "../bin/Debug"
#r "Campaign.dll"

#load "Configuration.fsx" 

open Configuration

let lines = Campaign.Run.PlayChess.run config
let en = lines.GetEnumerator()
let step() =
    if en.MoveNext() then
        printfn "%s" en.Current
        true
    else
        printfn "DONE"
        false

let mutable keepgoing = true
while keepgoing do
    keepgoing <- step()
