open System
open System.Threading
open Suave
open Campaign.WebController
open Campaign.WebController.Routes

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let controller = Controller(Settings.Default)
    let routes = mkRoutes(controller, controller)
    let listening, server = startWebServerAsync conf routes
    Async.StartImmediate(server, cts.Token)
    printfn "Press a key to exit"
    Console.ReadKey(true) |> ignore
    cts.Cancel()
    0 // return an integer exit code
