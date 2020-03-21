open System
open System.Threading
open Suave
open Campaign.WebController
open Campaign.WebController.Routes
open Newtonsoft.Json

[<EntryPoint>]
let main argv =
    let cts = new CancellationTokenSource()
    let conf = { defaultConfig with cancellationToken = cts.Token }
    let serializer =
        { new ISerializer with
            member this.SerializeAsync(task) =
                let x = Async.RunSynchronously task
                JsonConvert.SerializeObject(x)
        }
    let controller = Controller(Settings.Default)
    let routes = mkRoutes(serializer, controller, controller)
    let listening, server = startWebServerAsync conf routes
    Async.StartImmediate(server, cts.Token)
    printfn "Press a key to exit"
    Console.ReadKey(true) |> ignore
    cts.Cancel()
    0 // return an integer exit code
