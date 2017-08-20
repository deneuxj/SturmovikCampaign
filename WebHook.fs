module Campaign.WebHook

open System.Net
open System.Net.Http
open Newtonsoft.Json

open Campaign.BasicTypes
open Campaign.Commenting
open Campaign.Configuration

type ChannelUpdate =
    { content : string
    }

let toChat (client : WebClient, hookUri : System.Uri) message =
    let json = JsonConvert.SerializeObject { content = message }
    client.Headers.Add(HttpRequestHeader.ContentType, "application/json");
    let response =
        try
            client.UploadString(hookUri, json)
            |> Some
        with
        | e ->
            eprintfn "Sending chat entry to web hook failed with '%s'" e.Message
            None
    let responseHeaders =
        client.ResponseHeaders
    let retryAfter =
        seq {
            for i in 0..responseHeaders.Count-1 do
                yield (responseHeaders.GetKey(i), responseHeaders.Get(i))
        }
        |> Seq.tryFind(fun (k, v) -> k = "Retry-After")
    match retryAfter with
    | Some(_, delay) ->
        match System.Int32.TryParse(delay) with
        | true, delay -> delay
        | false, _ -> 0
    | None ->
        0

let startQueue() =
    MailboxProcessor.Start(fun msg ->
        let rec loop() =
            async {
                let! msg = msg.Receive()
                let delay = msg()
                do! Async.Sleep(max delay 1000)
                return! loop()
            }
        loop()
    )

let onTookOff (queue : MailboxProcessor<unit -> int>, client) (flight : InFlight, pilot : Pilot, numFlights : int) =
    let message =
        let plural =
            if numFlights > 1 then "s" else ""
        sprintf "A plane took off%s. There are now %d plane%s in the air."
            (match pilot.Coalition with
             | Some Axis -> " on the axis side"
             | Some Allies -> " on the allies side"
             | None -> "")
            numFlights
            plural
    queue.Post(fun () -> toChat client message)

let onLanded (queue : MailboxProcessor<unit -> int>, client) (_, damage, flightDuration) =
    let planeState =
        if damage = 0.0f then
            "plane in pristine condition"
        elif damage < 0.1f then
            "plane with scratches"
        elif damage < 0.2f then
            "damaged plane"
        else
            "wreck"
    let flightDuration =
        match flightDuration with
        | None -> ""
        | Some (duration : System.TimeSpan) ->
            let duration = duration.TotalMinutes
            if duration <= 2.0 then
                "shortly after take off"
            elif duration <= 10.0 then
                "after flying for a few minutes"
            elif duration <= 20.0 then
                "after a short sortie"
            elif duration <= 45.0 then
                "after a sortie"
            else
                "after a long flight"
    queue.Post(fun() -> toChat client (sprintf "A %s landed %s" planeState flightDuration))

let onMissionStarted (queue : MailboxProcessor<unit -> int>, client) (missionTime : System.DateTime) =
    let message =
        sprintf "New mission started, in-game time is %s"
            (missionTime.ToString("HH:mm"))
    queue.Post(fun() -> toChat client message)

let onCampaignOver (queue : MailboxProcessor<unit -> int>, client) (victors : CoalitionId) =
    let be =
        match victors with
        | Axis -> "is"
        | Allies -> "are"
    let message =
        sprintf "Campaign is over, %s %s victorious"
            (victors.ToString())
            be
    queue.Post(fun() -> toChat client message)

let createClient(webHookUri) =
    let client = new WebClient()
    let uri = System.Uri(webHookUri)
    client, uri
