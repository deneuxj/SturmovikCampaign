module Campaign.WebHook

open System.Net
open System.Net.Http
open Newtonsoft.Json

open Campaign.BasicTypes
open Campaign.AfterActionReport
open Campaign.NewWorldState
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

let postMessage (queue : MailboxProcessor<unit -> int>, client) message =
    queue.Post(fun() -> toChat client message)

let postNumPlanes channel numFlights =
    let message =
        let be, plural =
            if numFlights > 1 then "are", "s" else "is", ""
        sprintf "There %s now %d plane%s in the air."
            be
            numFlights
            plural
    postMessage channel message

let onTookOff channel (flight : InFlight, pilot : Pilot, numFlights : int) =
    let message =
        sprintf "A plane took off%s."
            (match pilot.Coalition with
             | Some Axis -> " on the axis side"
             | Some Allies -> " on the allies side"
             | None -> "")
    postMessage channel message
    postNumPlanes channel numFlights

let onLanded channel (_, damage, flightDuration, numFlights) =
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
    postMessage channel (sprintf "A %s landed %s" planeState flightDuration)
    postNumPlanes channel numFlights

let onMissionStarted channel (missionTime : System.DateTime) =
    let message =
        sprintf "New mission started, in-game time is %s"
            (missionTime.ToString("HH:mm"))
    postMessage channel message

let onCampaignOver channel (victors : CoalitionId) =
    let be =
        match victors with
        | Axis -> "is"
        | Allies -> "are"
    let message =
        sprintf "Campaign is over, %s %s victorious"
            (victors.ToString())
            be
    postMessage channel message

let onMissionEnd channel (axisAAR : ReportData, alliesAAR : ReportData, battles : BattleSummary list) =
    let message =
        axisAAR.GetText(Axis)
        |> pseudoHtmlToMarkdown
    postMessage channel message
    let message =
        alliesAAR.GetText(Allies)
        |> pseudoHtmlToMarkdown
    postMessage channel message
    for battle in battles do
        let message =
            battle.GetText()
            |> String.concat "<br>"
            |> pseudoHtmlToMarkdown
        postMessage channel message

let createClient(webHookUri) =
    let client = new WebClient()
    let uri = System.Uri(webHookUri)
    client, uri
