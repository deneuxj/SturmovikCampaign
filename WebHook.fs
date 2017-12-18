module Campaign.WebHook

open System.Net
open System.Net.Http
open Newtonsoft.Json

open Campaign.BasicTypes
open Campaign.AfterActionReport
open Campaign.NewWorldState
open Campaign.Commenting
open Campaign.Configuration
open Campaign.WorldDescription
open Campaign.WorldState
open Campaign.PlaneModel

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

let onMissionStarted channel (missionTime : System.DateTime) =
    let message =
        sprintf "New mission started, in-game time is %s."
            (missionTime.ToString("HH:mm"))
    postMessage channel message

let postWorldState channel (world : World, state : WorldState) =
    let wg = world.FastAccess
    let sg = state.FastAccess
    let countProduction coalition =
        state.Regions
        |> Seq.filter (fun region -> region.Owner = Some coalition)
        |> Seq.sumBy(fun region -> region.ProductionCapacity(wg.GetRegion(region.RegionId), world.ProductionFactor))
    let countPlanes coalition =
        state.Airfields
        |> Seq.filter (fun af -> sg.GetRegion(wg.GetAirfield(af.AirfieldId).Region).Owner = Some coalition)
        |> Seq.map (fun af -> af.NumPlanes |> Map.map (fun _ qty -> int qty))
        |> Seq.fold Util.addMaps Map.empty
        |> Map.filter (fun _ num -> num > 0)
    let showPlanes planes =
        planes
        |> Map.map (fun (plane : PlaneModel) num -> sprintf "%s: %d" plane.PlaneName num)
        |> Map.toSeq
        |> Seq.map snd
        |> String.concat "\n"
    let countTanks coalition =
        state.Regions
        |> Seq.map (fun region ->
            if region.Owner = Some coalition then
                region.NumVehicles
            elif region.Owner = Some coalition.Other then
                region.NumInvadingVehicles
            else
                Map.empty
        )
        |> Seq.fold Util.addMaps Map.empty
        |> Map.filter (fun _ num -> num > 0)
    let showTanks tanks =
        tanks
        |> Map.map (fun (tank : GroundAttackVehicle) num -> sprintf "%s: %d" tank.Description num)
        |> Map.toSeq
        |> Seq.map snd
        |> String.concat "\n"
    let send = postMessage channel
    sprintf "New mission started, in-game time is %s." (state.Date.ToString("HH:mm")) |> send
    sprintf "Axis production: %5.0f units per hour." (countProduction Axis) |> send
    sprintf "Axis planes:\n%s\n" (countPlanes Axis |> showPlanes) |> send
    sprintf "Axis tanks:\n%s\n" (countTanks Axis |> showTanks) |> send
    sprintf "Allies production: %5.0f units per hour." (countProduction Allies) |> send
    sprintf "Allies planes:\n%s\n" (countPlanes Allies |> showPlanes) |> send
    sprintf "Allies tanks:\n%s\n" (countTanks Allies |> showTanks) |> send

let onCampaignOver channel (victors : CoalitionId) =
    let be =
        match victors with
        | Axis -> "is"
        | Allies -> "are"
    let message =
        sprintf "Campaign is over, %s %s victorious."
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

let postWeatherReport channel (weather : Weather.WeatherState) =
    let sky =
        if weather.CloudDensity < 0.1 then
            "clear"
        elif weather.CloudDensity < 0.3 then
            "light clouds"
        elif weather.CloudDensity < 0.6 then
            "medium cloud cover"
        elif weather.CloudDensity < 0.95 then
            "heavy clouds"
        else
            "overcast"
    let message =
        sprintf "Sky: %s" sky
    postMessage channel message

let createClient(webHookUri) =
    let client = new WebClient()
    let uri = System.Uri(webHookUri)
    client, uri
