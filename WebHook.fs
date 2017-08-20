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
    client.UploadString(hookUri, json)
    |> ignore

let onTookOff client (flight : InFlight, pilot : Pilot, numFlights : int) =
    let message =
        sprintf "A plane took off%s. There are now %d planes in the air."
            (match pilot.Coalition with
             | Some Axis -> " on the axis side"
             | Some Allies -> " on the allies side"
             | None -> "")
            numFlights
    toChat client message

let onLanded client (_, damage, flightDuration) =
    let planeState =
        if damage = 0.0f then
            "a plane in pristine condition"
        elif damage < 0.1f then
            "a plane with scratches"
        elif damage < 0.2f then
            "a damaged plane"
        else
            "a wreck"
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
    toChat client (sprintf "A %s landed %s" planeState flightDuration)

let createClient(webHookUri) =
    let client = new WebClient()
    let uri = System.Uri(webHookUri)
    client, uri
