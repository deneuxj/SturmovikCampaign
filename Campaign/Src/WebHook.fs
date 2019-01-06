// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

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

let private logger = NLog.LogManager.GetCurrentClassLogger()

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
            logger.Error(sprintf "Sending chat entry to web hook failed with '%s'" e.Message)
            None
    let responseHeaders =
        client.ResponseHeaders
    let responseHeaders =
        seq {
            for i in 0..responseHeaders.Count-1 do
                yield (responseHeaders.GetKey(i), responseHeaders.Get(i))
        }
        |> Map.ofSeq
    match responseHeaders.TryFind("retry-after") with
    | Some delay ->
        logger.Warn(sprintf "Discord rate-limiting us, retry-after %s ms" delay)
        for kvp in responseHeaders do
            logger.Debug(sprintf "Header from discord: %s: %s" kvp.Key kvp.Value)
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
                logger.Info(sprintf "Mailbox processor received message to send to Discord")
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
        |> Seq.sumBy(fun region -> region.ProductionCapacity(wg.GetRegion(region.RegionId), world.SubBlockSpecs, world.ProductionFactor))
    let countSupplies coalition =
        state.Regions
        |> Seq.filter (fun region -> region.Owner = Some coalition)
        |> Seq.sumBy(fun region -> region.Supplies)
    let countAfSupplies coalition =
        state.Airfields
        |> Seq.filter (fun af -> sg.GetRegion(wg.GetAirfield(af.AirfieldId).Region).Owner = Some coalition)
        |> Seq.sumBy (fun af -> af.Supplies)
    let countStorage coalition =
        state.Regions
        |> Seq.filter (fun region -> region.Owner = Some coalition)
        |> Seq.sumBy(fun region -> region.StorageCapacity(wg.GetRegion(region.RegionId), world.SubBlockSpecs))
    let countAfStorage coalition =
        state.Airfields
        |> Seq.filter (fun af -> sg.GetRegion(wg.GetAirfield(af.AirfieldId).Region).Owner = Some coalition)
        |> Seq.sumBy(fun af -> af.StorageCapacity(wg.GetAirfield(af.AirfieldId), world.SubBlockSpecs))
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
    sprintf "Axis region supplies: %5.0f units out of %5.0f storage available." (countSupplies Axis) (countStorage Axis) |> send
    sprintf "Axis airfield supplies: %5.0f units out of %5.0f storage available." (countAfSupplies Axis) (countAfStorage Axis) |> send
    sprintf "Axis planes:\n%s\n" (countPlanes Axis |> showPlanes) |> send
    sprintf "Axis tanks:\n%s\n" (countTanks Axis |> showTanks) |> send
    sprintf "Allies production: %5.0f units per hour." (countProduction Allies) |> send
    sprintf "Allies region supplies: %5.0f units out of %5.0f storage available." (countSupplies Allies) (countStorage Allies) |> send
    sprintf "Allies airfield supplies: %5.0f units out of %5.0f storage available." (countAfSupplies Allies) (countAfStorage Allies) |> send
    sprintf "Allies planes:\n%s\n" (countPlanes Allies |> showPlanes) |> send
    sprintf "Allies tanks:\n%s\n" (countTanks Allies |> showTanks) |> send

let postOrders channel (world : World, state : WorldState, orders: Map<CoalitionId, RegionId list>) =
    let columnMovements coalition =
        let destinations =
            orders.TryFind coalition
            |> Option.defaultValue []
        match destinations with
        | [] ->
            sprintf "Nothing is know about the planned column movements of the %s side.\n" (string coalition)
        | _ :: _ ->
            let destinations =
                destinations
                |> List.map string
                |> String.concat ", "
            sprintf "Spies report that the %s side is planning to move tanks to %s.\n"
                (string coalition)
                destinations
    let send = postMessage channel
    columnMovements Axis |> send
    columnMovements Allies |> send

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
    let sky = weather.CloudDescription
    let message =
        sprintf "Sky: %s" sky
    postMessage channel message

let createClient(webHookUri) =
    let client = new WebClient()
    let uri = System.Uri(webHookUri)
    client, uri
