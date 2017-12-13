module SturmovikMission.Blocks.ParaDrop

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.VirtualConvoy.Types
open SturmovikMission.Blocks.BlocksMissionData
open SturmovikMission.Blocks.EventReporting
open SturmovikMission.Blocks.Util

let preciseParaDropPrefix = "PreciseParaDrop"
let wideParaDropPrefix = "WideParaDrop"

type ParaDrop = {
    Pos : Vector2
    Country : Mcu.CountryValue
    NotifyPreciselyDropped : EventReporting
    NotifyWidelyDropped : EventReporting
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, lcStore : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, country : Mcu.CountryValue, eventName : string) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let substlc = Mcu.substLCId <| lcStore.GetIdMapper()
        let group = blocksData.GetGroup("ParaDrop").CreateMcuList()
        for mcu in group do
            subst mcu
            substlc mcu
        // Get key nodes
        let precise = getTriggerByName group T.Blocks.PreciselyDropped
        let wide = getTriggerByName group T.Blocks.WidelyDropped
        let preciseCx = getComplexTriggerByName group T.Blocks.PreciseDrop
        let wideCx = getComplexTriggerByName group T.Blocks.WideDrop
        let msgPrecise = getTriggerByName group T.Blocks.SubtitlePrecise
        let msgWide = getTriggerByName group T.Blocks.SubtitleWide
        // Correct positions
        let ref = Vector2.FromMcu precise.Pos
        let dv = pos - ref
        for mcu in group do
            let rel = Vector2.FromMcu(mcu.Pos) - ref
            let pos2 = rel.Rotate(ori) + dv + ref
            pos2.AssignTo mcu.Pos
            let angle = mcu.Ori.Y + float ori
            mcu.Ori.Y <- angle % 360.0
        McuUtil.vecCopy preciseCx.Pos wideCx.Pos
        // Correct countries
        preciseCx.Countries <- [ country ]
        wideCx.Countries <- [ country ]
        for mcu in group do
            match mcu with
            | :? Mcu.HasEntity as vehicle -> vehicle.Country <- country
            | _ -> ()
        // Notification
        let notifyPreciseName = sprintf "%s-%s" preciseParaDropPrefix eventName
        let notifyPrecise = EventReporting.Create(store, country, pos, notifyPreciseName)
        let notifyWideName = sprintf "%s-%s" wideParaDropPrefix eventName
        let notifyWide = EventReporting.Create(store, country, pos, notifyWideName)
        Mcu.addTargetLink precise notifyPrecise.Trigger.Index
        Mcu.addTargetLink wide notifyWide.Trigger.Index
        // Result
        { Pos = pos
          Country = country
          NotifyPreciselyDropped = notifyPrecise
          NotifyWidelyDropped = notifyWide
          All =
            { new McuUtil.IMcuGroup with
                  member x.Content = group
                  member x.LcStrings = [
                        match msgPrecise.SubtitleLC with
                        | Some lc -> yield lc.LCText, "Paratrooper landed alive in LZ"
                        | None -> ()
                        match msgWide.SubtitleLC with
                        | Some lc -> yield lc.LCText, "Paratrooper landed alive near LZ"
                        | None -> ()
                    ]
                  member x.SubGroups = [ notifyPrecise.All; notifyWide.All ]
            }
        }

    static member TryGetPreciseDropEventName(name : string) =
        if name.StartsWith(preciseParaDropPrefix + "-") then
            try
                let att, rest = String.sscanf "-%c-%s" (name.Substring(preciseParaDropPrefix.Length))
                Some (att, rest)
            with
            | _ -> None
        else
            None

    static member TryGetWideDropEventName(name : string) =
        if name.StartsWith(wideParaDropPrefix + "-") then
            try
                let att, rest = String.sscanf "-%c-%s" (name.Substring(wideParaDropPrefix.Length))
                Some (att, rest)
            with
            | _ -> None
        else
            None
