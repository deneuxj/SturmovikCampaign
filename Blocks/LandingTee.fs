﻿module SturmovikMission.Blocks.LandingTee

open System.Numerics
open VectorExtension
open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.McuUtil
open SturmovikMission.Blocks.Vehicles
open SturmovikMission.Blocks.BlocksMissionData

/// Landing canvas arranged to show the landing direction
type LandingTee = {
    All : McuUtil.IMcuGroup
}
with
    static member Create(store : NumericalIdentifiers.IdStore, pos : Vector2, ori : float32, country : Mcu.CountryValue) =
        // Instantiate
        let subst = Mcu.substId <| store.GetIdMapper()
        let group = blocksData.GetGroup("LandingTee").CreateMcuList()
        for mcu in group do
            subst mcu
        // Key nodes
        let leg = McuUtil.getVehicleByName group "Leg"
        let bar = McuUtil.getVehicleByName group "Tee"
        // Hide icons, or people might get confused and try to attack them
        leg.Name <- "NOICON"
        bar.Name <- "NOICON"
        // Position all nodes
        let refPos = Vector2.FromMcu leg.Pos
        let dv = pos - refPos
        for mcu in group do
            let rel = Vector2.FromMcu(mcu.Pos) - refPos
            (rel.Rotate(ori) + pos).AssignTo mcu.Pos
            mcu.Ori.Y <- (mcu.Ori.Y + float ori) % 360.0
        // Country
        leg.Country <- Some country
        bar.Country <- Some country
        // Return
        { All = McuUtil.groupFromList group
        }
