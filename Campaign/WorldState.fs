namespace Campaign.WorldState

open Campaign.WorldDescription

type RegionState = {
    RegionId : RegionId
    Owner : CoalitionId option
    StorageHealth : float32 list
    ShellCount : float32
    NumHeavyTanks : int
    NumMediumTanks : int
    NumLightArmor : int
}

type DefenseAreaState = {
    DefenseAreaId : DefenseAreaId
    NumUnits : int
}

type AirfieldState = {
    AirfieldId : AirfieldId
    NumPlanes : Map<PlaneModel, int>
    StorageHealth : float32 list
    BombWeight : float32
    NumRockets : int
}