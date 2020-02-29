module Campaign.StringResources

open System.Resources
open System.Reflection

type StringsAccess() =
    let rm = new ResourceManager("Strings", Assembly.GetExecutingAssembly());

    member this.Welcome = rm.GetString("Welcome")
    member this.FreshSpawns = rm.GetString("FreshSpawns")
    member this.ReservedPlanes = rm.GetString("ReservedPlanes")
    member this.TookLastPlane = rm.GetString("TookLastPlane")
    member this.EnteredPlane = rm.GetString("EnteredPlane")
    member this.PlaneAvailableAgain = rm.GetString("PlaneAvailableAgain")
    member this.NoSelfGift = rm.GetString("NoSelfGift")
    member this.GiftToPlayer = rm.GetString("GiftToPlayer")
    member this.GiftFromPlayer = rm.GetString("GiftFromPlayer")
    member this.GiftToPublic = rm.GetString("GiftToPublic")
    member this.UnauthorizedTakeOff = rm.GetString("UnauthorizedTakeOff")
    member this.BadSupplyMissionStart = rm.GetString("BadSupplyMissionStart")
    member this.SupplyMissionAdvice = rm.GetString("SupplyMissionAdvice")
    member this.FreshSpawnsExhausted = rm.GetString("FreshSpawnsExhausted")
    member this.FreshSpawnsExhaustedAlt = rm.GetString("FreshSpawnsExhaustedAlt")
    member this.TakeOffDenied = rm.GetString("TakeOffDenied")
    member this.TakeOffKickWarning = rm.GetString("TakeOffKickWarning")
    member this.TakeOffCleared = rm.GetString("TakeOffCleared")
    member this.TakeOffClearedAlt = rm.GetString("TakeOffClearedAlt")
    member this.NotAReservedPlane = rm.GetString("NotAReservedPlane")
    member this.RearSpawnAdvice = rm.GetString("RearSpawnAdvice")
    member this.ScarceTakeOff = rm.GetString("ScarceTakeOff")
    member this.TakeOff = rm.GetString("TakeOff")
    member this.CargoDelivered = rm.GetString("CargoDelivered")
    member this.EnemyLanded = rm.GetString("EnemyLanded")
    member this.LandingViolation = rm.GetString("LandingViolation")
    member this.BackAt = rm.GetString("BackAt")
    member this.CrashedNear = rm.GetString("CrashedNear")
    member this.LandedInTheRough = rm.GetString("LandedInTheRough")
    member this.Crashed = rm.GetString("Crashed")
    member this.KickNoBan = rm.GetString("KickNoBan")
    member this.Hours = rm.GetString("Hours")
    member this.Minutes = rm.GetString("Minutes")
    member this.Seconds = rm.GetString("Seconds")
    member this.TimeLeft = rm.GetString("TimeLeft")
    member this.PlaneReservedByOther = rm.GetString("PlaneReservedByOther")
    member this.InsufficientSupplies = rm.GetString("InsufficientSupplies")

let Strings = StringsAccess()

let welcome =
    sprintf (Printf.StringFormat<string -> string> Strings.Welcome)

let freshSpawns =
    sprintf (Printf.StringFormat<float32 -> float32 -> float32 -> float32 -> float32 -> string> Strings.FreshSpawns)

let reservedPlanes =
    sprintf (Printf.StringFormat<string -> string -> string> Strings.ReservedPlanes)

let tookLastPlane =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.TookLastPlane)

let enteredPlane =
    sprintf (Printf.StringFormat<string -> string -> string -> int -> string> Strings.EnteredPlane)

let planeAvailableAgain =
    sprintf (Printf.StringFormat<string -> string -> string> Strings.PlaneAvailableAgain)

let noSelfGift =
    Strings.NoSelfGift

let giftToPlayer =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.GiftToPlayer)

let giftFromPlayer =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.GiftFromPlayer)

let giftToPublic =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.GiftToPublic)

let unauthorizedTakeOff =
    Strings.UnauthorizedTakeOff

let badSupplyMissionStart =
    Strings.BadSupplyMissionStart

let supplyMissionAdvice =
    Strings.SupplyMissionAdvice

let freshSpawnsExhausted =
    Strings.FreshSpawnsExhausted

let freshSpawnsExhaustedAlt =
    sprintf (Printf.StringFormat<string -> string> Strings.FreshSpawnsExhaustedAlt)

let planeReservedByOther =
    Strings.PlaneReservedByOther

let insufficientSupplies =
    Strings.InsufficientSupplies

let takeOffDenied =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.TakeOffDenied)

let takeOffKickWarning =
    Strings.TakeOffKickWarning

let takeOffCleared =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.TakeOffCleared)

let takeOffClearedAlt =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.TakeOffClearedAlt)

let notAReservedPlane =
    Strings.NotAReservedPlane

let rearSpawnAdvice =
    Strings.RearSpawnAdvice

let scarceTakeOff =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.ScarceTakeOff)

let takeOff =
    sprintf (Printf.StringFormat<string -> string -> string -> string> Strings.TakeOff)

let cargoDelivered =
    sprintf (Printf.StringFormat<string -> float32 -> string -> string> Strings.CargoDelivered)

let enemyLanded =
    sprintf (Printf.StringFormat<string -> string -> string> Strings.EnemyLanded)

let landingViolation =
    Strings.LandingViolation

let backAt =
    sprintf (Printf.StringFormat<string -> string -> string> Strings.BackAt)

let crashedNear =
    sprintf (Printf.StringFormat<string -> string -> string> Strings.CrashedNear)

let landedInTheRough =
    sprintf (Printf.StringFormat<string -> string> Strings.LandedInTheRough)

let crashed =
    sprintf (Printf.StringFormat<string -> string> Strings.Crashed)

let kickNoBan =
    sprintf (Printf.StringFormat<string -> string> Strings.KickNoBan)

let hours =
    sprintf (Printf.StringFormat<int -> string> Strings.Hours)

let minutes =
    sprintf (Printf.StringFormat<int -> string> Strings.Minutes)

let seconds =
    sprintf (Printf.StringFormat<int -> string> Strings.Seconds)

let timeLeft =
    sprintf (Printf.StringFormat<string -> string> Strings.TimeLeft)
