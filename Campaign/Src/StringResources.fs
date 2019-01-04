module Campaign.StringResources

open FSharp.Configuration

type Strings = ResXProvider<"Strings.resx">

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
