// model/script strings of various known vehicles and planes
module SturmovikMission.Blocks.Vehicles

open SturmovikMission.DataProvider
open SturmovikMission.DataProvider.Parsing
open BlocksMissionData
open System.IO

type VehicleTypeData = {
    Script : string
    Model : string
}
with
    member this.ShortName =
        let filename =
            this.Model.Split([|'\\'|])
            |> Seq.last
        assert(filename.StartsWith "static_")
        assert(filename.EndsWith ".mgm")
        let withExtension = filename.["static_".Length..]
        let shortName = withExtension.[0..withExtension.Length-4-1]
        shortName

let private data =
    let path = Path.GetDirectoryName(System.Reflection.Assembly.GetCallingAssembly().Location)
    T.GroupData(Parsing.Stream.FromFile(Path.Combine(path, "Vehicles.mission")))

let private vehicles =
    data.ListOfVehicle
    |> List.map (fun vehicle -> vehicle.GetName().Value, vehicle)
    |> dict

let private planes =
    data.ListOfPlane
    |> List.map (fun vehicle -> vehicle.GetName().Value, vehicle)
    |> dict

let private trains =
    data.ListOfTrain
    |> List.map (fun vehicle -> vehicle.GetName().Value, vehicle)
    |> dict

let private statics =
    data.ListOfBlock
    |> List.map (fun block -> block.GetName().Value, block)
    |> dict

let russianCar =
    let truck = vehicles.[T.Vehicles.CarRUS]
    { Script = truck.GetScript().Value
      Model = truck.GetModel().Value
    }

let germanCar =
    let truck = vehicles.[T.Vehicles.CarGER]
    { Script = truck.GetScript().Value
      Model = truck.GetModel().Value
    }

let russianTruck =
    let truck = vehicles.[T.Vehicles.TruckRUS]
    { Script = truck.GetScript().Value
      Model = truck.GetModel().Value
    }

let germanTruck =
    let truck = vehicles.[T.Vehicles.TruckGER]
    { Script = truck.GetScript().Value
      Model = truck.GetModel().Value
    }

let russianAntiTankCanon =
    let canon = vehicles.[T.Vehicles.AntiTankRUS]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let germanAntiTankCanon =
    let canon = vehicles.[T.Vehicles.AntiTankGER]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let russianAntiAirCanon =
    let canon = vehicles.[T.Vehicles.AntiAirRUS]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let germanAntiAirCanon =
    let canon = vehicles.[T.Vehicles.AntiAirGER]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let russianFlak =
    let canon = vehicles.[T.Vehicles.FlakRUS]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let germanFlak =
    let canon = vehicles.[T.Vehicles.FlakGER]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let russianHeavyTank =
    let tank = vehicles.[T.Vehicles.HeavyTankRUS]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let germanHeavyTank =
    let tank = vehicles.[T.Vehicles.HeavyTankGER]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let russianMediumTank =
    let tank = vehicles.[T.Vehicles.MediumTankRUS]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let germanMediumTank =
    let tank = vehicles.[T.Vehicles.MediumTankGER]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let russianMobileAA =
    let tank = vehicles.[T.Vehicles.MobileAntiAirRUS]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let germanMobileAA =
    let tank = vehicles.[T.Vehicles.MobileAntiAirGER]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let russianLightArmor =
    let tank = vehicles.[T.Vehicles.LightArmorRUS]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let germanLightArmor =
    let tank = vehicles.[T.Vehicles.LightArmorGER]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let russianStaticHeavyTank =
    let tank = statics.[T.Vehicles.StaticHeavyTankRUS]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let germanStaticHeavyTank =
    let tank = statics.[T.Vehicles.StaticHeavyTankGER]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let russianStaticMediumTank =
    let tank = statics.[T.Vehicles.StaticMediumTankRUS]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let germanStaticMediumTank =
    let tank = statics.[T.Vehicles.StaticMediumTankGER]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let russianStaticLightArmor =
    let tank = statics.[T.Vehicles.StaticLightArmorRUS]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let germanStaticLightArmor =
    let tank = statics.[T.Vehicles.StaticLightArmorGER]
    { Script = tank.GetScript().Value
      Model = tank.GetModel().Value
    }

let tower =
    let tower = statics.[T.Vehicles.Tower]
    { Script = tower.GetScript().Value
      Model = tower.GetModel().Value
    }

let fuel =
    let tanks = statics.[T.Vehicles.FuelTanks]
    { Script = tanks.GetScript().Value
      Model = tanks.GetModel().Value
    }

let tankPosition =
    let position = statics.[T.Vehicles.TankPosition]
    { Script = position.GetScript().Value
      Model = position.GetModel().Value
    }

let russianSearchLight =
    let light = vehicles.[T.Vehicles.SearchLightRUS]
    { Script = light.GetScript().Value
      Model = light.GetModel().Value
    }

let germanSearchLight =
    let light = vehicles.[T.Vehicles.SearchLightGER]
    { Script = light.GetScript().Value
      Model = light.GetModel().Value
    }

let russianTrain =
    let train = trains.[T.Vehicles.TrainRUS]
    { Script = train.GetScript().Value
      Model = train.GetModel().Value
    }

let germanTrain =
    let train = trains.[T.Vehicles.TrainGER]
    { Script = train.GetScript().Value
      Model = train.GetModel().Value
    }

let mkRussianTrainMcu() =
    trains.[T.Vehicles.TrainRUS].CreateMcu() :?> Mcu.HasEntity

let mkGermanTrainMcu() =
    trains.[T.Vehicles.TrainGER].CreateMcu() :?> Mcu.HasEntity

let russianFighter1 =
    let fighter = planes.[T.Vehicles.fighter1RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanFighter1 =
    let fighter = planes.[T.Vehicles.fighter1GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianFighter2 =
    let fighter = planes.[T.Vehicles.fighter2RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanFighter2 =
    let fighter = planes.[T.Vehicles.fighter2GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianFighter3 =
    let fighter = planes.[T.Vehicles.fighter3RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanFighter3 =
    let fighter = planes.[T.Vehicles.fighter3GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianFighter4 =
    let fighter = planes.[T.Vehicles.fighter4RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanFighter4 =
    let fighter = planes.[T.Vehicles.fighter4GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianFighter5 =
    let fighter = planes.[T.Vehicles.fighter5RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanFighter5 =
    let fighter = planes.[T.Vehicles.fighter5GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianFighter6 =
    let fighter = planes.[T.Vehicles.fighter6RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanFighter6 =
    let fighter = planes.[T.Vehicles.fighter6GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianFighter7 =
    let fighter = planes.[T.Vehicles.fighter7RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanFighter7 =
    let fighter = planes.[T.Vehicles.fighter7GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanFighter8 =
    let fighter = planes.[T.Vehicles.fighter8GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianAttacker1 =
    let fighter = planes.[T.Vehicles.attacker1RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanAttacker1 =
    let fighter = planes.[T.Vehicles.attacker1GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianAttacker2 =
    let fighter = planes.[T.Vehicles.attacker2RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanAttacker2 =
    let fighter = planes.[T.Vehicles.attacker2GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianAttacker3 =
    let fighter = planes.[T.Vehicles.attacker3RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanAttacker3 =
    let fighter = planes.[T.Vehicles.attacker3GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianBomber1 =
    let fighter = planes.[T.Vehicles.bomber1RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanBomber1 =
    let fighter = planes.[T.Vehicles.bomber1GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianBomber2 =
    let fighter = planes.[T.Vehicles.bomber2RUS]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanBomber2 =
    let fighter = planes.[T.Vehicles.bomber2GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanBomber3 =
    let fighter = planes.[T.Vehicles.bomber3GER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let germanTransport =
    let fighter = planes.[T.Vehicles.transportGER]
    { Script = fighter.GetScript().Value
      Model = fighter.GetModel().Value
    }

let russianStaI16 =
    let block = statics.[T.Vehicles.staI16]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaI16Net =
    let block = statics.[T.Vehicles.staI16Net]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaMig3 =
    let block = statics.[T.Vehicles.staMig3]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaMig3Net =
    let block = statics.[T.Vehicles.staMig3Net]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaLagg3 =
    let block = statics.[T.Vehicles.staLagg3]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaLagg3Net =
    let block = statics.[T.Vehicles.staLagg3Net]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaLagg3W1 =
    let block = statics.[T.Vehicles.staLagg3W1]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaLagg3W2 =
    let block = statics.[T.Vehicles.staLagg3W2]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaYak1 =
    let block = statics.[T.Vehicles.staYak1]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaYak1Net=
    let block = statics.[T.Vehicles.staYak1Net]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaYak1Open =
    let block = statics.[T.Vehicles.staYak1Open]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaBf109e7 =
    let block = statics.[T.Vehicles.staBf109e7]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaBf109e7Net =
    let block = statics.[T.Vehicles.staBf109e7Net]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaBf109e7Open =
    let block = statics.[T.Vehicles.staBf109e7Open]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaBf109 =
    let block = statics.[T.Vehicles.staBf109]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaBf109Net =
    let block = statics.[T.Vehicles.staBf109Net]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaBf109Open =
    let block = statics.[T.Vehicles.staBf109Open]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaAttacker =
    let block = statics.[T.Vehicles.staAttackerRUS]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaAttacker =
    let block = statics.[T.Vehicles.staAttackerGER]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let russianStaBomber =
    let block = statics.[T.Vehicles.staBomberRUS]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaBomber =
    let block = statics.[T.Vehicles.staBomberGER]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaTransport =
    let block = statics.[T.Vehicles.staTransportGER]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaJu87 =
    let block = statics.[T.Vehicles.staJu87GER]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let germanStaHe111h6 =
    let block = statics.[T.Vehicles.staHe111h6GER]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let antiTankPosition =
    let block = statics.[T.Vehicles.AntiTankPosition]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }

let antiAirPosition =
    let block = statics.[T.Vehicles.AntiAirPosition]
    { Script = block.GetScript().Value
      Model = block.GetModel().Value
    }
