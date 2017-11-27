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

type VehicleDataTypeStore() =
    let data =
        let path = Path.GetDirectoryName(System.Reflection.Assembly.GetCallingAssembly().Location)
        T.GroupData(Parsing.Stream.FromFile(Path.Combine(path, "Vehicles.mission")))

    let vehicles =
        data.ListOfVehicle
        |> List.map (fun vehicle -> vehicle.GetName().Value, vehicle)
        |> dict

    let planes =
        data.ListOfPlane
        |> List.map (fun vehicle -> vehicle.GetName().Value, vehicle)
        |> dict

    let trains =
        data.ListOfTrain
        |> List.map (fun vehicle -> vehicle.GetName().Value, vehicle)
        |> dict

    let ships =
        data.ListOfShip
        |> List.map (fun vehicle -> vehicle.GetName().Value, vehicle)
        |> dict

    let statics =
        data.ListOfBlock
        |> List.map (fun block -> block.GetName().Value, block)
        |> dict

    member this.RussianCar =
        let truck = vehicles.[T.Vehicles.CarRUS]
        { Script = truck.GetScript().Value
          Model = truck.GetModel().Value
        }

    member this.GermanCar =
        let truck = vehicles.[T.Vehicles.CarGER]
        { Script = truck.GetScript().Value
          Model = truck.GetModel().Value
        }

    member this.RussianTruck =
        let truck = vehicles.[T.Vehicles.TruckRUS]
        { Script = truck.GetScript().Value
          Model = truck.GetModel().Value
        }

    member this.GermanTruck =
        let truck = vehicles.[T.Vehicles.TruckGER]
        { Script = truck.GetScript().Value
          Model = truck.GetModel().Value
        }

    member this.RussianAntiTankCanon =
        let canon = vehicles.[T.Vehicles.AntiTankRUS]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.GermanAntiTankCanon =
        let canon = vehicles.[T.Vehicles.AntiTankGER]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.RussianAntiAirMachineGun =
        let canon = vehicles.[T.Vehicles.MgAARUS]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.RussianAntiAirCanon =
        let canon = vehicles.[T.Vehicles.AntiAirRUS]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.GermanAntiAirMachineGun =
        let canon = vehicles.[T.Vehicles.MgAAGER]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.GermanAntiAirCanon =
        let canon = vehicles.[T.Vehicles.AntiAirGER]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.RussianFlak =
        let canon = vehicles.[T.Vehicles.FlakRUS]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.GermanFlak =
        let canon = vehicles.[T.Vehicles.FlakGER]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.RussianHeavyTank =
        let tank = vehicles.[T.Vehicles.HeavyTankRUS]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanHeavyTank =
        let tank = vehicles.[T.Vehicles.HeavyTankGER]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianMediumTank =
        let tank = vehicles.[T.Vehicles.MediumTankRUS]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanMediumTank =
        let tank = vehicles.[T.Vehicles.MediumTankGER]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianMobileAA =
        let tank = vehicles.[T.Vehicles.MobileAntiAirRUS]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanMobileAA =
        let tank = vehicles.[T.Vehicles.MobileAntiAirGER]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianLightArmor =
        let tank = vehicles.[T.Vehicles.LightArmorRUS]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanLightArmor =
        let tank = vehicles.[T.Vehicles.LightArmorGER]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianStaticHeavyTank =
        let tank = statics.[T.Vehicles.StaticHeavyTankRUS]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanStaticHeavyTank =
        let tank = statics.[T.Vehicles.StaticHeavyTankGER]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianStaticMediumTank =
        let tank = statics.[T.Vehicles.StaticMediumTankRUS]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanStaticMediumTank =
        let tank = statics.[T.Vehicles.StaticMediumTankGER]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianStaticLightArmor =
        let tank = statics.[T.Vehicles.StaticLightArmorRUS]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanStaticLightArmor =
        let tank = statics.[T.Vehicles.StaticLightArmorGER]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.Tower =
        let tower = statics.[T.Vehicles.Tower]
        { Script = tower.GetScript().Value
          Model = tower.GetModel().Value
        }

    member this.Fuel =
        let tanks = statics.[T.Vehicles.FuelTanks]
        { Script = tanks.GetScript().Value
          Model = tanks.GetModel().Value
        }

    member this.TankPosition =
        let position = statics.[T.Vehicles.TankPosition]
        { Script = position.GetScript().Value
          Model = position.GetModel().Value
        }

    member this.RussianSearchLight =
        let light = vehicles.[T.Vehicles.SearchLightRUS]
        { Script = light.GetScript().Value
          Model = light.GetModel().Value
        }

    member this.GermanSearchLight =
        let light = vehicles.[T.Vehicles.SearchLightGER]
        { Script = light.GetScript().Value
          Model = light.GetModel().Value
        }

    member this.RussianTrain =
        let train = trains.[T.Vehicles.TrainRUS]
        { Script = train.GetScript().Value
          Model = train.GetModel().Value
        }

    member this.GermanTrain =
        let train = trains.[T.Vehicles.TrainGER]
        { Script = train.GetScript().Value
          Model = train.GetModel().Value
        }

    member this.MkRussianTrainMcu() =
        trains.[T.Vehicles.TrainRUS].CreateMcu() :?> Mcu.HasEntity

    member this.MkGermanTrainMcu() =
        trains.[T.Vehicles.TrainGER].CreateMcu() :?> Mcu.HasEntity

    member this.RussianFighter1 =
        let fighter = planes.[T.Vehicles.fighter1RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter1 =
        let fighter = planes.[T.Vehicles.fighter1GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter2 =
        let fighter = planes.[T.Vehicles.fighter2RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter2 =
        let fighter = planes.[T.Vehicles.fighter2GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter3 =
        let fighter = planes.[T.Vehicles.fighter3RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter3 =
        let fighter = planes.[T.Vehicles.fighter3GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter4 =
        let fighter = planes.[T.Vehicles.fighter4RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter4 =
        let fighter = planes.[T.Vehicles.fighter4GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter5 =
        let fighter = planes.[T.Vehicles.fighter5RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter5 =
        let fighter = planes.[T.Vehicles.fighter5GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter6 =
        let fighter = planes.[T.Vehicles.fighter6RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter6 =
        let fighter = planes.[T.Vehicles.fighter6GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter7 =
        let fighter = planes.[T.Vehicles.fighter7RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter7 =
        let fighter = planes.[T.Vehicles.fighter7GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter8 =
        let fighter = planes.[T.Vehicles.fighter8RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter8 =
        let fighter = planes.[T.Vehicles.fighter8GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianAttacker1 =
        let fighter = planes.[T.Vehicles.attacker1RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanAttacker1 =
        let fighter = planes.[T.Vehicles.attacker1GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianAttacker2 =
        let fighter = planes.[T.Vehicles.attacker2RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanAttacker2 =
        let fighter = planes.[T.Vehicles.attacker2GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianAttacker3 =
        let fighter = planes.[T.Vehicles.attacker3RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanAttacker3 =
        let fighter = planes.[T.Vehicles.attacker3GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanAttacker4 =
        let fighter = planes.[T.Vehicles.attacker4GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianBomber1 =
        let fighter = planes.[T.Vehicles.bomber1RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanBomber1 =
        let fighter = planes.[T.Vehicles.bomber1GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianBomber2 =
        let fighter = planes.[T.Vehicles.bomber2RUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanBomber2 =
        let fighter = planes.[T.Vehicles.bomber2GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanBomber3 =
        let fighter = planes.[T.Vehicles.bomber3GER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianTransport =
        let fighter = planes.[T.Vehicles.transportRUS]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanTransport =
        let fighter = planes.[T.Vehicles.transportGER]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianStaI16 =
        let block = statics.[T.Vehicles.staI16]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaI16Net =
        let block = statics.[T.Vehicles.staI16Net]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaMig3 =
        let block = statics.[T.Vehicles.staMig3]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaMig3Net =
        let block = statics.[T.Vehicles.staMig3Net]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaLagg3 =
        let block = statics.[T.Vehicles.staLagg3]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaLagg3Net =
        let block = statics.[T.Vehicles.staLagg3Net]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaLagg3W1 =
        let block = statics.[T.Vehicles.staLagg3W1]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaLagg3W2 =
        let block = statics.[T.Vehicles.staLagg3W2]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaYak1 =
        let block = statics.[T.Vehicles.staYak1]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaYak1Net=
        let block = statics.[T.Vehicles.staYak1Net]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaYak1Open =
        let block = statics.[T.Vehicles.staYak1Open]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109e7 =
        let block = statics.[T.Vehicles.staBf109e7]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109e7Net =
        let block = statics.[T.Vehicles.staBf109e7Net]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109e7Open =
        let block = statics.[T.Vehicles.staBf109e7Open]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109 =
        let block = statics.[T.Vehicles.staBf109]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109Net =
        let block = statics.[T.Vehicles.staBf109Net]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109Open =
        let block = statics.[T.Vehicles.staBf109Open]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaAttacker =
        let block = statics.[T.Vehicles.staAttackerRUS]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaAttacker =
        let block = statics.[T.Vehicles.staAttackerGER]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaBomber =
        let block = statics.[T.Vehicles.staBomberRUS]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBomber =
        let block = statics.[T.Vehicles.staBomberGER]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaTransport =
        let block = statics.[T.Vehicles.staTransportGER]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaJu87 =
        let block = statics.[T.Vehicles.staJu87GER]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaJu87Net =
        let block = statics.[T.Vehicles.staJu87NetGER]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaHe111h6 =
        let block = statics.[T.Vehicles.staHe111h6GER]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.AntiTankPosition =
        let block = statics.[T.Vehicles.AntiTankPosition]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.AntiAirPosition =
        let block = statics.[T.Vehicles.AntiAirPosition]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanLandLight =
        let light = vehicles.[T.Vehicles.LandLightGER]
        { Script = light.GetScript().Value
          Model = light.GetModel().Value
        }

    member this.RussianLandLight =
        let light = vehicles.[T.Vehicles.LandLightRUS]
        { Script = light.GetScript().Value
          Model = light.GetModel().Value
        }

    member this.GermanRocketArtillery =
        let vehicle = vehicles.[T.Vehicles.RocketArtilleryGER]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RussianRocketArtillery =
        let vehicle = vehicles.[T.Vehicles.RocketArtilleryRUS]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.GermanPlayerTank =
        let vehicle = vehicles.[T.Vehicles.PlayerTankGer]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RussianPlayerTank =
        let vehicle = vehicles.[T.Vehicles.PlayerTankRus]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.GermanTorpedoBoat =
        let vehicle = ships.[T.Vehicles.TorpedoBoatGer]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RussianTorpedoBoat =
        let vehicle = ships.[T.Vehicles.TorpedoBoatRus]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RussianGunBoat =
        let vehicle = ships.[T.Vehicles.ArmoredBoatRus]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.CargoShip =
        let vehicle = ships.[T.Vehicles.CargoShip]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RiverCargoShip =
        let vehicle = ships.[T.Vehicles.RiverCargoShip]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.LandShip =
        let vehicle = ships.[T.Vehicles.LandShipGer]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.Destroyer =
        let vehicle = ships.[T.Vehicles.DestroyerRus]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }
        
    member this.MkShip() =
        ships.[T.Vehicles.CargoShip].CreateMcu() :?> Mcu.HasEntity

let vehicles = VehicleDataTypeStore()