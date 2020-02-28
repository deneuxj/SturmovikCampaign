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

    member this.AssignTo(vehicle : Mcu.HasEntity) =
        vehicle.Model <- this.Model
        vehicle.Script <- this.Script


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
        let truck = vehicles.["CarRUS"]
        { Script = truck.GetScript().Value
          Model = truck.GetModel().Value
        }

    member this.GermanCar =
        let truck = vehicles.["CarGER"]
        { Script = truck.GetScript().Value
          Model = truck.GetModel().Value
        }

    member this.RussianTruck =
        let truck = vehicles.["TruckRUS"]
        { Script = truck.GetScript().Value
          Model = truck.GetModel().Value
        }

    member this.GermanTruck =
        let truck = vehicles.["TruckGER"]
        { Script = truck.GetScript().Value
          Model = truck.GetModel().Value
        }

    member this.RussianAntiTankCanon =
        let canon = vehicles.["AntiTankRUS"]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.GermanAntiTankCanon =
        let canon = vehicles.["AntiTankGER"]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.RussianAntiAirMachineGun =
        let canon = vehicles.["MgAARUS"]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.RussianAntiAirCanon =
        let canon = vehicles.["AntiAirRUS"]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.GermanAntiAirMachineGun =
        let canon = vehicles.["MgAAGER"]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.GermanAntiAirCanon =
        let canon = vehicles.["AntiAirGER"]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.RussianFlak =
        let canon = vehicles.["FlakRUS"]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.GermanFlak =
        let canon = vehicles.["FlakGER"]
        { Script = canon.GetScript().Value
          Model = canon.GetModel().Value
        }

    member this.RussianHeavyTank =
        let tank = vehicles.["HeavyTankRUS"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanHeavyTank =
        let tank = vehicles.["HeavyTankGER"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianMediumTank =
        let tank = vehicles.["MediumTankRUS"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanMediumTank =
        let tank = vehicles.["MediumTankGER"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianMobileAA =
        let tank = vehicles.["MobileAntiAirRUS"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanMobileAA =
        let tank = vehicles.["MobileAntiAirGER"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianLightArmor =
        let tank = vehicles.["LightArmorRUS"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanLightArmor =
        let tank = vehicles.["LightArmorGER"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanMachineGun =
        let gun = vehicles.["MgGER"]
        { Script = gun.GetScript().Value
          Model = gun.GetModel().Value
        }

    member this.RussianMachineGun =
        let gun = vehicles.["MgRUS"]
        { Script = gun.GetScript().Value
          Model = gun.GetModel().Value
        }

    member this.GermanArtillery =
        let gun = vehicles.["ArtilleryGER"]
        { Script = gun.GetScript().Value
          Model = gun.GetModel().Value
        }

    member this.RussianArtillery =
        let gun = vehicles.["ArtilleryRUS"]
        { Script = gun.GetScript().Value
          Model = gun.GetModel().Value
        }

    member this.RussianStaticHeavyTank =
        let tank = statics.["StaticHeavyTankRUS"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanStaticHeavyTank =
        let tank = statics.["StaticHeavyTankGER"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianStaticMediumTank =
        let tank = statics.["StaticMediumTankRUS"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanStaticMediumTank =
        let tank = statics.["StaticMediumTankGER"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.RussianStaticLightArmor =
        let tank = statics.["StaticLightArmorRUS"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.GermanStaticLightArmor =
        let tank = statics.["StaticLightArmorGER"]
        { Script = tank.GetScript().Value
          Model = tank.GetModel().Value
        }

    member this.Tower =
        let tower = statics.["Tower"]
        { Script = tower.GetScript().Value
          Model = tower.GetModel().Value
        }

    member this.Fuel =
        let tanks = statics.["FuelTanks"]
        { Script = tanks.GetScript().Value
          Model = tanks.GetModel().Value
        }

    member this.TankPosition =
        let position = statics.["TankPosition"]
        { Script = position.GetScript().Value
          Model = position.GetModel().Value
        }

    member this.MachineGunPosition =
        let position = statics.["MachineGunPosition"]
        { Script = position.GetScript().Value
          Model = position.GetModel().Value
        }

    member this.ArtilleryPosition =
        let position = statics.["ArtilleryPosition"]
        { Script = position.GetScript().Value
          Model = position.GetModel().Value
        }

    member this.Nets =
        let position = statics.["Nets"]
        { Script = position.GetScript().Value
          Model = position.GetModel().Value
        }

    member this.RussianSearchLight =
        let light = vehicles.["SearchLightRUS"]
        { Script = light.GetScript().Value
          Model = light.GetModel().Value
        }

    member this.GermanSearchLight =
        let light = vehicles.["SearchLightGER"]
        { Script = light.GetScript().Value
          Model = light.GetModel().Value
        }

    member this.RussianTrain =
        let train = trains.["TrainRUS"]
        { Script = train.GetScript().Value
          Model = train.GetModel().Value
        }

    member this.GermanTrain =
        let train = trains.["TrainGER"]
        { Script = train.GetScript().Value
          Model = train.GetModel().Value
        }

    member this.MkRussianTrainMcu() =
        trains.["TrainRUS"].CreateMcu() :?> Mcu.HasEntity

    member this.MkGermanTrainMcu() =
        trains.["TrainGER"].CreateMcu() :?> Mcu.HasEntity

    member this.MkRussianTrainMgAAMcu() =
        trains.["TrainMgAARUS"].CreateMcu() :?> Mcu.HasEntity

    member this.MkGermanTrainMgAAMcu() =
        trains.["TrainMgAAGER"].CreateMcu() :?> Mcu.HasEntity

    member this.RussianFighter1 =
        let fighter = planes.["fighter1RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter1 =
        let fighter = planes.["fighter1GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter2 =
        let fighter = planes.["fighter2RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter2 =
        let fighter = planes.["fighter2GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter3 =
        let fighter = planes.["fighter3RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter3 =
        let fighter = planes.["fighter3GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter4 =
        let fighter = planes.["fighter4RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter4 =
        let fighter = planes.["fighter4GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter5 =
        let fighter = planes.["fighter5RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter5 =
        let fighter = planes.["fighter5GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter6 =
        let fighter = planes.["fighter6RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter6 =
        let fighter = planes.["fighter6GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter7 =
        let fighter = planes.["fighter7RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter7 =
        let fighter = planes.["fighter7GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter8 =
        let fighter = planes.["fighter8RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter8 =
        let fighter = planes.["fighter8GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter9 =
        let fighter = planes.["fighter9RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanFighter9 =
        let fighter = planes.["fighter9GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter10 =
        let fighter = planes.["fighter10RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianFighter11 =
        let fighter = planes.["fighter11RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianAttacker1 =
        let fighter = planes.["attacker1RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanAttacker1 =
        let fighter = planes.["attacker1GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianAttacker2 =
        let fighter = planes.["attacker2RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanAttacker2 =
        let fighter = planes.["attacker2GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianAttacker3 =
        let fighter = planes.["attacker3RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanAttacker3 =
        let fighter = planes.["attacker3GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanAttacker4 =
        let fighter = planes.["attacker4GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianBomber1 =
        let fighter = planes.["bomber1RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanBomber1 =
        let fighter = planes.["bomber1GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianBomber2 =
        let fighter = planes.["bomber2RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanBomber2 =
        let fighter = planes.["bomber2GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianBomber3 =
        let fighter = planes.["bomber3RUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanBomber3 =
        let fighter = planes.["bomber3GER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianTransport =
        let fighter = planes.["transportRUS"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.GermanTransport =
        let fighter = planes.["transportGER"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.RussianStaI16 =
        let block = statics.["staI16"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaI16Net =
        let block = statics.["staI16Net"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaMig3 =
        let block = statics.["staMig3"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaMig3Net =
        let block = statics.["staMig3Net"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaLagg3 =
        let block = statics.["staLagg3"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaLagg3Net =
        let block = statics.["staLagg3Net"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaLagg3W1 =
        let block = statics.["staLagg3W1"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaLagg3W2 =
        let block = statics.["staLagg3W2"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaYak1 =
        let block = statics.["staYak1"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaYak1Net=
        let block = statics.["staYak1Net"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaYak1Open =
        let block = statics.["staYak1Open"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109e7 =
        let block = statics.["staBf109e7"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109e7Net =
        let block = statics.["staBf109e7Net"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109e7Open =
        let block = statics.["staBf109e7Open"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109 =
        let block = statics.["staBf109"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109Net =
        let block = statics.["staBf109Net"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBf109Open =
        let block = statics.["staBf109Open"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaAttacker =
        let block = statics.["staAttackerRUS"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaAttacker =
        let block = statics.["staAttackerGER"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.RussianStaBomber =
        let block = statics.["staBomberRUS"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaBomber =
        let block = statics.["staBomberGER"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaTransport =
        let block = statics.["staTransportGER"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaJu87 =
        let block = statics.["staJu87GER"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaJu87Net =
        let block = statics.["staJu87NetGER"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanStaHe111h6 =
        let block = statics.["staHe111h6GER"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.AntiTankPosition =
        let block = statics.["AntiTankPosition"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.AntiAirPosition =
        let block = statics.["AntiAirPosition"]
        { Script = block.GetScript().Value
          Model = block.GetModel().Value
        }

    member this.GermanLandLight =
        let light = vehicles.["LandLightGER"]
        { Script = light.GetScript().Value
          Model = light.GetModel().Value
        }

    member this.RussianLandLight =
        let light = vehicles.["LandLightRUS"]
        { Script = light.GetScript().Value
          Model = light.GetModel().Value
        }

    member this.GermanRocketArtillery =
        let vehicle = vehicles.["RocketArtilleryGER"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RussianRocketArtillery =
        let vehicle = vehicles.["RocketArtilleryRUS"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.GermanPlayerTank =
        let vehicle = vehicles.["PlayerTankGer"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RussianPlayerTank =
        let vehicle = vehicles.["PlayerTankRus"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.GermanTorpedoBoat =
        let vehicle = ships.["TorpedoBoatGer"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RussianTorpedoBoat =
        let vehicle = ships.["TorpedoBoatRus"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RussianGunBoat =
        let vehicle = ships.["ArmoredBoatRus"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.CargoShip =
        let vehicle = ships.["CargoShip"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.RiverCargoShip =
        let vehicle = ships.["RiverCargoShip"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.LandShip =
        let vehicle = ships.["LandShipGer"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.Destroyer =
        let vehicle = ships.["DestroyerRus"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }
        
    member this.MkShip() =
        ships.["CargoShip"].CreateMcu() :?> Mcu.HasEntity

    member this.bf109g14 =
        let fighter = planes.["bf109g14"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.bf109k4 =
        let fighter = planes.["bf109k4"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.fw190a8 =
        let fighter = planes.["fw190a8"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.fw190d9 =
        let fighter = planes.["fw190d9"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.me262 =
        let fighter = planes.["me262a"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.p47d28 =
        let fighter = planes.["p47d28"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.p38j25 =
        let fighter = planes.["p38j25"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.p51d15 =
        let fighter = planes.["p51d15"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.spitfiremkixe =
        let fighter = planes.["spitfiremkixe"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.tempestmkvs2 =
        let fighter = planes.["tempestmkvs2"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.b25 =
        let fighter = planes.["b25draf"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.u2vs =
        let fighter = planes.["u2vs"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.fokkerd7 =
        let fighter = planes.["fokkerd7"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.fokkerd7f =
        let fighter = planes.["fokkerd7f"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }
    member this.fokkerdr1 =
        let fighter = planes.["fokkerdr1"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.pfalzd3a =
        let fighter = planes.["pfalzd3a"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.sopcamel =
        let fighter = planes.["sopcamel"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.sopdolphin =
        let fighter = planes.["sopdolphin"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.spad13 =
        let fighter = planes.["spad13"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.albatrosd5 =
        let fighter = planes.["albatrosd5"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.haberstadtcl2 =
        let fighter = planes.["haberstadtcl2"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.haberstadtcl2au =
        let fighter = planes.["haberstadtcl2au"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.bristolf2bf2 =
        let fighter = planes.["bristolf2bf2"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.bristolf2bf3 =
        let fighter = planes.["bristolf2bf3"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.se5a =
        let fighter = planes.["se5a"]
        { Script = fighter.GetScript().Value
          Model = fighter.GetModel().Value
        }

    member this.StaticRussianTruck =
        let vehicle = statics.["StaticTruckRUS"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

    member this.StaticGermanTruck =
        let vehicle = statics.["StaticTruckGER"]
        { Script = vehicle.GetScript().Value
          Model = vehicle.GetModel().Value
        }

let vehicles = VehicleDataTypeStore()