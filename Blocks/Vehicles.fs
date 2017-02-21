// model/script strings of various known vehicles and planes
module SturmovikMission.Blocks.Vehicles

open SturmovikMission.DataProvider

type T = SturmovikMissionTypes.Provider<"../data/Blocks/Vehicles.mission", "../data/Blocks/Vehicles.mission">

type VehicleTypeData = {
    Script : string
    Model : string
}

let private data =
    T.GroupData(Parsing.Stream.FromFile "Vehicles.mission")

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
