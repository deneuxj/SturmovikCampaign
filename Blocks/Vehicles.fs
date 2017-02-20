// model/script strings of various known vehicles and planes
module SturmovikMission.Blocks.Vehicles

open SturmovikMission.DataProvider

type T = SturmovikMissionTypes.Provider<"../data/Blocks/Vehicles.mission", "../data/Blocks/Vehicles.mission">

type VehicleTypeData = {
    Script : string
    Model : string
}

let private data =
    T.GroupData(Parsing.Stream.FromFile "Vehicles.mission").ListOfVehicle
    |> List.map (fun vehicle -> vehicle.GetName().Value, vehicle)
    |> dict

let russianCar =
    let truck = data.[T.Vehicles.CarRUS]
    { Script = truck.GetScript().Value
      Model = truck.GetModel().Value
    }

let germanCar =
    let truck = data.[T.Vehicles.CarGER]
    { Script = truck.GetScript().Value
      Model = truck.GetModel().Value
    }

let russianTruck =
    let truck = data.[T.Vehicles.TruckRUS]
    { Script = truck.GetScript().Value
      Model = truck.GetModel().Value
    }

let germanTruck =
    let truck = data.[T.Vehicles.TruckGER]
    { Script = truck.GetScript().Value
      Model = truck.GetModel().Value
    }

let russianAntiTankCanon =
    let canon = data.[T.Vehicles.AntiTankRUS]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let germanAntiTankCanon =
    let canon = data.[T.Vehicles.AntiTankGER]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let russianAntiAirCanon =
    let canon = data.[T.Vehicles.AntiAirRUS]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let germanAntiAirCanon =
    let canon = data.[T.Vehicles.AntiAirGER]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let russianFlak =
    let canon = data.[T.Vehicles.FlakRUS]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }

let germanFlak =
    let canon = data.[T.Vehicles.FlakGER]
    { Script = canon.GetScript().Value
      Model = canon.GetModel().Value
    }
