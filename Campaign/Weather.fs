module Campaign.Weather

open SturmovikMission.Blocks.BlocksMissionData

type WindState =  {
    Speed : float
    Direction : float
}

type WeatherState = {
    CloudDensity : float
    CloudHeight : float
    CloudThickness : float
    Precipitation : float
    Wind : WindState
    Turbulence : float
    Temperature : float
    Pressure : float
}

let wave mag period pow x =
    let y = sin (2.0 * System.Math.PI * x / period)
    let y =
        if y > 0.0 then
            System.Math.Pow(y, pow)
        else
            -System.Math.Pow(-y, pow)
    mag * y

let noisedWave (random : System.Random) mag period pow hnoise vnoise x =
    wave mag period pow (x + hnoise * (random.NextDouble() - 0.5)) + vnoise * (random.NextDouble() - 0.5)

let typicalPeriod = System.TimeSpan(7, 0, 0, 0).TotalSeconds
 
let getPressure random (t : System.DateTime) =
    let t0 = System.DateTime(1941, 1, 1)
    let hnoise = System.TimeSpan(1, 30, 0).TotalSeconds
    let vnoise = 2.0
    let avgPressure = 760.0
    let magPressure = 25.0
    let dt = (t - t0).TotalSeconds
    avgPressure + noisedWave random magPressure typicalPeriod 1.0 hnoise vnoise dt

let getHumidity random (t : System.DateTime) =
    let t0 = System.DateTime(1941, 1, 1)
    let hnoise = System.TimeSpan(0, 30, 0).TotalSeconds
    let vnoise = 0.1
    let avgHumidity = 0.5
    let magHumidity = 0.5
    let dt = (t - t0).TotalSeconds
    avgHumidity + noisedWave random magHumidity (typicalPeriod / 2.0) 1.0 hnoise vnoise dt
    |> max 0.0
    |> min 1.0

let getTemperature random (t : System.DateTime) =
    let t0 = System.DateTime(1941, 4, 22)
    let year = System.TimeSpan(365, 0, 0, 0).TotalSeconds
    let hnoise = System.TimeSpan(15, 0, 0, 0).TotalSeconds
    let vnoise = 5.0
    let avgTemperature = 5.0
    let magTemperature = 30.0
    let dt = (t - t0).TotalSeconds
    let baseTemp = avgTemperature + noisedWave random magTemperature year 2.0 hnoise vnoise dt
    let hourOfDayOffset =
        let hm = 60.0 * (float t.Hour) + (float t.Minute)
        let sunrise = 7.0 * 60.0
        let sunset = 19.0 * 60.0
        let midday = 0.5 * (sunrise + sunset)
        if hm < sunrise then
            -5.0
        elif hm < sunset then
            -5.0 + 10.0 * (1.0 - abs(hm - midday) / (0.5 * (sunset - sunrise)))
        else
            -5.0
    baseTemp + hourOfDayOffset

let getWindDirection random (t : System.DateTime) =
    let t0 = System.DateTime(1941, 1, 1)
    let hnoise = System.TimeSpan(0, 30, 0).TotalSeconds
    let vnoise = 0.0
    let avgDirection = 180.0
    let magDirection = 180.0
    let dt = (t - t0).TotalSeconds
    avgDirection + noisedWave random magDirection (typicalPeriod / 2.0) 1.0 hnoise vnoise  dt

let getWeather random (t : System.DateTime) =
    let pressure = getPressure random t
    let pressureDailyVariation =
        let day = System.TimeSpan(24, 0, 0, 0)
        let p = getPressure random (t - day)
        (pressure - p) / day.TotalSeconds
    let pressureHourlyVariation =
        let hour = System.TimeSpan(1, 0, 0, 0)
        let p = getPressure random (t - hour)
        (pressure - p) / hour.TotalSeconds
    let humidity =
        let h = getHumidity random t
        let dryPressure = 780.0
        let wetPressure = 760.0
        let k =
            if pressure < wetPressure then
                1.0
            elif pressure < dryPressure then
                1.0 - (pressure - wetPressure) / (dryPressure - wetPressure)
            else
                0.0
        k * h
    let windDirection =
        getWindDirection random t
    let windSpeed, turbulence =
        let hurricanePressureVariation =
            25.0 / (24.0 * 3600.0)
        let hurricaneWind = 80.0
        let speed = hurricaneWind * pressureDailyVariation / hurricanePressureVariation
        let turb = 0.01 * hurricaneWind * pressureHourlyVariation / hurricanePressureVariation
        speed, abs(turb)
    let windDirection, windSpeed =
        if windSpeed < 0.0 then
            windDirection - 180.0, -windSpeed
        else
            windDirection, windSpeed
    let windDirection =
        if windDirection < 0.0 then
            windDirection + 180.0
        else
            windDirection
    let temperature = getTemperature random t
    let hLcl = (20.0 + temperature / 5.0) * 100.0 * (1.0 - humidity)
    { CloudDensity = humidity
      CloudHeight = max 500.0 hLcl
      CloudThickness = 500.0
      Precipitation = humidity
      Wind = { Speed = windSpeed; Direction = windDirection }
      Turbulence = turbulence
      Temperature = temperature
      Pressure = pressure
    }

let setOptions (weather : WeatherState) (t : System.DateTime) (options : T.Options) =
    let precType =
        if weather.Temperature < 0.0 then
            2 // snow
        else
            1 // rain
    let precLevel =
        int(floor(100.0 * min (weather.Precipitation / 0.8) 1.0))
    let cloudConfig =
        let prefix =
            let x = weather.Precipitation * 5.0
            if x < 1.0 then
                "00_clear"
            elif x < 2.0 then
                "01_light"
            elif x < 3.0 then
                "02_medium"
            elif x < 4.0 then
                "03_heavy"
            else
                "04_overcast"
        let suffix =
            let x = int(floor(weather.Precipitation * 10.0)) % 10
            sprintf "%02d" x
        let season = "summer"
        sprintf @"%s\%s_%s\sky.ini" season prefix suffix
    let windLayers =
        let windDir = int weather.Wind.Direction
        let windSpeed = int weather.Wind.Speed
        options.GetWindLayers().Value
        |> List.map(fun trip ->
            let alt, dir, speed = trip.Value
            T.Options.WindLayers.WindLayers_ValueType(alt, T.Integer windDir, T.Integer windSpeed)
        )
    options
        .SetDate(T.Date(t.Day, t.Month, t.Year))
        .SetTime(T.Options.Time((T.Integer t.Hour, T.Integer t.Minute, T.Integer t.Second)))
        .SetCloudLevel(T.Integer(int(floor(weather.CloudHeight))))
        .SetCloudHeight(T.Integer(int(floor(weather.CloudThickness))))
        .SetPrecType(T.Integer precType)
        .SetPrecLevel(T.Integer precLevel)
        .SetCloudConfig(T.String cloudConfig)
        .SetTurbulence(T.Float weather.Turbulence)
        .SetTemperature(T.Integer(int(floor(weather.Temperature))))
        .SetWindLayers(T.Options.WindLayers(windLayers))