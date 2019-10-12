// SturmovikCampaign, a dynamic persistent campaign for IL-2 Sturmovik: Battle of Stalingrad
// Copyright (C) 2018 Johann Deneux <johann.deneux@gmail.com>
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

/// Random weather generation
/// It follows a pattern based on cyclical atmospheric pressure, humidity and temperature.
module Campaign.Weather

open SturmovikMission.Blocks.BlocksMissionData

/// <summary>
/// Cyclical interpolation
/// </summary>
/// <param name="x0">Start of interval</param>
/// <param name="x1">End of interval</param>
/// <param name="points">Points to interpolate between within the interval</param>
let interpolate (x0 : float, x1 : float) (points : (float * float) list) =
    let w = x1 - x0
    let xp, yp = List.last points
    let xp = xp - w
    let xP, yP = List.head points
    let xP = xP + w
    let points = (xp, yp) :: points @ [xP, yP]
    fun (x : float) ->
        let xDiv = (x - x0) / w
        let x' = x0 + x - (floor xDiv) * w
        let (x0, y0), (x1, y1) =
            points
            |> Seq.pairwise
            |> Seq.find (fun ((x0, _), (x1, _)) -> x0 <= x' && x' < x1)
        let f = (x'  - x0) / (x1 - x0)
        y0 * (1.0 - f) + y1 * f

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
with
    static member RainThreshold = 0.95

    member this.IsOvercast =
        this.Precipitation >= WeatherState.RainThreshold

    member this.CloudDescription =
        let x = this.CloudDensity
        if x < 0.1 then
            "clear sky"
        elif x < 0.3 then
            "light clouds"
        elif x < 0.6 then
            "medium clouds"
        elif not this.IsOvercast then
            "heavy clouds"
        else
            "overcast"

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
    let avgTemperature = 7.5
    let magTemperature = 25.0
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
    // Wind speed depends on medium term pressure variation, turbulence depends on short term pressure variation.
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
    let hLcl = (10.0 + temperature / 1.5) * 100.0 * (1.0 - 0.5 * humidity)
    let hLcl =
        if humidity > 0.75 then
            max hLcl 700.0
        else
            max hLcl 200.0
    let thickness = 100.0 + 500.0 * humidity
    { CloudDensity = humidity
      CloudHeight = hLcl
      CloudThickness = thickness
      Precipitation = humidity
      Wind = { Speed = windSpeed; Direction = windDirection }
      Turbulence = turbulence
      Temperature = temperature
      Pressure = pressure
    }

/// Build an options mission section, based on an existing one.
let setOptions (random : System.Random) (weather : WeatherState) (t : System.DateTime) (options : T.Options) =
    let precType =
        if weather.Temperature < 0.0 then
            2 // snow
        else
            1 // rain
    let rainThreshold = WeatherState.RainThreshold
    let rainAmount =
        (weather.Precipitation - rainThreshold) / (1.0 - rainThreshold)
        |> min 1.0
        |> max 0.0
    let precLevel =
        int(floor(100.0 * rainAmount))
    let cloudConfig =
        let prefix =
            let x = weather.Precipitation
            if x < 0.1 then
                "00_clear"
            elif x < 0.3 then
                "01_light"
            elif x < 0.6 then
                "02_medium"
            elif x < rainThreshold then
                "03_heavy"
            else
                "04_overcast"
        let suffix =
            let x = int(floor(weather.Precipitation * 10.0)) % 10
            sprintf "%02d" x
        let season =
            match options.GetSeasonPrefix().Value with
            | "wi" -> "winter"
            | "su" -> "summer"
            | _ ->
                if weather.Temperature < 15.0 then
                    "winter"
                else
                    "summer"
        sprintf @"%s\%s_%s\sky.ini" season prefix suffix
    let windLayers =
        let windDir = int weather.Wind.Direction
        let windSpeed = int weather.Wind.Speed
        options.GetWindLayers().Value
        |> List.map(fun trip ->
            let alt, _, _ = trip.Value
            let dir = int((float windDir + 60.0 * (random.NextDouble() - 0.5)) % 360.0)
            let altSpeedK = 1.0 + (float alt.Value) / 3000.0
            let speed = int(float windSpeed * altSpeedK * (1.0 + random.NextDouble() * 0.1))
            T.Options.WindLayers.WindLayers_ValueType((alt, T.Integer dir, T.Integer speed))
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
        .SetSeaState(T.Integer (int (6.0 * weather.Wind.Speed / 40.0) |> min 6))