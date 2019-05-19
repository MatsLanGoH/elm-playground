module Convert exposing (humanTimeHM, kelvinToCelsius, kelvinToFahrenheit)

import Time exposing (toHour, toMinute, toSecond, utc)



-- Temperature conversion


kelvinToCelsius : Float -> Float
kelvinToCelsius kelvin =
    kelvin - 273.15


kelvinToFahrenheit : Float -> Float
kelvinToFahrenheit kelvin =
    kelvin * 9.0 / 5.0 - 459.67



-- Time conversion


toUtcString : Time.Posix -> String
toUtcString time =
    String.fromInt (toHour utc time)
        ++ ":"
        ++ String.fromInt (toMinute utc time)
        ++ ":"
        ++ String.fromInt (toSecond utc time)
        ++ " (UTC)"


humanTimeHM : Int -> String
humanTimeHM ms =
    let
        time =
            Time.millisToPosix ms
    in
    toUtcString time
