module Convert exposing (humanTimeHM, kelvinToCelsius, kelvinToFahrenheit)

import Time exposing (toHour, toMinute, toSecond, utc)



-- Temperature conversion


kelvinToCelsius : Float -> Int
kelvinToCelsius kelvin =
    floor <| kelvin - 273.15


kelvinToFahrenheit : Float -> Int
kelvinToFahrenheit kelvin =
    floor <| kelvin * 9.0 / 5.0 - 459.67



-- Time conversion


toUtcString : Time.Posix -> String
toUtcString time =
    padString (String.fromInt (toHour utc time))
        ++ ":"
        ++ padString (String.fromInt (toMinute utc time))
        ++ ":"
        ++ padString (String.fromInt (toSecond utc time))
        ++ " (UTC)"


padString : String -> String
padString str =
    case String.length str of
        1 ->
            "0" ++ str

        _ ->
            str


humanTimeHM : Int -> String
humanTimeHM ms =
    let
        time =
            Time.millisToPosix (ms * 1000)
    in
    toUtcString time
