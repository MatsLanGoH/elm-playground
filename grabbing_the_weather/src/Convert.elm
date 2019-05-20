module Convert exposing (humanTimeHMS, kelvinToCelsius, kelvinToFahrenheit)

import Time exposing (Posix, Zone, toHour, toMinute, toSecond, utc)
import TimeZone



-- Temperature conversion


kelvinToCelsius : Float -> Int
kelvinToCelsius kelvin =
    floor <| kelvin - 273.15


kelvinToFahrenheit : Float -> Int
kelvinToFahrenheit kelvin =
    floor <| kelvin * 9.0 / 5.0 - 459.67



-- Time conversion


posixToString : Posix -> Zone -> String -> String
posixToString time zone zonename =
    padString (String.fromInt (toHour zone time))
        ++ ":"
        ++ padString (String.fromInt (toMinute zone time))
        ++ ":"
        ++ padString (String.fromInt (toSecond zone time))
        ++ " ("
        ++ zonename
        ++ ")"


padString : String -> String
padString str =
    case String.length str of
        1 ->
            "0" ++ str

        _ ->
            str


humanTimeHMS : Int -> Zone -> String -> String
humanTimeHMS ms zone zonename =
    let
        time =
            Time.millisToPosix (ms * 1000)
    in
    posixToString time zone zonename
