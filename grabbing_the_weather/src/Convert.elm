module Convert exposing (humanTimeHMS, humanTimeMD, kelvinToCelsius, kelvinToFahrenheit)

import Time exposing (Posix, Zone, toDay, toHour, toMinute, toMonth, toSecond, utc)
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


posixToDate : Posix -> Zone -> String
posixToDate time zone =
    let
        day =
            posixToDay time zone

        ordinal =
            case String.right 2 day of
                "11" ->
                    "th"

                "12" ->
                    "th"

                "13" ->
                    "th"

                "1" ->
                    "st"

                "2" ->
                    "nd"

                "3" ->
                    "rd"

                _ ->
                    "th"
    in
    (++) (posixToMonth time zone) <|
        (++) " " <|
            (++) day <|
                ordinal


posixToMonth : Posix -> Zone -> String
posixToMonth time zone =
    case toMonth zone time of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


posixToDay : Posix -> Zone -> String
posixToDay time zone =
    toDay zone time
        |> String.fromInt


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


humanTimeMD : Int -> Zone -> String
humanTimeMD ms zone =
    let
        time =
            Time.millisToPosix (ms * 1000)
    in
    posixToDate time zone
