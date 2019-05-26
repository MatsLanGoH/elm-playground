module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Config exposing (owmApiBaseUrl, owmApiKey)
import Convert exposing (humanTimeHMS, humanTimeMD, kelvinToCelsius, kelvinToFahrenheit)
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string)
import Json.Decode.Pipeline exposing (optional, required)
import Task exposing (Task)
import Time
import TimeZone
import Url.Builder as U exposing (crossOrigin, string)



---- MODEL ----


type alias Model =
    { currentPage : Page
    , previousPage : Maybe Page
    , navbar : Toggle
    , location : String
    , weatherData : WeatherDataStatus
    , forecastData : ForecastDataStatus
    , error : String
    , unit : Unit
    , timezone : Time.Zone
    , zonename : String
    , dayOrNight : DayOrNight
    }


type Unit
    = Celsius
    | Fahrenheit
    | Kelvin


type WeatherDataStatus
    = AwaitingWeatherData
    | FailureWeatherData
    | SuccessWeatherData OwmData


type ForecastDataStatus
    = AwaitingForecastData
    | FailureForecastData
    | SuccessForecastData OwmForecastList


type Toggle
    = Active
    | Inactive


type TZ
    = TzFailure TimeZone.Error
    | TzSuccessWeatherData String Time.Zone


type Page
    = SearchPage
    | ResultPage
    | SettingsPage


type DayOrNight
    = Day
    | Night


type KindOfDay
    = Fantastic
    | Normal
    | Hot
    | Cold


type alias OwmData =
    { weather : List OwmWeather
    , main : OwmMain
    , sys : OwmSys
    , wind : Maybe OwmWind
    , name : String
    , dt : Int
    }


type alias OwmForecastList =
    { list : List OwmForecast
    }


type alias OwmForecast =
    { dt : Int
    , main : OwmMain
    , weather : List OwmWeather
    , wind : Maybe OwmWind
    }


type alias OwmWeather =
    { id : Int
    , main : String
    , description : String
    , icon : String
    }


type alias OwmMain =
    { temp : Float
    , humidity : Int
    }


type alias OwmSys =
    { type_ : Int
    , id_ : Int
    , message : Float
    , country : String
    , sunrise : Int
    , sunset : Int
    }


type alias OwmWind =
    { speed : Float
    , degree : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = SearchPage
      , previousPage = Nothing
      , navbar = Inactive
      , location = ""
      , weatherData = AwaitingWeatherData
      , forecastData = AwaitingForecastData
      , error = ""
      , unit = Celsius
      , timezone = Time.utc
      , zonename = "UTC"
      , dayOrNight = Day
      }
    , TimeZone.getZone |> Task.attempt ReceiveTimeZone
    )



-- DECODERS


owmDataDecoder : Decoder OwmData
owmDataDecoder =
    Decode.succeed OwmData
        |> required "weather" owmWeatherListDecoder
        |> required "main" owmMainDecoder
        |> required "sys" owmSysDecoder
        |> required "wind" (nullable owmWindDecoder)
        |> required "name" Decode.string
        |> required "dt" int


owmWeatherListDecoder : Decoder (List OwmWeather)
owmWeatherListDecoder =
    Decode.list owmWeatherDecoder


owmWeatherDecoder : Decoder OwmWeather
owmWeatherDecoder =
    Decode.succeed OwmWeather
        |> required "id" int
        |> required "main" Decode.string
        |> required "description" Decode.string
        |> required "icon" Decode.string


owmMainDecoder : Decoder OwmMain
owmMainDecoder =
    Decode.succeed OwmMain
        |> required "temp" float
        |> required "humidity" int


owmWindDecoder : Decoder OwmWind
owmWindDecoder =
    Decode.succeed OwmWind
        |> required "speed" float
        |> required "deg" float


owmSysDecoder : Decoder OwmSys
owmSysDecoder =
    Decode.succeed OwmSys
        |> required "type" int
        |> required "id" int
        |> required "message" float
        |> required "country" Decode.string
        |> required "sunrise" int
        |> required "sunset" int


owmForecastListDecoder : Decoder OwmForecastList
owmForecastListDecoder =
    Decode.succeed OwmForecastList
        |> required "list" (Decode.list owmForecastDecoder)


owmForecastDecoder : Decoder OwmForecast
owmForecastDecoder =
    Decode.succeed OwmForecast
        |> required "dt" int
        |> required "main" owmMainDecoder
        |> required "weather" owmWeatherListDecoder
        |> required "wind" (nullable owmWindDecoder)



---- UPDATE ----


type Msg
    = NoOp
    | ChangeLocation String
    | SwitchTo Unit
    | ShowResults
    | ShowSearch
    | TogglePage Page
    | ToggleNavbar Toggle
    | ToggleSettings
    | GotOwmDataJson (Result Http.Error String)
    | GotOwmForecastJson (Result Http.Error String)
    | ReceiveTimeZone (Result TimeZone.Error ( String, Time.Zone ))
    | SetTimeZone String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocation newLocation ->
            ( { model | location = newLocation }
            , Cmd.none
            )

        SwitchTo unit ->
            ( { model | unit = unit }
            , Cmd.none
            )

        ShowResults ->
            ( model
            , let
                resultUrl =
                    crossOrigin owmApiBaseUrl [ "weather" ] [ U.string "q" model.location, U.string "appid" owmApiKey ]
              in
              Http.get
                { url = resultUrl
                , expect = Http.expectString GotOwmDataJson
                }
            )

        ShowSearch ->
            ( { model | currentPage = SearchPage, location = "" }
            , Cmd.none
            )

        ToggleSettings ->
            case model.currentPage of
                SettingsPage ->
                    let
                        previous =
                            case model.previousPage of
                                Nothing ->
                                    SearchPage

                                Just page ->
                                    page
                    in
                    ( { model | previousPage = Just SettingsPage, currentPage = previous }
                    , Cmd.none
                    )

                _ ->
                    ( { model | previousPage = Just model.currentPage, currentPage = SettingsPage }
                    , Cmd.none
                    )

        ToggleNavbar toggle ->
            case toggle of
                Active ->
                    ( { model | navbar = Inactive }
                    , Cmd.none
                    )

                Inactive ->
                    ( { model | navbar = Active }
                    , Cmd.none
                    )

        TogglePage newPage ->
            ( { model | currentPage = newPage }
            , Cmd.none
            )

        GotOwmDataJson result ->
            case result of
                Ok fullJson ->
                    let
                        owmResponse =
                            decodeString owmDataDecoder fullJson
                    in
                    case owmResponse of
                        Ok data ->
                            ( { model | weatherData = SuccessWeatherData data, dayOrNight = getDayOrNight data }
                            , let
                                resultUrl =
                                    crossOrigin owmApiBaseUrl [ "forecast" ] [ U.string "q" model.location, U.string "appid" owmApiKey ]
                              in
                              Http.get
                                { url = resultUrl
                                , expect = Http.expectString GotOwmForecastJson
                                }
                            )

                        Err _ ->
                            ( { model | weatherData = FailureWeatherData }, Cmd.none )

                Err _ ->
                    ( { model | weatherData = FailureWeatherData }, Cmd.none )

        GotOwmForecastJson result ->
            case result of
                Ok fullJson ->
                    let
                        owmResponse =
                            decodeString owmForecastListDecoder fullJson
                    in
                    case owmResponse of
                        Ok data ->
                            { model | forecastData = SuccessForecastData data }
                                |> update (TogglePage ResultPage)

                        Err error ->
                            { model | forecastData = FailureForecastData, error = Decode.errorToString error }
                                |> update (TogglePage ResultPage)

                Err _ ->
                    ( { model | forecastData = FailureForecastData }, Cmd.none )

        ReceiveTimeZone result ->
            ( case result of
                Ok ( zoneName, zone ) ->
                    { model | timezone = zone, zonename = zoneName }

                Err error ->
                    { model | timezone = Time.utc }
            , Cmd.none
            )

        SetTimeZone newZonename ->
            case Dict.get newZonename TimeZone.zones of
                Nothing ->
                    ( model
                    , Cmd.none
                    )

                Just tz ->
                    ( { model | zonename = newZonename, timezone = tz () }
                    , Cmd.none
                    )

        NoOp ->
            ( model, Cmd.none )



-- VIEW HELPERS


getWeather : List OwmWeather -> OwmWeather
getWeather owmWeatherList =
    case List.head owmWeatherList of
        Nothing ->
            OwmWeather 0 "" "" ""

        Just owmWeather ->
            owmWeather


getDayOrNight : OwmData -> DayOrNight
getDayOrNight owmData =
    if (owmData.sys.sunrise < owmData.dt) && (owmData.dt < owmData.sys.sunset) then
        Day

    else
        Night


getKindOfDay : OwmData -> KindOfDay
getKindOfDay owmData =
    let
        celsius =
            kelvinToCelsius owmData.main.temp

        weather =
            getWeather owmData.weather
    in
    if celsius < 12 && weather.id < 800 then
        Cold

    else if celsius >= 35 then
        Hot

    else if celsius > 20 && weather.id > 800 then
        Fantastic

    else
        Normal



---- VIEW ----


view : Model -> Html Msg
view model =
    section [ class "hero" ]
        [ viewNavBar model
        , div [ class "container is-widescreen" ]
            [ h1 [ class "title" ]
                [ text "Grabbing the Weather" ]
            , p [ class "subtitle " ]
                [ text "Rise and shine!" ]
            , div [ class "card" ]
                [ case model.currentPage of
                    SearchPage ->
                        viewSearchPage model

                    ResultPage ->
                        viewResultPage model

                    SettingsPage ->
                        viewSettingsPage model
                ]
            , div [ class "box" ]
                [ div
                    [ class "field" ]
                    [ button
                        [ class "button is-large"
                        , onClick ToggleSettings
                        ]
                        [ span [ class "icon is-large is-right" ]
                            [ i [ class "fa fa-cog" ] []
                            ]
                        ]
                    ]
                ]
            ]
        ]


viewNavBar : Model -> Html Msg
viewNavBar model =
    let
        navBarWeatherDataStatus =
            case model.navbar of
                Active ->
                    " is-active"

                Inactive ->
                    ""
    in
    nav [ class "navbar is-fixed-top" ]
        [ div [ class "navbar-brand" ]
            [ div [ class ("navbar-burger" ++ navBarWeatherDataStatus), onClick (ToggleNavbar model.navbar) ]
                [ span [] []
                , span [] []
                , span [] []
                ]
            ]
        , div [ class ("navbar-menu" ++ navBarWeatherDataStatus) ]
            [ div [ class "navbar-start" ]
                [ div [ class "navbar-item" ]
                    [ div [ class "" ]
                        [ p [ class "columns is-mobile buttons are-medium" ]
                            [ div [ class "column" ]
                                [ navButton "°C" (model.unit == Celsius) (SwitchTo Celsius) ]
                            , div [ class "column" ]
                                [ navButton "°F" (model.unit == Fahrenheit) (SwitchTo Fahrenheit) ]
                            , div [ class "column" ]
                                [ navButton "K" (model.unit == Kelvin) (SwitchTo Kelvin) ]
                            ]
                        ]
                    ]
                ]
            , div [ class "navbar-end" ]
                [ div [ class "navbar-item" ]
                    [ div [ class "field" ]
                        [ p [ class "control has-icons-left is-expanded" ]
                            [ span [ class "select is-fullwidth" ]
                                [ select [ onInput SetTimeZone ]
                                    (viewSelectOptions model.zonename TimeZone.zones)
                                ]
                            , span [ class "icon is-left" ]
                                [ i [ class "fas fa-clock" ] [] ]
                            ]
                        ]
                    ]
                ]
            ]
        ]


navButton : String -> Bool -> Msg -> Html Msg
navButton buttonText isActive msg =
    let
        buttonWeatherDataStatus =
            case isActive of
                True ->
                    "button is-success"

                False ->
                    "button"
    in
    button [ class buttonWeatherDataStatus, onClick msg ] [ text buttonText ]


viewSettingsPage : Model -> Html Msg
viewSettingsPage model =
    div [ class "card-content" ]
        [ div [ class "box" ]
            [ viewRadioTemperatureUnit model
            ]
        , div [ class "box" ]
            [ viewSelectTimeZone model
            ]
        ]


viewSearchPage : Model -> Html Msg
viewSearchPage model =
    div [ class "card-content" ]
        [ div [ class "box" ]
            [ h3 [ class "subtitle" ] [ text "Where are you at?" ]
            , div [ class "field has-addons" ]
                [ div [ class "control is-expanded" ]
                    [ input [ class "input is-large", value model.location, placeholder "Location", onInput ChangeLocation ] []
                    ]
                , div [ class "control" ]
                    [ button [ class "button is-large is-info", onClick ShowResults, disabled (String.length model.location == 0) ]
                        [ span [ class "icon is-large is-right" ]
                            [ i [ class "fa fa-globe" ] []
                            ]
                        ]
                    ]
                ]
            , viewSearchError model
            ]
        ]


viewSearchError : Model -> Html Msg
viewSearchError model =
    case model.weatherData of
        FailureWeatherData ->
            p [ class "text-danger" ] [ text "Location not found." ]

        _ ->
            text ""


viewResultPage : Model -> Html Msg
viewResultPage model =
    div []
        [ case model.weatherData of
            SuccessWeatherData data ->
                viewResultWeatherData model data

            _ ->
                Html.text ""
        , case model.forecastData of
            SuccessForecastData forecasts ->
                viewResultForecast model forecasts

            _ ->
                Html.text model.error
        , div [ class "box" ]
            [ button [ class "button", onClick ShowSearch ]
                [ text "New search" ]
            ]
        ]


viewResultWeatherData : Model -> OwmData -> Html Msg
viewResultWeatherData model data =
    let
        weatherData =
            getWeather data.weather
    in
    div [ class "box" ]
        [ viewWeatherCatchPhrase model weatherData data
        , div [ class "level-item" ]
            [ table [ class "table is-hoverable is-striped" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Location" ]
                        , th [] [ data.name |> text ]
                        ]
                    , tr []
                        [ th [] [ text "Date" ]
                        , th [] [ humanTimeMD data.dt model.timezone |> text ]
                        ]
                    ]
                , tbody []
                    [ tr []
                        [ td [] [ text "Temperature" ]
                        , td [] [ viewTemperature model data.main |> text ]
                        ]
                    , tr []
                        [ td [] [ text "Weather" ]
                        , td [] [ weatherData.main |> text ]
                        ]
                    , tr []
                        [ td [] [ text "Humidity" ]
                        , td [] [ "%" |> (++) (data.main.humidity |> String.fromInt) |> text ]
                        ]
                    , tr []
                        [ td [] [ text "Sunrise" ]
                        , td [] [ humanTimeHMS data.sys.sunrise model.timezone model.zonename |> text ]
                        ]
                    , tr []
                        [ td [] [ text "Sunset" ]
                        , td [] [ humanTimeHMS data.sys.sunset model.timezone model.zonename |> text ]
                        ]
                    , trDisplayWind data.wind
                    ]
                ]
            ]
        ]


viewResultForecast : Model -> OwmForecastList -> Html Msg
viewResultForecast model forecasts =
    div [ class "box" ]
        [ div [ class "title" ]
            [ text "Weekly forecast"
            ]
        , div [ class "level-item" ]
            [ table [ class "table is-fullwidth is-hoverable is-striped" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Date" ]
                        , th [] [ text "Weather " ]
                        ]
                    ]
                , tbody []
                    (viewForecastTable forecasts)
                ]
            ]
        ]


viewForecastTable : OwmForecastList -> List (Html Msg)
viewForecastTable forecasts =
    List.map viewForecastRow forecasts.list


viewForecastRow : OwmForecast -> Html Msg
viewForecastRow forecast =
    let
        weather =
            getWeather forecast.weather

        temperatureString =
            -- viewTemperature model forecast.main
            "22"

        weatherText =
            temperatureString ++ " / " ++ weather.main
    in
    tr []
        [ td [] [ text <| humanTimeMD forecast.dt Time.utc ] -- model.timezone ]
        , td [] [ text <| weatherText ]
        ]


viewWeatherCatchPhrase : Model -> OwmWeather -> OwmData -> Html Msg
viewWeatherCatchPhrase model weather data =
    let
        symbol =
            if model.dayOrNight == Day then
                "sun"

            else
                "moon"

        kindOfDayPhrase =
            case getKindOfDay data of
                Normal ->
                    ""

                Cold ->
                    "Keep warm!"

                Hot ->
                    "Stay hydrated!"

                Fantastic ->
                    "Enjoy your day!"

        ( icon, catchphrase ) =
            if weather.id > 802 then
                ( "cloud"
                , "I'm sure there's a silver lining."
                )

            else if weather.id > 800 then
                ( "cloud-" ++ symbol
                , "What's a few clouds, ey?"
                )

            else if weather.id == 800 then
                ( symbol
                , "Clear blue sky!"
                )

            else if weather.id >= 730 then
                ( "cloud-meatball"
                , "The skies are falling!"
                )

            else if weather.id >= 700 then
                ( "smog"
                , "Sight ain't so great today!"
                )

            else if weather.id >= 600 then
                ( "snowflake"
                , "Winter is coming."
                )

            else if weather.id >= 500 then
                ( "cloud-showers-heavy"
                , "Might wanna get an umbrella."
                )

            else if weather.id >= 300 then
                ( "cloud-" ++ symbol ++ "rain"
                , "It's not rain, more like a curative mist."
                )

            else if weather.id >= 200 then
                ( "bolt"
                , "Thunderbolt and lightning? Check."
                )

            else
                ( "rainbow"
                , "Here's your weather report."
                )
    in
    div [ class "title is-3 is-spaced" ]
        [ p []
            [ i [ class ("fa fa-" ++ icon) ] []
            ]
        , p [] [ text catchphrase ]
        , p [] [ text kindOfDayPhrase ]
        ]


viewRadioTemperatureUnit : Model -> Html Msg
viewRadioTemperatureUnit model =
    div [ class "text-center" ]
        [ h3 [ class "subtitle" ]
            [ text "Choose temperature unit: " ]
        , fieldset
            []
            [ radio "Celsius" (model.unit == Celsius) (SwitchTo Celsius)
            , radio "Fahrenheit" (model.unit == Fahrenheit) (SwitchTo Fahrenheit)
            , radio "Kelvin" (model.unit == Kelvin) (SwitchTo Kelvin)
            ]
        ]


radio : String -> Bool -> Msg -> Html Msg
radio value isChecked msg =
    label [ class "radio" ]
        [ input [ type_ "radio", name "unit", onClick msg, checked isChecked ] []
        , text (" " ++ value)
        ]


viewSelectTimeZone : Model -> Html Msg
viewSelectTimeZone model =
    div [ class "text-center" ]
        [ h3 [ class "subtitle" ]
            [ text "Choose time zone: " ]
        , div [ class "field" ]
            [ p [ class "control has-icons-left is-expanded" ]
                [ span [ class "select is-fullwidth" ]
                    [ select [ onInput SetTimeZone ]
                        (viewSelectOptions model.zonename TimeZone.zones)
                    ]
                , span [ class "icon is-left" ]
                    [ i [ class "fas fa-globe" ] [] ]
                ]
            ]
        ]


viewSelectOptions : String -> Dict String (() -> Time.Zone) -> List (Html Msg)
viewSelectOptions check tzDict =
    List.map (selectOption check) (Dict.keys tzDict)


selectOption : String -> String -> Html Msg
selectOption check val =
    option [ value val, selected (check == val) ] [ text val ]


viewTemperature : Model -> OwmMain -> String
viewTemperature model owmMain =
    case model.unit of
        Fahrenheit ->
            "°F" |> (++) (owmMain.temp |> kelvinToFahrenheit |> String.fromInt)

        Celsius ->
            "°C" |> (++) (owmMain.temp |> kelvinToCelsius |> String.fromInt)

        Kelvin ->
            "K" |> (++) (owmMain.temp |> String.fromFloat)


trDisplayWind : Maybe OwmWind -> Html Msg
trDisplayWind owmWind =
    case owmWind of
        Just wind ->
            tr []
                [ td [] [ text "Wind" ]
                , td [] [ viewWind wind |> text ]
                ]

        _ ->
            text ""


viewWind : OwmWind -> String
viewWind owmWind =
    let
        direction =
            if owmWind.degree >= 348.75 then
                "N"

            else if owmWind.degree >= 281.25 then
                "NW"

            else if owmWind.degree >= 236.25 then
                "W"

            else if owmWind.degree >= 191.25 then
                "SW"

            else if owmWind.degree >= 146.25 then
                "S"

            else if owmWind.degree >= 101.25 then
                "SE"

            else if owmWind.degree >= 56.25 then
                "E"

            else if owmWind.degree >= 11.25 then
                "NE"

            else
                "N"
    in
    String.fromFloat owmWind.speed
        ++ "m/s from "
        ++ direction



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
