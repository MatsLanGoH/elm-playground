module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Config exposing (owmApiBaseUrl, owmApiKey)
import Convert exposing (humanTimeHM, kelvinToCelsius, kelvinToFahrenheit)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, string)
import Json.Decode.Pipeline exposing (required)
import Url.Builder as U exposing (crossOrigin, string)



---- MODEL ----


type alias Model =
    { currentPage : Page
    , location : String
    , status : Status
    , unit : Unit
    }


type Unit
    = Celsius
    | Fahrenheit
    | Kelvin


type Status
    = Awaiting
    | Failure
    | Success OwmData


type Page
    = SearchPage
    | ResultPage


type alias OwmData =
    { weather : List OwmWeather
    , main : OwmMain
    , sys : OwmSys
    , name : String
    }


type alias OwmWeather =
    { id : Int
    , main : String
    , description : String
    , icon : String
    }


type alias OwmMain =
    { temp : Float
    , pressure : Int
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


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = SearchPage, location = "", status = Awaiting, unit = Celsius }
    , Cmd.none
    )



-- DECODERS


owmDataDecoder : Decoder OwmData
owmDataDecoder =
    Decode.succeed OwmData
        |> required "weather" owmWeatherListDecoder
        |> required "main" owmMainDecoder
        |> required "sys" owmSysDecoder
        |> required "name" Decode.string


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
        |> required "pressure" int
        |> required "humidity" int


owmSysDecoder : Decoder OwmSys
owmSysDecoder =
    Decode.succeed OwmSys
        |> required "type" int
        |> required "id" int
        |> required "message" float
        |> required "country" Decode.string
        |> required "sunrise" int
        |> required "sunset" int



---- UPDATE ----


type Msg
    = NoOp
    | ChangeLocation String
    | SwitchTo Unit
    | ShowResults
    | ShowSearch
    | GotOwmJson (Result Http.Error String)


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
            ( { model | currentPage = ResultPage }
            , let
                resultUrl =
                    crossOrigin owmApiBaseUrl [] [ U.string "q" model.location, U.string "appid" owmApiKey ]
              in
              Http.get
                { url = resultUrl
                , expect = Http.expectString GotOwmJson
                }
            )

        ShowSearch ->
            ( { model | currentPage = SearchPage, location = "" }
            , Cmd.none
            )

        GotOwmJson result ->
            case result of
                Ok fullJson ->
                    let
                        owmResponse =
                            decodeString owmDataDecoder fullJson
                    in
                    case owmResponse of
                        Ok data ->
                            ( { model | status = Success data }, Cmd.none )

                        Err _ ->
                            ( { model | status = Failure }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

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



---- VIEW ----


view : Model -> Html Msg
view model =
    section [ class "section" ]
        [ div [ class "container is-size-5" ]
            [ h1 [ class "title" ]
                [ text "Grabbing the Weather" ]
            , p [ class "subtitle " ]
                [ text "Rise and shine!" ]
            , case model.currentPage of
                SearchPage ->
                    viewSearchPage model

                ResultPage ->
                    viewResultPage model
            ]
        ]


viewSearchPage : Model -> Html Msg
viewSearchPage model =
    div [ class "card" ]
        [ h1 [ class "card-header level-item" ]
            [ text "Search Page" ]
        , p [ class "card-content" ]
            [ text "Where are you at? "
            , br [] []
            , input [ value model.location, placeholder "Location", onInput ChangeLocation ] []
            ]
        , button [ class "button", onClick ShowResults, disabled (String.length model.location == 0) ] [ text "Search" ]
        ]


viewResultPage : Model -> Html Msg
viewResultPage model =
    div [ class "card " ]
        [ h1 [ class "card-header level-item" ]
            [ text "Results Page" ]
        , case model.status of
            Success data ->
                viewResultWeatherData model data

            _ ->
                Html.text ""
        , viewRadioTemperatureUnit model
        , button [ class "button", onClick ShowSearch ] [ text "New search" ]
        ]


viewResultWeatherData : Model -> OwmData -> Html Msg
viewResultWeatherData model data =
    let
        weatherData =
            getWeather data.weather
    in
    p [ class "card-content level-item has-text-centered" ]
        [ table [ class "table is-hoverable is-striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Location" ]
                    , th [] [ data.name |> text ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text "Temperature" ]
                    , td [] [ viewTemperature model data |> text ]
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
                    , td [] [ humanTimeHM data.sys.sunrise |> text ]
                    ]
                , tr []
                    [ td [] [ text "Sunset" ]
                    , td [] [ humanTimeHM data.sys.sunset |> text ]
                    ]
                ]
            ]
        ]


viewRadioTemperatureUnit : Model -> Html Msg
viewRadioTemperatureUnit model =
    div [ class "control" ]
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


viewTemperature : Model -> OwmData -> String
viewTemperature model data =
    case model.unit of
        Fahrenheit ->
            "°F" |> (++) (data.main.temp |> kelvinToFahrenheit |> String.fromInt)

        Celsius ->
            "°C" |> (++) (data.main.temp |> kelvinToCelsius |> String.fromInt)

        Kelvin ->
            "K" |> (++) (data.main.temp |> String.fromFloat)



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
