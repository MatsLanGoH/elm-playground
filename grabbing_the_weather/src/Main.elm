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
    }


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
    ( { currentPage = SearchPage, location = "", status = Awaiting }
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
    div [ class "container" ]
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


viewSearchPage : Model -> Html Msg
viewSearchPage model =
    div [ class "card" ]
        [ h1 [ class "card-header" ]
            [ text "Search Page" ]
        , p [ class "card-content" ]
            [ text "Let's implement a search page here "
            , input [ value model.location, placeholder "Location", onInput ChangeLocation ] []
            , button [ class "button", onClick ShowResults ] [ text "Search" ]
            ]
        ]


viewResultPage : Model -> Html Msg
viewResultPage model =
    div [ class "card" ]
        [ h1 [ class "card-header" ]
            [ text "Results Page" ]
        , p [ class "card-content" ]
            [ case model.status of
                Success data ->
                    viewResultWeatherData data

                _ ->
                    Html.text ""
            , button [ class "button", onClick ShowSearch ] [ text "New search" ]
            ]
        ]


viewResultWeatherData : OwmData -> Html Msg
viewResultWeatherData data =
    let
        weatherData =
            getWeather data.weather
    in
    div []
        [ table [ class "table is-hoverable is-striped" ]
            [ thead []
                [ tr []
                    [ th [] [ text "item" ]
                    , th [] [ text "value" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text "Location" ]
                    , td [] [ data.name |> text ]
                    ]
                , tr []
                    [ td [] [ text "Temperature" ]
                    , td [] [ "°C" |> (++) (data.main.temp |> kelvinToCelsius |> String.fromInt) |> text ]
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



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
