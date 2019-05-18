module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, string)
import Json.Decode.Pipeline exposing (required)



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


owmWeatherListDecoder : Decoder (List OwmWeather)
owmWeatherListDecoder =
    Decode.list owmWeatherDecoder


owmWeatherDecoder : Decoder OwmWeather
owmWeatherDecoder =
    Decode.succeed OwmWeather
        |> required "id" int
        |> required "main" string
        |> required "description" string
        |> required "icon" string


owmMainDecoder : Decoder OwmMain
owmMainDecoder =
    Decode.succeed OwmMain
        |> required "temp" float
        |> required "pressure" int
        |> required "humidity" int



---- UPDATE ----


type Msg
    = NoOp
    | ChangeLocation String
    | ShowResults
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
            , Http.get
                { url = "https://api.openweathermap.org/data/2.5/weather?q=London,uk&appid="
                , expect = Http.expectString GotOwmJson
                }
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



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [ class "title" ]
            [ text "Grabbing the Weather" ]
        , p [ class "subtitle" ]
            [ span [ class "fa fa-sun" ] []
            , text " Your Elm App is working!"
            ]
        , p []
            [ strong []
                [ text "Rise and shine!" ]
            ]
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
            [ text "Your location: "
            , text model.location
            ]
        , case model.status of
            Success data ->
                div [] [ text <| String.fromFloat <| data.main.temp ]

            _ ->
                Html.text ""
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
