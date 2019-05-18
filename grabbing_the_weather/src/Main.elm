module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



---- MODEL ----


type alias Model =
    { currentPage : Page
    , location : String
    }


type Page
    = SearchPage
    | ResultPage


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentPage = SearchPage, location = "" }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | ChangeLocation String
    | ShowResults


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocation newLocation ->
            ( { model | location = newLocation }
            , Cmd.none
            )

        ShowResults ->
            ( { model | currentPage = ResultPage }
            , Cmd.none
            )

        _ ->
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
        , p [ class "card-body" ]
            [ text "Let's implement a search page here " ]
        , input [ value model.location, placeholder "Location", onInput ChangeLocation ] []
        , button [ class "button", onSubmit ShowResults ]
            [ text "Search" ]
        ]


viewResultPage : Model -> Html Msg
viewResultPage model =
    div [ class "card" ]
        [ h1 [ class "card-header" ]
            [ text "Results Page" ]
        , p [ class "card-body" ]
            [ text "Your location: "
            , text model.location
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
