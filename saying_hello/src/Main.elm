module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { currentInput : String
    , name : String
    }


init : Model
init =
    { currentInput = ""
    , name = ""
    }



-- UPDATE


type Msg
    = Change String
    | UpdateName


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newInput ->
            { model | currentInput = newInput }

        UpdateName ->
            { model
                | name = model.currentInput
                , currentInput = ""
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Saying Hello" ]
        , p []
            [ text "What is your name? "
            ]
        , input [ placeholder "Your name", value model.currentInput, onInput Change ] []
        , button [ onClick UpdateName ] [ text "Send" ]
        , viewGreeting model
        ]


viewGreeting : Model -> Html Msg
viewGreeting model =
    case String.length model.name of
        0 ->
            text model.name

        _ ->
            p [] [ text ("Hello, " ++ model.name ++ ", nice to meet you!") ]
