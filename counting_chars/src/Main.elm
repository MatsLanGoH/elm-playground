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
    { input : String }


init : Model
init =
    { input = "" }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newInput ->
            { model | input = newInput }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Counting Chars" ]
        , p [] [ text "What is the input string?" ]
        , input [ placeholder "Your string...", value model.input, onInput Change ] []
        , viewCount model
        ]


viewCount : Model -> Html Msg
viewCount model =
    case String.length model.input of
        0 ->
            p [] [ text "Please enter a string!" ]

        length ->
            p []
                [ text
                    (model.input
                        ++ " has "
                        ++ Debug.toString length
                        ++ (if length > 1 then
                                " characters."

                            else
                                " character."
                           )
                    )
                ]
