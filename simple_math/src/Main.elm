module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, h2, input, p, text)
import Html.Attributes exposing (value)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { x : Int
    , y : Int
    }


init : Model
init =
    Model 1 1



-- UPDATE


type Msg
    = UpdateX String
    | UpdateY String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateX newX ->
            case String.toInt newX of
                Just int ->
                    if int > 0 then
                        { model | x = int }

                    else
                        model

                _ ->
                    model

        UpdateY newY ->
            case String.toInt newY of
                Just int ->
                    if int > 0 then
                        { model | y = int }

                    else
                        model

                _ ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Simple Math" ]
        , div []
            [ h2 [] [ text "Input" ]
            , p []
                [ text "What is the first number? "
                , input [ value (String.fromInt model.x), onInput UpdateX ] []
                ]
            , p []
                [ text "What is the second number? "
                , input [ value (String.fromInt model.y), onInput UpdateY ] []
                ]
            ]
        , div []
            [ h2 [] [ text "Output" ]
            , viewCalc model " + " (+)
            , viewCalc model " - " (-)
            , viewCalc model " * " (*)
            , viewCalc model " // " (//)
            ]
        ]



-- Note: (Int -> Int -> Int) is the type signature of an Integer arithmetic operator


type alias Arithmetic =
    Int -> Int -> Int


viewCalc : Model -> String -> Arithmetic -> Html Msg
viewCalc model sym op =
    p []
        [ text <|
            String.fromInt model.x
                ++ sym
                ++ String.fromInt model.y
                ++ " = "
                ++ String.fromInt (op model.x model.y)
        ]
