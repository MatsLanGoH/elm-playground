module Main exposing (Model, Msg(..), init, main, update, view, viewMonth)

import Array exposing (..)
import Browser
import Html exposing (Html, div, h1, input, p, text)
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
    { month : Maybe Int
    }


init : Model
init =
    { month = Nothing }



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change newInput ->
            { model | month = String.toInt newInput }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Numbers to Names" ]
        , p []
            [ text "Please enter the number of the month: "
            , input [ onInput Change ] []
            ]
        , viewMonth model
        ]


viewMonth : Model -> Html Msg
viewMonth model =
    case model.month of
        Nothing ->
            text ""

        Just month ->
            let
                monthNames : Array String
                monthNames =
                    Array.fromList
                        [ "January"
                        , "February"
                        , "March"
                        , "April"
                        , "May"
                        , "June"
                        , "July"
                        , "August"
                        , "September"
                        , "October"
                        , "November"
                        , "December"
                        ]
            in
            case Array.get (month - 1) monthNames of
                Just monthName ->
                    p []
                        [ text <|
                            "The name of the month is "
                                ++ monthName
                                ++ "."
                        ]

                Nothing ->
                    text ""
