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
    { base : Int
    }


init : Model
init =
    { base = 12
    }



-- UPDATE


type Msg
    = SetBase String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetBase newBaseString ->
            case String.toInt newBaseString of
                Just newBase ->
                    { model | base = newBase }

                _ ->
                    model



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Multiplication Table" ]
        , select [ onInput SetBase ]
            (List.range 1 12
                |> List.map (intToOption model)
            )
        , p []
            [ text "Selected: "
            , text <| String.fromInt model.base
            ]
        , table
            [ style "border" "1px solid"
            , style "text-align" "center"
            ]
            (List.range 0 model.base
                |> List.map (multiplyRow model.base)
            )
        ]


intToOption : Model -> Int -> Html Msg
intToOption model v =
    let
        vString =
            String.fromInt v
    in
    option
        [ value vString
        , selected (model.base == v)
        ]
        [ text vString ]


intToCell : Int -> Html Msg
intToCell v =
    td
        [ style "border" "1px solid"
        , style "padding" "5px"
        ]
        [ text (String.fromInt v) ]


multiplyRow : Int -> Int -> Html Msg
multiplyRow base multiplier =
    tr []
        (List.range 0 base
            |> List.map ((*) multiplier)
            |> List.map intToCell
        )
