module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, int, list, string)
import Json.Decode.Pipeline exposing (required)



-- MODEL


type Model
    = Failure
    | Loading
    | Success Roster


type alias Roster =
    { count : Int
    , astronauts : List Astronaut
    }


type alias Astronaut =
    { craft : String
    , name : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.get
        { url = "http://api.open-notify.org/astros.json"
        , expect = Http.expectString GotJson
        }
    )



-- DECODER


rosterDecoder : Decoder Roster
rosterDecoder =
    Decode.succeed Roster
        |> required "number" int
        |> required "people" (list astronautDecoder)


astronautDecoder : Decoder Astronaut
astronautDecoder =
    Decode.succeed Astronaut
        |> required "craft" string
        |> required "name" string



-- MSG


type Msg
    = GotJson (Result Http.Error String)



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotJson result ->
            case result of
                Ok fullJson ->
                    let
                        getRoster =
                            decodeString rosterDecoder fullJson
                    in
                    case getRoster of
                        Ok newRoster ->
                            ( Success newRoster, Cmd.none )

                        Err _ ->
                            ( Failure, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Who's in Space?" ]
        , case model of
            Failure ->
                div [] [ text "Couldn't load feed." ]

            Loading ->
                div [] [ text "Loading..." ]

            Success fullRoster ->
                div []
                    [ viewRoster fullRoster
                    ]
        ]


viewCell : List (Attribute Msg) -> String -> Html Msg
viewCell style str =
    td
        style
        [ text str ]


viewAstronaut : Astronaut -> Html Msg
viewAstronaut astro =
    tr []
        [ viewCell (cellPadding ++ solidBorderRight) astro.name
        , viewCell cellPadding astro.craft
        ]


viewRoster : Roster -> Html Msg
viewRoster roster =
    div []
        [ p []
            [ text <|
                (++) "There are " <|
                    (++) (String.fromInt (List.length roster.astronauts)) <|
                        " people in space right now."
            ]
        , table
            solidBorder
            ([ tr
                []
                [ th (cellPadding ++ solidBorderBottom ++ solidBorderRight) [ text "Name" ]
                , th (cellPadding ++ solidBorderBottom) [ text "Craft" ]
                ]
             ]
                ++ List.map viewAstronaut roster.astronauts
            )
        ]



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- STYLES


solidBorder : List (Attribute msg)
solidBorder =
    [ style "border" "1px solid black" ]


solidBorderBottom : List (Attribute msg)
solidBorderBottom =
    [ style "border-bottom" "1px solid black" ]


solidBorderRight : List (Attribute msg)
solidBorderRight =
    [ style "border-right" "1px solid black" ]


cellPadding : List (Attribute msg)
cellPadding =
    [ style "padding" "5px"
    , style "text-align" "left"
    ]
