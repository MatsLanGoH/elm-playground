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
    { noun : String
    , verb : String
    , adjective : String
    , adverb : String
    , story : String
    }


init : Model
init =
    Model "" "" "" "" ""



-- UPDATE


type Msg
    = ChangeNoun String
    | ChangeVerb String
    | ChangeAdjective String
    | ChangeAdverb String
    | CreateStory


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeNoun newNoun ->
            { model | noun = newNoun }

        ChangeVerb newVerb ->
            { model | verb = newVerb }

        ChangeAdjective newAdjective ->
            { model | adjective = newAdjective }

        ChangeAdverb newAdverb ->
            { model | adverb = newAdverb }

        CreateStory ->
            { model
                | story =
                    "Do you "
                        ++ model.verb
                        ++ " your "
                        ++ model.adjective
                        ++ " "
                        ++ model.noun
                        ++ " "
                        ++ model.adverb
                        ++ "? That's hilarious!"
            }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Mad Lib" ]
        , viewInput "Enter a noun..." model.noun ChangeNoun
        , viewInput "Enter a verb..." model.verb ChangeVerb
        , viewInput "Enter an adjective..." model.adjective ChangeAdjective
        , viewInput "Enter an adverb..." model.adverb ChangeAdverb
        , button [ onClick CreateStory ] [ text "Create story" ]
        , viewStory model
        ]


viewInput : String -> String -> (String -> Msg) -> Html Msg
viewInput t v m =
    p [] [ input [ placeholder t, value v, onInput m ] [] ]


viewStory : Model -> Html Msg
viewStory model =
    p [] [ text model.story ]
