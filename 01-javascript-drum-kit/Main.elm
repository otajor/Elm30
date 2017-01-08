port module Main exposing (..)

import Html exposing (Attribute, Html, audio, div, span, kbd, text)
import Html.Attributes exposing (class, controls, type_, src, id, attribute)
import Html.Events exposing (on)
import Json.Decode as Json
import Char exposing (fromCode)
import String exposing (fromChar)
import Debug
import List exposing (map, filter, head, sortBy)
import Keyboard


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Sound =
    { order : Int
    , key : Char
    , desc : String
    , playing : Bool
    }


type alias Model =
    List Sound



-- MSG


type Msg
    = NoOp
    | KeyDown Keyboard.KeyCode
    | StopPlaying Char



-- INIT


init : ( Model, Cmd Msg )
init =
    [ { order = 1, key = 'A', desc = "clap", playing = False }
    , { order = 2, key = 'S', desc = "hihat", playing = False }
    , { order = 3, key = 'D', desc = "kick", playing = False }
    , { order = 4, key = 'F', desc = "openhat", playing = False }
    , { order = 5, key = 'G', desc = "boom", playing = False }
    , { order = 6, key = 'H', desc = "ride", playing = False }
    , { order = 8, key = 'J', desc = "snare", playing = False }
    , { order = 9, key = 'K', desc = "tom", playing = False }
    , { order = 10, key = 'L', desc = "tink", playing = False }
    ]
        ! []



-- UPDATE


getSound : Char -> List Sound -> Maybe Sound
getSound key =
    head << filter (\x -> x.key == key)


getOtherSounds : Char -> List Sound -> List Sound
getOtherSounds key =
    filter (\x -> x.key /= key)


changePlaying : Bool -> Char -> List Sound -> List Sound
changePlaying status key sounds =
    let
        maybeSound =
            getSound key sounds
    in
        case maybeSound of
            Just sound ->
                sortBy .order <|
                    { sound | playing = status }
                        :: getOtherSounds key sounds

            Nothing ->
                sounds


startPlaying : Char -> List Sound -> List Sound
startPlaying =
    changePlaying True


stopPlaying : Char -> List Sound -> List Sound
stopPlaying =
    changePlaying False


port play : String -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown code ->
            ( startPlaying (fromCode code) model, play <| toString code )

        StopPlaying key ->
            ( stopPlaying key model, Cmd.none )

        _ ->
            Debug.log "Unknown message" ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown ]



-- VIEW


getClasses : Sound -> String
getClasses drum =
    if drum.playing then
        "key playing"
    else
        "key"


toKeyDiv : Sound -> Html Msg
toKeyDiv drum =
    div
        [ class <| getClasses drum
        , on "transitionend" (Json.succeed (StopPlaying drum.key))
        ]
        [ kbd [] [ text <| fromChar drum.key ]
        , span [ class "sound" ] [ text drum.desc ]
        ]


view : Model -> Html Msg
view model =
    div [ class "keys" ]
        (map toKeyDiv model)
