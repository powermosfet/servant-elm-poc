module Model exposing (..)

import Message exposing (Msg(..))


type alias Model =
    String


init : Maybe Model -> ( Model, Cmd Msg )
init m =
    ( "", Cmd.none )
