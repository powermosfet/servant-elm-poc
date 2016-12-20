module Init exposing (..)

import Model exposing (Model)
import Message exposing (Msg)
import Command exposing (getCats)


init : Maybe Model -> ( Model, Cmd Msg )
init m =
    ( { cats = [] }, getCats )
