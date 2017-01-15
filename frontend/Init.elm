module Init exposing (..)

import Model exposing (Model)
import Message exposing (Msg)
import Command exposing (getCats)


init : ( Model, Cmd Msg )
init =
    ( { cats = [] }, getCats )
