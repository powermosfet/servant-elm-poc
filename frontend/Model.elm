module Model exposing (..)


type alias Model =
    String


init : Maybe Model -> ( Model, Cmd Msg )
init m =
    ( "", Cmd.none )
