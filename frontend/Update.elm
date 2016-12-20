module Update exposing (..)

import Message exposing (Msg(..))
import Model exposing (Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        NewCats (Ok cats) ->
            ( { model | cats = cats }, Cmd.none )

        _ ->
            ( model, Cmd.none )
