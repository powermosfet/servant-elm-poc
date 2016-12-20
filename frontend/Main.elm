module Main exposing (main)

import Html
import Message
import Model
import Init
import Update
import View


main =
    Html.programWithFlags { init = Init.init, view = View.view, update = Update.update, subscriptions = subscriptions }


subscriptions : Model.Model -> Sub Message.Msg
subscriptions model =
    Sub.none
