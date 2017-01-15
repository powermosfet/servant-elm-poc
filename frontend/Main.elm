module Main exposing (main)

import Html
import Init
import Update
import View


main =
    Html.program
        { init = Init.init
        , view = View.view
        , update = Update.update
        , subscriptions = \_ -> Sub.none
        }
