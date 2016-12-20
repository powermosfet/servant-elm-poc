module Styles.Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (div, body, li)
import Styles.Colors as Color
import Styles.Classes as Class


css =
    stylesheet
        [ body
            [ backgroundColor Color.background
            ]
        ]
