module View exposing (..)

import Html exposing (Html, div, ul, li, text)
import Model exposing (Model, Cat)
import Message exposing (Msg)


view : Model -> Html Msg
view model =
    div []
        [ ul []
            (List.map
                catLi
                model.cats
            )
        ]


catLi : Cat -> Html Msg
catLi cat =
    li [] [ text cat.name ]
