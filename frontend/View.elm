module View exposing (..)

import Html exposing (Html, div, h1, p, ul, li, text)
import Model exposing (Model, Cat)
import Message exposing (Msg)
import Css
import Styles.Styles as Styles


view : Model -> Html Msg
view model =
    div []
        [ node "style" (Css.compile [ Styles.css ])
        , h1 [] [ text "servant-elm-poc" ]
        , p [] [ text "The API returned the following cats:" ]
        , ul []
            (List.map
                catLi
                model.cats
            )
        ]


catLi : Cat -> Html Msg
catLi cat =
    li [] [ text cat.name ]
