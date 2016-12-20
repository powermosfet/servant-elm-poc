module Model exposing (..)

import Http


type alias Cat =
    { name : String }


type alias Model =
    { cats : List Cat }
