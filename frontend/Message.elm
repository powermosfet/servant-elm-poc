module Message exposing (..)

import Http
import Model exposing (Cat)


type Msg
    = NewCats (Result Http.Error (List Cat))
