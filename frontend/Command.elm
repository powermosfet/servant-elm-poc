module Command exposing (..)

import Http
import Json.Decode exposing (..)
import Message exposing (Msg(..))
import Model exposing (Cat)


catDecoder : Decoder Cat
catDecoder =
    map Cat (field "name" string)


getCats : Cmd Msg
getCats =
    let
        url =
            "cats"

        request =
            Http.get url (list catDecoder)
    in
        Http.send NewCats request
