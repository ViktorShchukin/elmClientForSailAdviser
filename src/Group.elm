module Group exposing (..)

import Http
import Iso8601
import Json.Decode exposing (Decoder, field, map3, string)
import Time

type alias Group =
    { id: String
    , name: String
    , creationDate: Time.Posix}


groupDecoder: Decoder Group
groupDecoder =
    map3 Group
        (field "id" string)
        (field "name" string)
        (field "creationDate" Iso8601.decoder)
