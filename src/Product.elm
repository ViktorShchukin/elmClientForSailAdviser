module Product exposing (..)

import Json.Decode exposing (Decoder, field, map2, string)
type alias Product =
    { id: String
    , name: String
    }


productDecoder: Decoder Product
productDecoder =
    map2 Product
        (field "id" string)
        (field "name" string)