module Sale exposing (..)


import Iso8601
import Json.Decode exposing (Decoder, field, float, int, map5, string)
import Time
type alias Sale =
    { id: String
    , productId: String
    , quantity: Int
    , totalValue: Float
    , date: Time.Posix
    }

saleDecoder: Decoder Sale
saleDecoder =
    map5 Sale
        (field "id" string)
        (field "productId" string)
        (field "quantity" int)
        (field "cost" float)
        (field "date" Iso8601.decoder)