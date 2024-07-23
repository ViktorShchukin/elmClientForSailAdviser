module Prediction exposing (..)

import Iso8601
import Json.Decode exposing (Decoder, field, float, map3, string)
import Time


type alias Prediction =
    { range: Time.Posix
    , value: Float
    , productId: String
    }

predictionDecoder: Decoder Prediction
predictionDecoder =
    map3 Prediction
        (field "predictionDate" Iso8601.decoder)
        (field "value" float)
        (field "productId" string)
