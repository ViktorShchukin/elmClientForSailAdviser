module Pages.ProductCard.ProductId_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Html
import Html.Attributes exposing (attribute, class)
import Page exposing (Page)
import Shared
import View exposing (View)
import Http
import Json.Decode exposing (Decoder, map2, map5, field, string, int, float)

import Components.Sidebar

page : Shared.Model -> Route { productId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT

type alias Sale =
    { id: String
    , productId: String
    , quantity: Int
    , totalValue: Float
    , date: String
    }

type  SearchResult
    = Failure
    | Loading
    | Success (List Sale)

type alias Prediction =
    { range: Int
    , value: Int
    }

type PredictionResult
    = FailurePrediction Http.Error
    | SuccessPrediction Prediction
    | Nothing


type alias Model =
    { sales: SearchResult
    , prediction: PredictionResult
    }


init : Route { productId : String }  -> () -> ( Model, Effect Msg )
init route () =
    ( { sales = Loading
      , prediction = Nothing
      }
    , Effect.batch
         [ Effect.sendCmd <| Http.get
            { url = "/dictionary/product/" ++ route.params.productId ++ "/sale"
            , expect = Http.expectJson GotSales <| Json.Decode.list saleDecoder
            }
         , Effect.sendCmd <| doPrediction route "30"
         ]

    )

saleDecoder: Decoder Sale
saleDecoder =
    map5 Sale
        (field "id" string)
        (field "product_id" string)
        (field "quantity" int)
        (field "total_sum" float)
        (field "date" string)



-- UPDATE


type Msg
    = GotSales (Result Http.Error (List Sale))
    --| Predict
    | GotPrediction (Result Http.Error Prediction)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotSales res ->
            case res of
                Ok listSales ->
                    ({model | sales = Success listSales}
                    , Effect.none)
                Err _ ->
                    ({model | sales = Failure}
                    , Effect.none)
        {-Predict ->
            ( model
            , Effect.sendCmd doPrediction
            )-}
        GotPrediction res ->
            case res of
                Ok pred ->
                    ( {model | prediction = SuccessPrediction pred}
                    , Effect.none)


                Err error ->
                    ( {model | prediction = FailurePrediction error}
                    , Effect.none
                    )


doPrediction: Route { productId : String } -> String -> Cmd Msg
doPrediction route range =
    Http.get
        { url = "/dictionary/product/" ++ route.params.productId ++ "/prediction/" ++ range
        , expect = Http.expectJson GotPrediction  predictionDecoder}

predictionDecoder: Decoder Prediction
predictionDecoder =
    map2 Prediction
        (field "range" int)
        (field "value" int)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    Components.Sidebar.view
        { title = "ProductCard"
        , body = [ drawPageBody model ]
        }

drawPageBody: Model -> Html.Html Msg
drawPageBody model =
    Html.div [class "grid"] [ Html.div [] [ drawSalesTable model.sales]
                , Html.div [] [ drawPrediction model.prediction]
                ]


drawSalesTable: SearchResult -> Html.Html Msg
drawSalesTable res =
    case res of
        Loading -> Html.text "it's loading, please waite"
        Failure -> Html.text "Something went wrong. I can't get sales of this product"
        Success listSales ->
            Html.table [] <| List.append
                             [ Html.th [] [Html.text "date"]
                             , Html.th [] [Html.text "quantity"]
                             ]
                             <| List.map saleToRow listSales



saleToRow: Sale -> Html.Html Msg
saleToRow sale =
    Html.tr []
            [ Html.td [] [Html.text sale.date]
            , Html.td [] [Html.text <| String.fromInt sale.quantity]
            ]

--todo refactor from table to div construction
drawPrediction: PredictionResult -> Html.Html Msg
drawPrediction res =
    Html.table [] [ Html.th [] [Html.text "Prediction rate"]
                  , Html.th [] [Html.text "Prediction result"]
                  , drawPredictionRow res
                  ]


drawPredictionRow: PredictionResult -> Html.Html Msg
drawPredictionRow res =
    case res of
        Nothing -> Html.text "select prediction rate"
        FailurePrediction error ->
            case error of
                Http.BadUrl _ -> Html.text ("BadUrl")
                Http.Timeout -> Html.text ("Timeout")
                Http.NetworkError -> Html.text ("NetWorkError")
                Http.BadStatus code -> Html.text ("Bad status " ++ String.fromInt code)
                Http.BadBody reason -> Html.text ("BadBody. " ++ reason)
        SuccessPrediction prediction ->
             Html.tr []
                    [ Html.td [] [ Html.text <| String.fromInt prediction.range ]
                    , Html.td [] [ Html.text <| String.fromInt prediction.value ]
                    ]


