module Pages.ProductCard.ProductId_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Route exposing (Route)
import Html
import Page exposing (Page)
import Shared
import View exposing (View)
import Http
import Json.Decode exposing (Decoder, map5, field, string, int, float)

import Components.Sidebar

page : Shared.Model -> Route { productId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init
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

type  SearchResult = Failure
  | Loading
  | Success (List Sale)

type PredictionResult = Failure
    | Success
    | Nothing

type alias Model =
    { sales: SearchResult
    , prediction: PredictionResult
    }


init : () -> ( Model, Effect Msg )
init () =
    ( { sales = Loading
      , prediction = Nothing
      }
    , Effect.sendCmd <|
         Http.get
            { url = "https://elm-lang.org/assets/public-opinion.txt"
            , expect = Http.expectJson GotSales <| Json.Decode.list saleDecoder
            }
    )

saleDecoder: Decoder Sale
saleDecoder =
    map5 Sale
        (field "sale_id" string)
        (field "product_id" string)
        (field "quantity" int)
        (field "total_value" float)
        (field "date" string)



-- UPDATE


type Msg
    = GotSales (Result Http.Error (List Sale))
    | DoPrediction
    | GotPrediction


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
        DoPrediction ->
            (model
            , Effect.none
            )
        GotPrediction ->
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    Components.Sidebar.view
        { title = "Pages.ProductCard.ProductId_"
        , body = [ Html.text "/product-card/:product-id" ]
        }
