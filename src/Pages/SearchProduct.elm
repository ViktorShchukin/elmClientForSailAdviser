module Pages.SearchProduct exposing (Model, Msg, page)

import Html exposing (Attribute, Html, div, input, text, br, table, th, thead, tr, td, a)
import Html.Attributes exposing (attribute, placeholder, value, href)
import Html.Events exposing (onInput)
import Page exposing (Page)
import View exposing (View)
import Http
import Json.Decode exposing (Decoder, map2, field, string)
import Route

import Components.Sidebar

page : Page Model Msg
page =
    Page.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT

type alias Product =
    { id: String
    , name: String
    }

type  SearchResult = Failure Http.Error
  | Loading
  | Success (List Product)

type alias Model =
    { content: String
    , products: SearchResult
    , statusMessage: Maybe String
    }

init : (Model, Cmd Msg)
init =
    ({ content = ""
     , products = Loading
     , statusMessage = Nothing
     }, Cmd.none)



-- UPDATE


type Msg
  = Change String
  | GotProduct (Result Http.Error (List Product))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newContent ->
            if String.length newContent >= 3 then
                ({model | content = newContent
                , statusMessage = Nothing}, doSearch newContent)
            else
                ({model | content = newContent
                , statusMessage = Just "Type more"
                , products = Loading}, Cmd.none)

        GotProduct res ->
          case res of
            Ok listProduct ->
                if (List.length listProduct) == 0 then
                    ({model | products = Success listProduct
                   , statusMessage = Just "I can't find this product"}, Cmd.none)
                else
                  ({model | products = Success listProduct
                   , statusMessage = Nothing}, Cmd.none)

            Err err ->
              ({model | products = Failure err
               , statusMessage = Just "error"}, Cmd.none)

doSearch: String -> Cmd Msg
doSearch str =
  Http.get
      { url = "/dictionary/product?product_name=" ++ str
      , expect = Http.expectJson GotProduct (Json.Decode.list productDecoder)
      }

productDecoder: Decoder Product
productDecoder =
    map2 Product
        (field "id" string)
        (field "name" string)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    let
        statusString =
            case model.statusMessage of
                Just str -> str
                Nothing -> ""
    in
        Components.Sidebar.view
            { title = "SearchProduct"
            , body = [
                        div []
                        [ input [ placeholder "type product name", value model.content, onInput Change ] []
                        , div [] <| drawProductsTable model.products
                        , div [] [text statusString]
                        ]
                     ]
            }



drawProductsTable: SearchResult -> List (Html Msg)
drawProductsTable products =
    case products of
        Loading ->
            [text ("it's loading")]
        Failure err ->
            case err of
                Http.BadUrl _ -> [text ("BadUrl")]
                Http.Timeout -> [text ("Timeout")]
                Http.NetworkError -> [text ("NetWorkError")]
                Http.BadStatus code -> [text ("Bad status " ++ String.fromInt code)]
                Http.BadBody _ -> [text ("BadBody")]
        Success pl ->
             [table [ attribute "role" "grid" ]
                (List.append
                [ th [] [text "product name"]
                ]
                <| List.map productToRow pl)]

productToRow: Product -> Html Msg
productToRow product =
    tr []
        [ td [] [  a [href <| "/product-card/" ++ product.id] [ text product.name ] ]
        ]