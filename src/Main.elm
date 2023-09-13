-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--
module Main exposing (..)
import Browser
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, map2, field, string)



-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL

type alias Product =
    { id: String
    , name: String
    }

type  SearchResult = Failure
  | Loading
  | Success (List Product)

type alias Model =
    { content: String
    , products: SearchResult
    }


init : () -> (Model, Cmd Msg)
init _ =
  ({ content = ""
   , products = Loading
   }, Cmd.none)



-- UPDATE


type Msg
  = Change String 
  | GotProduct (Result Http.Error (List Product))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ({model | content = newContent}, doSearch newContent)

    GotProduct res ->
      case res of
        Ok listProduct ->
          ({model | products = Success listProduct}, Cmd.none)

        Err _ ->
          ({model | products = Failure}, Cmd.none)



doSearch: String -> Cmd Msg
doSearch str =
  Http.get
      { url = "/dictionary/product?product_name=" ++ str
      , expect = Http.expectJson GotProduct (Json.Decode.list productDecoder)
      }

productDecoder: Decoder Product
productDecoder =
    map2 Product
        (field "product_id" string)
        (field "name" string)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
    , div [] [ text (String.reverse model.content) ]
    , div [] <| drawProductsTable model.products
    ]

drawProductsTable: SearchResult -> List (Html Msg)
drawProductsTable products =
    case products of
        Loading ->
            [text ("it's loading")]
        Failure ->
            [text ("loading has failed")]
        Success pl ->
            List.map (\product -> text <|product.id ++ "|" ++ product.name) pl
