-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--
module Main exposing (..)
import Browser
import Html exposing (Attribute, Html, div, input, text, br, table, th, thead, tr,td)
import Html.Attributes exposing (attribute, placeholder, value)
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
    , statusMessage: Maybe String
    }


init : () -> (Model, Cmd Msg)
init _ =
  ({ content = ""
   , products = Loading
   , statusMessage = Nothing
   }, Cmd.none)



-- UPDATE


type Msg
  = Change String 
  | GotProduct (Result Http.Error (List Product))


update : Msg -> Model -> (Model, Cmd Msg)
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

        Err _ ->
          ({model | products = Failure
           , statusMessage = Just "Network error"}, Cmd.none)



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
  let
      statusString = case model.statusMessage of
          Just str -> str
          Nothing -> ""
  in
      div []
        [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
        , div [] <| drawProductsTable model.products
        , div [] [text statusString]
        ]

drawProductsTable: SearchResult -> List (Html Msg)
drawProductsTable products =
    case products of
        Loading ->
            [text ("it's loading")]
        Failure ->
            [text ("loading has failed")]
        Success pl ->
             [table [ attribute "role" "grid" ]
                (List.append
                [ th [] [text "product id"]
                , th [] [text "product name"]
                ]
                <| List.map productToRow pl)]

productToRow: Product -> Html Msg
productToRow product =
    tr []
        [ td [] [text product.id]
        , td [] [text product.name]
        ]

            {-List.map (\product -> text <|product.id ++ "|" ++ product.name) pl
            |> List.intersperse (br [] [])-}