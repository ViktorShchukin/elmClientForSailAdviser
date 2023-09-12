-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--
module Main exposing (..)
import Browser
import Html exposing (Html, Attribute, div, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import Http
import Json.Decode exposing (Decoder, map2, field, string)



-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL

type alias Product =
    { name: String
    , id: String
    }

type Model = Failure
  | Loading
  | Success Product


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, Cmd.none)



-- UPDATE


type Msg
  = Change String 
  | GotProduct (Result Http.Error Product)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      (Loading, doSearch newContent)

    GotProduct res ->
      case res of
        Ok product ->
          (Success product, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



doSearch: String -> Cmd Msg
doSearch str =
  Http.get
      { url = "/dictionary/product?product_name=" ++ str
      , expect = Http.expectJson GotProduct productDecoder
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
    [ input [ placeholder "Text to reverse", onInput Change ] []
    , div [] [ text (String.reverse model.content) ]
    , div [] [text ("result: " ++ model.strJSON)]
    ]