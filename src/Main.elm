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
import Json.Decode exposing (Decoder, map4, field, int, string)



-- MAIN


main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
  { content : String
    ,strJSON : String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ({ content = ""
    ,strJSON = ""
  }, Cmd.none)



-- UPDATE


type Msg
  = Change String 
  | GotText (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Change newContent ->
      ({ model | content = newContent }, doSearch newContent)

    GotText res ->
      case res of
        Ok str ->
          ({model | strJSON = str}, Cmd.none)

        Err _ ->
          (model, Cmd.none)



doSearch: String -> Cmd Msg
doSearch str =
  Http.get
      { url = "/dictionary/product?product_name=" ++ str
      , expect = Http.expectString GotText
      }

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input [ placeholder "Text to reverse", value model.content, onInput Change ] []
    , div [] [ text (String.reverse model.content) ]
    , div [] [text ("result: " ++ model.strJSON)]
    ]