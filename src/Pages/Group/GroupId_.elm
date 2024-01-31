module Pages.Group.GroupId_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Attributes
import Route exposing (Route)
import Html
import Page exposing (Page)
import Shared
import View exposing (View)
import Http
import Json.Decode exposing (Decoder, map2, field, string)

import Components.Sidebar


page : Shared.Model -> Route { groupId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT

type alias Product =
    { id: String
    , name: String}

type  SearchResult
  = Failure Http.Error
  | Loading
  | Success (List Product)

type alias Model =
    { products: SearchResult}


init : Route { groupId : String } -> () -> ( Model, Effect Msg )
init route () =
    ( {products = Loading}
    , Effect.sendCmd <| Http.get
              { url = "/dictionary/group/" ++ route.params.groupId ++ "/product"
              , expect = Http.expectJson GotProducts <| Json.Decode.list productDecoder}
    )

productDecoder: Decoder Product
productDecoder =
    map2 Product
        (field "id" string)
        (field "name" string)


-- UPDATE


type Msg
    = GotProducts (Result Http.Error (List Product))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotProducts res ->
            case res of
                Ok listProduct ->
                      ({ model | products = Success listProduct}
                      , Effect.none)
                Err err ->
                      ({ model | products = Failure err}
                      , Effect.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    Components.Sidebar.view
        { title = "Pages.Group.GroupId_"
        , body = [ drowPageBody model ]
        }

drowPageBody: Model -> Html.Html Msg
drowPageBody model =
    case model.products of
        Loading -> Html.text "it's loading, please waite"
        Failure err ->
            case err of
                Http.BadUrl _ -> Html.text ("BadUrl")
                Http.Timeout -> Html.text ("Timeout")
                Http.NetworkError -> Html.text ("NetWorkError")
                Http.BadStatus code -> Html.text ("Bad status " ++ String.fromInt code)
                Http.BadBody reason -> Html.text ("BadBody. " ++ reason)
        Success listGroups ->
            Html.table [] <| List.append
                                     [ Html.th [] [Html.text "name"]
                                     , Html.th [] [Html.text "id"]
                                     ]
                                     <| List.map productToRow listGroups

productToRow: Product -> Html.Html Msg
productToRow product =
    Html.tr []
        [ Html.td [] [ Html.text product.name]
        , Html.td [] [ Html.a [Html.Attributes.href <| "/product-card/" ++ product.id] [ Html.text "product card"] ]
        ]