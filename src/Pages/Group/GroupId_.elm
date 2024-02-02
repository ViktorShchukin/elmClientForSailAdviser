module Pages.Group.GroupId_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Attributes
import Html.Events
import Route exposing (Route)
import Html
import Page exposing (Page)
import Shared
import View exposing (View)
import Http
import Json.Decode exposing (Decoder, map2, map3, field, string, float, maybe)

import Components.Sidebar


page : Shared.Model -> Route { groupId : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update route
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Product =
    { id: String
    , name: String
    }


type  RequestResultForProducts
  = Failure Http.Error
  | Loading
  | Success (List Product)




type alias Model =
    { productsInSearch: RequestResultForProducts
    , productsInGroup: RequestResultForProducts
    , content: String
    }

init : Route { groupId : String } -> () -> ( Model, Effect Msg )
init route () =
    ( { productsInSearch = Loading
      , productsInGroup = Loading
      , content = ""}
    , Effect.sendCmd <| doSearchForProductsInGroup route.params.groupId
    )


doSearchForProductsInGroup: String -> Cmd Msg
doSearchForProductsInGroup route =
    Http.get
                  { url = "/dictionary/group/" ++ route ++ "/product"
                  , expect = Http.expectJson GotProductsForGroup <| Json.Decode.list productDecoder}



productDecoder: Decoder Product
productDecoder =
    map2 Product
        (field "id" string)
        (field "name" string)



-- UPDATE


type Msg
    = GotProductsForGroup (Result Http.Error (List Product))
    | GotProductsForSearch (Result Http.Error (List Product))
    | Change String
    | AddProductToGroup String
    | GotResponseAddProduct (Result Http.Error ())


update : Route { groupId : String } -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        GotProductsForGroup res ->
            case res of
                Ok listProduct ->
                      ({ model | productsInGroup = Success listProduct}
                      , Effect.none)
                Err err ->
                      ({ model | productsInGroup = Failure err}
                      , Effect.none)
        GotProductsForSearch res ->
            case res of
                Ok listProduct ->
                      ({ model | productsInSearch = Success listProduct}
                      , Effect.none)
                Err err ->
                      ({ model | productsInSearch = Failure err}
                      , Effect.none)
        Change newContent ->
            if String.length newContent >= 3 then
                ({model | content = newContent}, Effect.sendCmd <| doSearch newContent)
            else
                ({model | content = newContent}, Effect.none)
        AddProductToGroup productId ->
            (model, Effect.sendCmd <| requestToAddProductToGroup route.params.groupId productId)
        GotResponseAddProduct res ->
            (model, Effect.sendCmd <| doSearchForProductsInGroup route.params.groupId)


requestToAddProductToGroup: String -> String -> Cmd Msg
requestToAddProductToGroup groupId productId =
    Http.post { url = "/dictionary/group/" ++ groupId ++ "/product/" ++ productId
              , body = Http.emptyBody
              , expect = Http.expectWhatever GotResponseAddProduct
              }


doSearch: String -> Cmd Msg
doSearch str =
    Http.get
              { url = "/dictionary/product?product_name=" ++ str
              , expect = Http.expectJson GotProductsForSearch (Json.Decode.list productDecoder)
              }


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    Components.Sidebar.view
        { title = "Pages.Group.GroupId_"
        , body = [ drawPageBody model ]
        }

drawPageBody: Model -> Html.Html Msg
drawPageBody model =
    Html.div [ Html.Attributes.class "grid"] [ Html.div [] [ drawProductInGroup model ]
                                             , Html.div [] [ Html.input [ Html.Attributes.placeholder "type product name", Html.Attributes.value model.content, Html.Events.onInput Change] []
                                                           , drawSearchAndAddProduct model] ]






drawProductInGroup: Model -> Html.Html Msg
drawProductInGroup model =
    case model.productsInGroup of
        Loading -> Html.text "it's loading, please wait"
        Failure err ->
            case err of
                Http.BadUrl _ -> Html.text ("BadUrl")
                Http.Timeout -> Html.text ("Timeout")
                Http.NetworkError -> Html.text ("NetWorkError")
                Http.BadStatus code -> Html.text ("Bad status " ++ String.fromInt code)
                Http.BadBody reason -> Html.text ("BadBody. " ++ reason)
        Success listProducts -> Html.table [] <| List.append [ Html.th [] [Html.text "name"]
                                                             , Html.th [] [Html.text "id"]
                                                             ] <| List.map productToRow listProducts



productToRow: Product -> Html.Html Msg
productToRow product =
    Html.tr []
        [ Html.td [] [ Html.a [Html.Attributes.href <| "/product-card/" ++ product.id] [ Html.text product.name] ]
        ]


drawSearchAndAddProduct: Model -> Html.Html Msg
drawSearchAndAddProduct model =
    case model.productsInSearch of
        Loading -> Html.text "it's loading, please wait"
        Failure err ->
            case err of
                Http.BadUrl _ -> Html.text ("BadUrl")
                Http.Timeout -> Html.text ("Timeout")
                Http.NetworkError -> Html.text ("NetWorkError")
                Http.BadStatus code -> Html.text ("Bad status " ++ String.fromInt code)
                Http.BadBody reason -> Html.text ("BadBody. " ++ reason)
        Success listProducts -> Html.table [] <| List.append [ Html.th [] [Html.text "name"]
                                                             , Html.th [] [Html.text "id"]
                                                             ] <| List.map productToRowForSearch listProducts


productToRowForSearch: Product -> Html.Html Msg
productToRowForSearch product =
    Html.tr []
        [ Html.td [] [Html.button [ Html.Events.onClick <| AddProductToGroup product.id ] [ Html.text "add to group"] ]
        , Html.td [] [ Html.a [Html.Attributes.href <| "/product-card/" ++ product.id] [ Html.text product.name] ]
        ]
