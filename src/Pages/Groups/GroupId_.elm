module Pages.Groups.GroupId_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Attributes
import Html.Events
import Route exposing (Route)
import Html
import Page exposing (Page)
import Shared
import View exposing (View)
import Http
import Json.Decode exposing (Decoder, map2, map3, field, string, float, int)
import Json.Encode
import Time
import Iso8601
import Task
import File.Download

import MyTime
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

type alias Prediction =
    { range: Time.Posix
    , value: Float
    , productId: String
    }

type alias CustomValue =
    { value: Int
    , productId: String
    }

type alias GroupRow =
    { product: Product
    , prediction: Maybe Prediction
    , customValue: Maybe CustomValue
    }

type alias Date =
    { humanString: String
    , posix: Time.Posix
    , status: Maybe String
    }

type alias Group =
    { id: String
    , name: String
    , creationDate: String}


type alias Model =
    { productsInSearch: List Product
    , productsInGroup: List GroupRow
    , content: String
    , range: Date
    , group: Group
    , debug: List String
    }

productToGroupRow: Product -> GroupRow
productToGroupRow product =
    GroupRow product Nothing Nothing



init : Route { groupId : String } -> () -> ( Model, Effect Msg )
init route () =
    ( { productsInSearch = []
      , productsInGroup = []
      , content = ""
      , range = Date "" (Time.millisToPosix 0) Nothing  --todo create normal input for prediction and add date time module
      , group = Group route.params.groupId "" ""
      , debug = []
      }
    , Effect.batch  [Effect.sendCmd <| doSearchForProductsInGroup route.params.groupId, Effect.sendCmd getInitTime, Effect.sendCmd <| requestToGetGroup route.params.groupId]
    )


getInitTime: Cmd Msg
getInitTime =
    Task.perform InitTime Time.now


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


doPrediction: Time.Posix  -> Product -> Cmd Msg
doPrediction range product =
    Http.get
        { url = "/dictionary/product/" ++ product.id ++ "/prediction/" ++ Iso8601.fromTime range
        , expect = Http.expectJson GotPrediction  predictionDecoder}


predictionDecoder: Decoder Prediction
predictionDecoder =
    map3 Prediction
        (field "range" Iso8601.decoder)
        (field "value" float)
        (field "product-id" string)

requestToGetGroup: String -> Cmd Msg
requestToGetGroup groupId=
    Http.get
        { url = "/dictionary/group/" ++ groupId
        , expect = Http.expectJson GotGroup groupDecoder}


groupDecoder: Decoder Group
groupDecoder =
    map3 Group
        (field "id" string)
        (field "name" string)
        (field "creation-date" string)


-- UPDATE


type Msg
    = GotProductsForGroup (Result Http.Error (List Product))
    | GotProductsForSearch (Result Http.Error (List Product))
    | Change String
    | AddProductToGroup String
    | DeleteProductFromGroup String
    | GotResponseAddProduct (Result Http.Error ())
    | GotResponseDeleteProduct (Result Http.Error ())
    | GotPrediction (Result Http.Error Prediction)
    | ChangeCustomValue Product String
    | ChangePredictionRange String
    | GotCustomValue (Result Http.Error CustomValue)
    | GotResponseUpdateCustomValue (Result Http.Error ())
    | InitTime Time.Posix
    | GotGroup (Result Http.Error Group)
    | DownloadFile



update : Route { groupId : String } -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        GotProductsForGroup res ->
            case res of
                Ok listProduct ->
                      ({ model | productsInGroup = List.map productToGroupRow listProduct}
                      , Effect.batch <| List.append
                                          (List.map Effect.sendCmd <| List.map (doPrediction model.range.posix) listProduct)
                                          (List.map Effect.sendCmd <| List.map (requestToGetCustomValue route.params.groupId) listProduct)
                      )
                Err err ->
                      (model, Effect.none)

        GotProductsForSearch res ->
            case res of
                Ok listProduct ->
                      ({ model | productsInSearch = listProduct}
                      , Effect.none)
                Err err ->
                      (model
                      , Effect.none)

        Change newContent ->
            if String.length newContent >= 3 then
                ({model | content = newContent}, Effect.sendCmd <| doSearch newContent)
            else
                ({model | content = newContent}, Effect.none)

        AddProductToGroup productId -> (model, Effect.sendCmd <| requestToAddProductToGroup route.params.groupId productId)

        DeleteProductFromGroup productId -> (model, Effect.sendCmd <| requestToDeleteProductFromGroup route.params.groupId productId)

        GotResponseDeleteProduct res -> (model, Effect.sendCmd <| doSearchForProductsInGroup route.params.groupId)

        GotResponseAddProduct res ->
            (model, Effect.sendCmd <| doSearchForProductsInGroup route.params.groupId)

        GotPrediction res ->
            case res of
                Ok pred -> ( setPrediction model pred, Effect.none)
                Err err -> (model, Effect.none)

        ChangeCustomValue product str -> case String.toInt str of
                                            Nothing -> (model,Effect.sendCmd <| requestToUpdateCustomValue route.params.groupId product 0)
                                            Just a -> (model, Effect.sendCmd <| requestToUpdateCustomValue route.params.groupId product a)

        ChangePredictionRange newRange -> case MyTime.fromInputStringToTime newRange of
                                              Nothing -> (setDateError model newRange, Effect.none)
                                              Just newTime ->
                                                  (setDate model newTime
                                                  , Effect.batch <| List.map Effect.sendCmd
                                                                 <| List.map (doPrediction newTime)
                                                                 <| List.map getProductFromGroupRow model.productsInGroup) --todo need to do request for prediction

        GotCustomValue res ->
            case res of
                Ok value -> ( setCustomValue model value, Effect.none)
                Err err ->  let oldDebug = model.debug in ({ model | debug = List.append oldDebug ["err"] }, Effect.none)

        GotResponseUpdateCustomValue res -> (model, Effect.batch <| List.map Effect.sendCmd
                                                                 <| List.map (requestToGetCustomValue route.params.groupId)
                                                                 <| List.map getProductFromGroupRow model.productsInGroup)

        InitTime timeNow -> (setDate model <| MyTime.plusOneMoth timeNow, Effect.none)

        GotGroup res ->
            case res of
                Ok group -> let oldDebug = model.debug in ({ model | group = group, debug = (List.append oldDebug ["got-group"])}, Effect.none)
                Err err -> let oldDebug = model.debug in ({ model | debug = (List.append oldDebug ["err-group", getHttpErrorInfo err])}, Effect.none)

        DownloadFile -> (model, Effect.sendCmd <| File.Download.url <| getFileUrl model)



getFileUrl: Model -> String
getFileUrl model =
    "/dictionary/group/" ++ model.group.id ++ "/prediction-file/" ++ Iso8601.fromTime model.range.posix


setDate: Model -> Time.Posix -> Model
setDate model newTime  =
    case model.range.status of
        Nothing ->
            let
                oldRange = model.range
                newRange = { oldRange | posix = newTime, humanString = MyTime.timeToHumanReadable newTime}
            in
                { model | range = newRange}
        Just value ->
            let
                oldRange = model.range
                newRange = { oldRange | posix = newTime, humanString = MyTime.timeToHumanReadable newTime, status = Nothing}
            in
                { model | range = newRange}


setDateError: Model -> String -> Model
setDateError model humanInput =
    let
        oldRange = model.range
        newRange = { oldRange | humanString = humanInput, status = Just "--invalid input"}
    in
        { model | range = newRange}


requestToUpdateCustomValue: String -> Product -> Int -> Cmd Msg
requestToUpdateCustomValue groupId product updatedValue =
    Http.request { method = "PUT"
                 , headers = []
                 , url = "/dictionary/group/" ++ groupId ++ "/product/" ++ product.id ++ "/custom-value"
                 , body = Http.jsonBody <| Json.Encode.object [("custom-value", Json.Encode.int updatedValue)]
                 , expect = Http.expectWhatever GotResponseUpdateCustomValue
                 , timeout = Nothing
                 , tracker = Nothing
                 }


requestToGetCustomValue: String -> Product -> Cmd Msg
requestToGetCustomValue groupId product =
    Http.get { url = "/dictionary/group/" ++ groupId ++ "/product/" ++ product.id ++ "/custom-value"
             , expect = Http.expectJson GotCustomValue customValueDecoder
             }


customValueDecoder: Decoder CustomValue
customValueDecoder =
    map2 CustomValue
        (field "custom-value" int)
        (field "product-id" string)


getProductFromGroupRow: GroupRow -> Product
getProductFromGroupRow groupRow =
    groupRow.product


requestToAddProductToGroup: String -> String -> Cmd Msg
requestToAddProductToGroup groupId productId =
    Http.post { url = "/dictionary/group/" ++ groupId ++ "/product/" ++ productId
              , body = Http.emptyBody
              , expect = Http.expectWhatever GotResponseAddProduct
              }


requestToDeleteProductFromGroup: String -> String -> Cmd Msg
requestToDeleteProductFromGroup groupId productId =
    Http.request { method = "DELETE"
                 , headers = []
                 , url = "/dictionary/group/" ++ groupId ++ "/product/" ++ productId
                 , body = Http.emptyBody
                 , expect = Http.expectWhatever GotResponseDeleteProduct
                 , timeout = Nothing
                 , tracker = Nothing
                 }

doSearch: String -> Cmd Msg
doSearch str =
    Http.get
              { url = "/dictionary/product?product_name=" ++ str
              , expect = Http.expectJson GotProductsForSearch (Json.Decode.list productDecoder)
              }


setPrediction: Model -> Prediction -> Model
setPrediction model pred =
    let
        updatePrediction: GroupRow -> GroupRow
        updatePrediction group =
            if group.product.id == pred.productId then
                {group | prediction = Just pred}
            else
                group

        updateProductsInGroup: List GroupRow -> List GroupRow
        updateProductsInGroup groups =
            List.map updatePrediction groups

    in
        { model | productsInGroup = updateProductsInGroup model.productsInGroup }

setCustomValue: Model -> CustomValue -> Model
setCustomValue model newValue =
    let
        updateCustomValue: GroupRow -> GroupRow
        updateCustomValue group =
            if group.product.id == newValue.productId then
                {group | customValue = Just newValue}
            else
                group

        updateGroupRow: List GroupRow -> List GroupRow
        updateGroupRow listGroups =
            List.map updateCustomValue listGroups
    in
        { model | productsInGroup = updateGroupRow model.productsInGroup}



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    Components.Sidebar.view
        { title = "Pages.Group.GroupId_"
        , body = [ Html.h1 [] [ Html.text model.group.name] , drawPageBody model , Html.div [] <| List.map Html.text model.debug]
        }

drawPageBody: Model -> Html.Html Msg
drawPageBody model =
      Html.div [ Html.Attributes.class "grid"] [ Html.div [] [ Html.fieldset [role "group"] [ Html.input [ Html.Attributes.placeholder "type prediction date", Html.Attributes.value model.range.humanString, Html.Events.onInput ChangePredictionRange] []
                                                                                            , Html.button [ Html.Events.onClick DownloadFile ] [  Html.text "file" ]]
                                                             , Html.a [ Html.Attributes.class "pico-color-pink-450"] [ Html.text <| printDateError model ]  -- Html.Attributes.href <| getFileUrl model
                                                             , drawProductInGroup model ]
                                               , Html.div [] [ Html.input [ Html.Attributes.placeholder "type product name", Html.Attributes.value model.content, Html.Events.onInput Change] []
                                                             , drawSearchAndAddProduct model] ]






drawProductInGroup: Model -> Html.Html Msg
drawProductInGroup model =
     Html.table [] <| List.append [ Html.th [] [Html.text "name"]
                                  , Html.th [] [Html.text "prediction"]
                                  , Html.th [] [Html.text "custom value"]
                                  ] <| List.map productToRow model.productsInGroup



productToRow: GroupRow -> Html.Html Msg
productToRow groupRow =
    Html.tr []
        [ Html.td [] [ Html.a [Html.Attributes.href <| "/product-card/" ++ groupRow.product.id] [ Html.text groupRow.product.name] ]
        , Html.td [] [Html.text <| predictionToText groupRow.prediction]
        , Html.td [] [ Html.input [ Html.Attributes.value <| customValueToText groupRow.customValue, Html.Events.onInput (ChangeCustomValue groupRow.product) ] [] ]--Html.text <| customValueToText groupRow.customValue]
        , Html.td [] [ Html.span [ Html.Attributes.class "pico-color-pink-450", Html.Events.onClick <| DeleteProductFromGroup groupRow.product.id ] [ Html.text "x"] ]
        ] --todo add delete product from group


predictionToText: Maybe Prediction -> String
predictionToText prediction =
    case prediction of
        Nothing -> "-"
        Just pred -> String.fromFloat pred.value


customValueToText: Maybe CustomValue -> String
customValueToText value =
    case value of
        Nothing -> "-"
        Just a -> String.fromInt a.value


drawSearchAndAddProduct: Model -> Html.Html Msg
drawSearchAndAddProduct model =
    Html.table [ Html.Attributes.style "background" "lightblue" ] <| List.append [ Html.th [] [Html.text "name"]
                                 , Html.th [] [Html.text "id"]
                                 ] <| List.map productToRowForSearch model.productsInSearch


productToRowForSearch: Product -> Html.Html Msg
productToRowForSearch product =
    Html.tr []
        [ Html.td [] [ Html.span [ Html.Attributes.class "pico-color-green-200", Html.Events.onClick <| AddProductToGroup product.id, dataTooltip "Add product to group" ] [ Html.text "<<-"] ]
        , Html.td [] [ Html.text product.name ]
        ]

dataTooltip: String -> Html.Attribute msg
dataTooltip value =
    Html.Attributes.attribute "data-tooltip" value

prepareDateForPrint: Model -> String
prepareDateForPrint model =
    case model.range.status of
        Nothing -> model.range.humanString
        Just str -> model.range.humanString ++ "    " ++ str

printDateError: Model -> String
printDateError model =
    case model.range.status of
        Nothing -> ""
        Just value -> value

role: String -> Html.Attribute msg
role value =
    Html.Attributes.attribute "role" value


getHttpErrorInfo: Http.Error -> String
getHttpErrorInfo err =
    case err of
        Http.BadUrl string -> "bad url request error: " ++ string
        Http.Timeout -> "request error: Timeout"
        Http.NetworkError -> "request error: NetworkError"
        Http.BadStatus int -> "request error BadStatus: " ++ String.fromInt int
        Http.BadBody string -> "Bad body request error:" ++ string
