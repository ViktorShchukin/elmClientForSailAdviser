module Pages.Groups exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Attributes
import Route exposing (Route)
import Html
import Page exposing (Page)
import Shared
import View exposing (View)
import Http
import Json.Decode exposing (Decoder, map3, map5, field, string, int, float)

import Components.Sidebar


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT

type alias Group =
    { id: String
    , name: String
    , creationDate: String}

type  SearchResult
  = Failure Http.Error
  | Loading
  | Success (List Group)

type alias Model =
    { groups: SearchResult}


init : () -> ( Model, Effect Msg )
init () =
    ( {groups = Loading}
    , Effect.sendCmd <| Http.get
        { url = "/dictionary/group"
        , expect = Http.expectJson GotGroups <| Json.Decode.list groupDecoder}
    )

groupDecoder: Decoder Group
groupDecoder =
    map3 Group
        (field "id" string)
        (field "name" string)
        (field "creation_date" string)


-- UPDATE


type Msg
    = GotGroups (Result Http.Error (List Group))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotGroups res ->
            case res of
                Ok listGroups -> ({ model | groups = Success listGroups}, Effect.none)
                Err error -> ({ model | groups = Failure error}, Effect.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    Components.Sidebar.view
        { title = "Pages.Group"
        , body = [ drowPageBody model ]
        }

drowPageBody: Model -> Html.Html Msg
drowPageBody model =
    case model.groups of
        Loading -> Html.text "it's loading, please waite"
        Failure err ->
            case err of
                Http.BadUrl _ -> Html.text ("BadUrl")
                Http.Timeout -> Html.text ("Timeout")
                Http.NetworkError -> Html.text ("NetWorkError")
                Http.BadStatus code -> Html.text ("Bad status " ++ String.fromInt code)
                Http.BadBody reason -> Html.text ("BadBody. " ++ reason)
        Success listGroups ->
            Html.div [] [ Html.div [] [Html.button [] [Html.text "add new group"]]
                        , Html.div [] [Html.table [] <| List.append [ Html.th [] [ Html.text "name"]
                                                                                , Html.th [] [Html.text "id"]
                                                                                , Html.th [] [Html.text "creation date"]
                                                                                ] <| List.map groupToRow listGroups
                                      ]
                        ]


groupToRow: Group -> Html.Html Msg
groupToRow group =
    Html.tr []
                [ Html.td [] [Html.a [Html.Attributes.href <| "/group/" ++ group.id] [ Html.text group.name]]
                , Html.td [] [Html.text group.id]
                , Html.td [] [Html.text group.creationDate]
                , Html.td [] [Html.button [] [Html.text "delete group"]] --todo delete group button
                ]
