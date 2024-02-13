module Pages.Groups exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html.Attributes
import Route exposing (Route)
import Html
import Html.Events
import Page exposing (Page)
import Shared
import View exposing (View)
import Http
import Json.Decode exposing (Decoder, map3, map5, field, string, int, float)
import Json.Encode

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
    , Effect.sendCmd doSearchForGroups
    )

doSearchForGroups: Cmd Msg
doSearchForGroups  =
    Http.get { url = "/dictionary/group"
             , expect = Http.expectJson GotGroups <| Json.Decode.list groupDecoder
             }


groupDecoder: Decoder Group
groupDecoder =
    map3 Group
        (field "id" string)
        (field "name" string)
        (field "creation-date" string)


-- UPDATE


type Msg
    = GotGroups (Result Http.Error (List Group))
    | CreateNewGroup
    | GotResponseAddGroup (Result Http.Error ())
    | DeleteGroup String


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotGroups res ->
            case res of
                Ok listGroups -> ({ model | groups = Success listGroups}, Effect.none)
                Err error -> ({ model | groups = Failure error}, Effect.none)
        CreateNewGroup -> (model, Effect.sendCmd requestToCreateNewGroup)
        GotResponseAddGroup res -> (model, Effect.sendCmd doSearchForGroups)
        DeleteGroup groupId -> (model, Effect.sendCmd <| requestToDeleteGroup groupId)


requestToCreateNewGroup: Cmd Msg
requestToCreateNewGroup  =
    Http.post { url = "/dictionary/group"
              , body = Http.jsonBody <| Json.Encode.object [("name",Json.Encode.string "testName" )]
              , expect = Http.expectWhatever GotResponseAddGroup
              }


requestToDeleteGroup: String -> Cmd Msg
requestToDeleteGroup groupId=
    Http.request { method = "DELETE"
                 , headers = []
                 , url = "/dictionary/group/" ++ groupId
                 , body = Http.emptyBody
                 , expect = Http.expectWhatever GotResponseAddGroup
                 , timeout = Nothing
                 , tracker = Nothing
                 }

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    Components.Sidebar.view
        { title = "Pages.Group"
        , body = [ drawPageBody model ]
        }


drawPageBody: Model -> Html.Html Msg
drawPageBody model =
    case model.groups of
        Loading -> Html.text "it's loading, please wait"
        Failure err ->
            case err of
                Http.BadUrl _ -> Html.text ("BadUrl")
                Http.Timeout -> Html.text ("Timeout")
                Http.NetworkError -> Html.text ("NetWorkError")
                Http.BadStatus code -> Html.text ("Bad status " ++ String.fromInt code)
                Http.BadBody reason -> Html.text ("BadBody. " ++ reason)
        Success listGroups ->
            Html.div [] [ Html.div [] [ Html.button [ Html.Events.onClick CreateNewGroup ] [Html.text "add new group"]]
                        , Html.div [] [ drawGroupTable listGroups]
                        ]


drawGroupTable: (List Group) -> Html.Html Msg
drawGroupTable listGroups =
    Html.table [] <| List.append [ Html.th [] [ Html.text "name"]
                                 , Html.th [] [Html.text "id"]
                                 , Html.th [] [Html.text "creation date"]
                                 ] <| List.map groupToRow listGroups


groupToRow: Group -> Html.Html Msg
groupToRow group =
    Html.tr []
                [ Html.td [] [Html.a [Html.Attributes.href <| "/group/" ++ group.id] [ Html.text group.name]]
                , Html.td [] [Html.text group.id]
                , Html.td [] [Html.text group.creationDate]
                , Html.td [] [Html.button [ Html.Events.onClick <| DeleteGroup group.id] [Html.text "delete group"]] --todo complete "delete group" button
                ]
