module Pages.Home_ exposing (Model, Msg, page)

import Page exposing (Page)
import Components.Sidebar
import Browser
import File exposing (File)
import File.Select as Select
import View exposing (View)
import Html exposing (Attribute, Html, div, input, text, br, table, th, thead, tr, td, a, button)
import Html.Attributes exposing (attribute, placeholder, value, href, type_)
import Html.Events exposing (onInput, on, onClick)
import Http
import Json.Decode as D


page : Page Model Msg
page =
    Page.element
            { init = init
            , update = update
            , subscriptions = subscriptions
            , view = view
            }

    --Components.Sidebar.view
    --    { title = "Homepage"
    --    , body = [ Html.text "Hello, world!" ]
    --    }


--INIT

type UploadResult
    = Failure Http.Error
    | Success

type alias Model =
    { chosenFile: List File
    , uploadStatus: Maybe UploadResult}


init : (Model, Cmd Msg)
init =
    ({ chosenFile = []
     , uploadStatus = Nothing}, Cmd.none)


--UPDATE


type  Msg
 = GotFiles (List File)
 | ReciveUploadRespose (Result Http.Error ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotFiles files ->
            ({model | chosenFile = files}, uploadFiles files ReciveUploadRespose)

        ReciveUploadRespose result ->
            case result of
                Ok _ ->
                    ({model | uploadStatus = Just Success}, Cmd.none)
                Err error ->
                    ({model | uploadStatus = Just <| Failure error}, Cmd.none)




uploadFiles: (List File) -> (Result Http.Error () -> Msg) -> Cmd Msg
uploadFiles files msg =
    List.map (uploadFile msg) files
    |> Cmd.batch


uploadFile: (Result Http.Error () -> Msg) -> File -> Cmd Msg
uploadFile msg file =
    let
        url = "/dictionary/sale/file/upload" --todo end this
    in
    Http.request
        { method = "POST"
        , url = url
        , body = Http.multipartBody [Http.filePart "file" file]  --todo read about and end this
        , expect = Http.expectWhatever msg
        , headers = []
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
                { title = "Home"
                , body = [
                            div []
                            [ Html.text "Hello, world!"
                            , input [type_ "file", on "change" (D.map GotFiles filesDecoder)] [Html.text "uploadFile"]
                            , Html.text <| uploadErrorHandler model
                            ]

                         ]
                }

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)

uploadErrorHandler: Model -> String
uploadErrorHandler model =
    case model.uploadStatus of
        Nothing -> ""
        Just Success -> "success upload"
        Just (Failure error) ->
            case error of
                Http.BadUrl string -> "file upload request error: " ++ string
                Http.Timeout -> "file upload request error: Timeout"
                Http.NetworkError -> "file upload request error: NetworkError"
                Http.BadStatus int -> "file upload request error BadStatus: " ++ String.fromInt int
                Http.BadBody string -> "file upload request error:" ++ string