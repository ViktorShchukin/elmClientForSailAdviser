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


type alias Model = List File


init : (Model, Cmd Msg)
init =
    ([], Cmd.none)


--UPDATE


type  Msg
 = GotFiles (List File)
 | ReciveUploadRespose (Result Http.Error ())


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ReciveUploadRespose result ->
            case result of
                Ok _ ->
                    (model, Cmd.none)
                Err _ ->
                    (model, Cmd.none)
        GotFiles files ->
            (files, uploadFiles files ReciveUploadRespose)


uploadFiles: (List File) -> (Result Http.Error () -> Msg) -> Cmd Msg
uploadFiles files msg =
    List.map (uploadFile msg) files
    |> Cmd.batch


uploadFile: (Result Http.Error () -> Msg) -> File -> Cmd Msg
uploadFile msg file =
    let
        url = "" --todo end this
    in
    Http.request
        { method = "Post"
        , url = url
        , body = Http.fileBody file --todo read about and end this
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
                            ]

                         ]
                }

filesDecoder : D.Decoder (List File)
filesDecoder =
  D.at ["target","files"] (D.list File.decoder)