module Pages.Authenticate exposing (Model, Msg, page)

import Common.Alert exposing (viewAlertError)
import Common.Footer exposing (viewFooter)
import Effect exposing (Effect)
import Gen.Route
import Hotkeys exposing (onKeyCode)
import Html exposing (Html, a, br, button, div, form, h1, hr, i, img, input, label, li, main_, ol, option, p, select, small, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import S3.Types
import List exposing (head)
import Page
import Request exposing (Request)
import Shared exposing (Msg(..))
import Storage exposing (signIn)
import View exposing (View)

page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init req shared
        , update = update shared req
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }

type Status
    = Error String
    | None

type alias Model =
    { account: S3.Types.Account
    , encryptionKeyHidden: Bool
    , status: Status
    }

init : Request -> Shared.Model -> (Model, Effect Msg)
init req shared =
    let
        tmpModel = { account = { name = ""
                               , region = Nothing
                               , customHost = Nothing
                               , accessKey = ""
                               , secretKey = ""
                               , buckets = [""]
                               }
                   , encryptionKeyHidden = True
                   , status = None
                   }
    in
    case shared.storage.account of
        Just acc ->
            ( { tmpModel | account = acc }, Effect.none )
        Nothing ->
            ( tmpModel, Effect.fromCmd (Request.replaceRoute Gen.Route.Login req) ) -- Redirect to login page if account is none

-- Update

type Msg
    = ChangeEncryptionKey String
    | ClickedDecrypt
    | ClickedLogout
    | ClickedHideEncryptionKey
    | PressedEnter

authenticate: Shared.Model -> Model -> (Model, Effect Msg)
authenticate shared model =
    if shared.encryptionKey == "" then
        ( { model | status = Error "Encryption key cannot be empty"}, Effect.none)
    else
        ( { model | status = Error "Invalid decryption key" } -- This will only show if the user isn't redirected to the home page
        , Effect.fromCmd (Storage.authenticate model.account shared.storage.password shared.storage.salt shared.storage)
        )

update: Shared.Model -> Request -> Msg -> Model -> (Model, Effect Msg)
update shared req msg model =
    case msg of
        ChangeEncryptionKey key ->
            ( model
            , Effect.fromShared (ChangeSharedEncryptionKey key)
            )

        ClickedDecrypt ->
            authenticate shared model

        ClickedHideEncryptionKey ->
            if model.encryptionKeyHidden then
                ( { model | encryptionKeyHidden = False }, Effect.none)
            else
                ( { model | encryptionKeyHidden = True }, Effect.none)

        PressedEnter ->
            authenticate shared model

        ClickedLogout ->
            ( model
            , Effect.fromCmd (Storage.signOut shared.storage)
            )

-- View

view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Cryptzilla | Authenticate"
    , body = [ viewMain shared model
             ]
    }

viewMain: Shared.Model -> Model -> Html Msg
viewMain shared model =
    div
        [ Attr.id "wrapper"
        ]
        [ div
            [ Attr.attribute "data-v" ""
            ]
            [
            div
                [ Attr.attribute "data-v" ""
                , Attr.id "login"
                , Attr.class "columns is-centered"
                ]
                [ div
                    [ Attr.attribute "data-v" ""
                    , Attr.class "column is-narrow"
                    ]
                    [ div
                        [ Attr.attribute "data-v" ""
                        , Attr.attribute "data-bitwarden-watching" "1"
                        ]
                        [ div
                            [ Attr.attribute "data-v" ""
                            , Attr.class "box"
                            ]
                            [ div
                                [ Attr.attribute "data-v" ""
                                , Attr.class "has-text-centered"
                                ]
                                [ img
                                    [ Attr.attribute "data-v" ""
                                    , Attr.src "/img/logo.png"
                                    , Attr.class "logo"
                                    ]
                                    []
                                ]
                            , br
                                [ Attr.attribute "data-v" ""
                                ]
                                []
                            , (case model.status of
                                    Error msg -> viewAlertError msg
                                    None -> div [] []
                                )
                            , div
                                [ Attr.attribute "data-v" ""
                                , Attr.class "field"
                                ]
                                [ label
                                    [ Attr.class "label"
                                    ]
                                    [ text "Encryption Key" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right is-clearfix"
                                    ]
                                    [ input
                                        [ if model.encryptionKeyHidden then
                                            Attr.type_ "password"
                                          else
                                            Attr.type_ "text"
                                        , Attr.class "input"
                                        , Attr.value shared.encryptionKey
                                        , onInput ChangeEncryptionKey
                                        , onKeyCode 13 PressedEnter
                                        ]
                                        []
                                    , small
                                        [ Attr.class "form-text text-muted"
                                        ]
                                        [ text "Used to locally encrypt configuration details (note: this is different than the rclone password)" ]
                                    , span
                                        [ Attr.class "icon is-right has-text-primary is-clickable"
                                        , onClick ClickedHideEncryptionKey
                                        ]
                                        [ i
                                            [ if model.encryptionKeyHidden then
                                                Attr.class "fas fa-eye fa-lg"
                                              else
                                                Attr.class "fas fa-eye-slash fa-lg"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            , div
                                [ Attr.attribute "data-v" ""
                                , Attr.class "is-flex is-justify-end"
                                ]
                                [ button
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "button is-primary"
                                    , Attr.style "margin-right" "10px"
                                    , onClick ClickedLogout
                                    ]
                                    [ text "Log out" ]
                                , button
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "button is-primary"
                                    , onClick ClickedDecrypt
                                    ]
                                    [ text "Decrypt local storage" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , viewFooter
        ]
