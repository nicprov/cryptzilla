module Pages.Settings exposing (Model, Msg, page)

import Common.Alert exposing (viewAlertError)
import File.Download as Download
import Gen.Route
import Html exposing (Html, a, br, button, div, form, h1, hr, i, img, input, label, li, main_, ol, option, p, select, small, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Json.Encode exposing (encode)
import S3.Types
import List exposing (head)
import Page
import Request exposing (Request)
import Shared
import Storage exposing (signIn)
import View exposing (View)
import Crypto.Strings exposing (decrypt, encrypt)
import Random exposing (Seed, initialSeed)

page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared
        , update = update shared req
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }

type Status
    = Error String
    | None

type alias Model =
    { account: S3.Types.Account
    , status: Status
    , rclonePassword: String
    , rcloneSalt: String
    , encryptionKey: String
    , secretKeyHidden: Bool
    , rclonePasswordHidden: Bool
    , rcloneSaltHidden: Bool
    , encryptionKeyHidden: Bool
    }

init : Shared.Model -> (Model, Cmd Msg)
init shared =
    case shared.storage.account of
        Just acc ->
            ( { account = { name = "S3"
                          , region = acc.region
                          , isDigitalOcean = acc.isDigitalOcean
                          , accessKey = acc.accessKey
                          , secretKey = acc.secretKey
                          , buckets = acc.buckets
                          }
              , status = None
              , rclonePassword = shared.storage.password
              , rcloneSalt = shared.storage.salt
              , encryptionKey = shared.storage.encryptionKey
              , secretKeyHidden = True
              , rclonePasswordHidden = True
              , rcloneSaltHidden = True
              , encryptionKeyHidden = True
              }
            , Cmd.none )
        Nothing ->
            ( { account = { name = ""
                          , region = Just ""
                          , isDigitalOcean = False
                          , accessKey = ""
                          , secretKey = ""
                          , buckets = [""]
                          }
              , status = None
              , rclonePassword = ""
              , rcloneSalt = ""
              , encryptionKey = ""
              , secretKeyHidden = True
              , rclonePasswordHidden = True
              , rcloneSaltHidden = True
              , encryptionKeyHidden = True
              }
            , Cmd.none )

-- Update

type Msg
    = ChangeRegion String
    | ChangeIsDigitalOcean String
    | ChangeAccessKey String
    | ChangeSecretKey String
    | ChangeBucket String
    | ChangeRclonePassword String
    | ChangeRcloneSalt String
    | ChangeEncryptionKey String
    | ClickedSave
    | ClickedHideSecretKey
    | ClickedHideEncryptionKey
    | ClickedHideRcloneSalt
    | ClickedHideRclonePassword
    | ClickedHome
    | ClickedDownloadConfig

stringFromBool : Bool -> String
stringFromBool value =
  if value then
    "True"
  else
    "False"

boolFromString : String -> Bool
boolFromString value =
  if value == "True" then
    True
  else
    False

update: Shared.Model -> Request -> Msg -> Model -> (Model, Cmd Msg)
update shared req msg model =
    case msg of
        ChangeRegion region ->
            let
                oldAccount = model.account
                newAccount = { oldAccount | region = Just region }
            in
            ( { model | account = newAccount }, Cmd.none)

        ChangeIsDigitalOcean isDigitalOcean ->
            let
                oldAccount = model.account
                newAccount = { oldAccount | isDigitalOcean = (boolFromString isDigitalOcean) }
            in
            ( { model | account = newAccount }, Cmd.none)

        ChangeAccessKey accessKey ->
            let
                oldAccount = model.account
                newAccount = { oldAccount | accessKey = accessKey }
            in
            ( { model | account = newAccount }, Cmd.none)

        ChangeBucket bucket ->
            let
                oldAccount = model.account
                newAccount = { oldAccount | buckets = [bucket] }
            in
            ( { model | account = newAccount }, Cmd.none)

        ChangeSecretKey secretKey ->
            let
                oldAccount = model.account
                newAccount = { oldAccount | secretKey = secretKey }
            in
            ( { model | account = newAccount }, Cmd.none)

        ChangeRclonePassword password ->
            ( { model | rclonePassword = password }, Cmd.none)

        ChangeRcloneSalt salt ->
            ( { model | rcloneSalt = salt }, Cmd.none)

        ChangeEncryptionKey key ->
            ( { model | encryptionKey = key }, Cmd.none)

        ClickedSave ->
            if model.account.accessKey == "" then
                ( { model | status = Error "Access Key cannot be empty"}, Cmd.none)
            else if model.account.secretKey == "" then
                ( { model | status = Error "Endpoint cannot be empty"}, Cmd.none)
            else if model.account.buckets == [] then
                ( { model | status = Error "Bucket cannot be empty"}, Cmd.none)
            else if model.rclonePassword == "" then
                ( { model | status = Error "Rclone password cannot be empty"}, Cmd.none)
            else if model.encryptionKey == "" then
            ( { model | status = Error "Encryption key cannot be empty"}, Cmd.none)
            else
                ( model
                , Storage.signIn model.account model.rclonePassword model.rcloneSalt model.encryptionKey shared.storage
                )

        ClickedHideSecretKey ->
            if model.secretKeyHidden then
                ( { model | secretKeyHidden = False }, Cmd.none)
            else
                ( { model | secretKeyHidden = True }, Cmd.none)

        ClickedHideEncryptionKey ->
            if model.encryptionKeyHidden then
                ( { model | encryptionKeyHidden = False }, Cmd.none)
            else
                ( { model | encryptionKeyHidden = True }, Cmd.none)

        ClickedHideRclonePassword ->
            if model.rclonePasswordHidden then
                ( { model | rclonePasswordHidden = False }, Cmd.none)
            else
                ( { model | rclonePasswordHidden = True }, Cmd.none)

        ClickedHideRcloneSalt ->
            if model.rcloneSaltHidden then
                ( { model | rcloneSaltHidden = False }, Cmd.none)
            else
                ( { model | rcloneSaltHidden = True }, Cmd.none)

        ClickedHome ->
            ( model, Request.replaceRoute Gen.Route.Home_ req )

        ClickedDownloadConfig ->
            ( model
            , Download.string "config.json" "application/json" (encode 0 (Storage.storageToJson shared.storage))
            )


-- View

view : Shared.Model -> Model -> View Msg
view _ model =
    { title = "Cryptzilla | Settings"
    , body = [ viewMain model
             ]
    }

viewMain: Model -> Html Msg
viewMain model =
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
                                [ a [ Attr.href "#"
                                    ]
                                    [ img
                                        [ Attr.attribute "data-v" ""
                                        , Attr.src "/img/logo.png"
                                        , Attr.class "logo"
                                        , onClick ClickedHome
                                        ]
                                        []
                                    ]
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
                                    [ text "Region" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right"
                                    ]
                                    [ input
                                        [ Attr.type_ "text"
                                        , Attr.class "input"
                                        , Attr.value ( case model.account.region of
                                                     Just region -> region
                                                     Nothing -> ""
                                                 )
                                             , onInput ChangeRegion
                                        ]
                                        []
                                    , small
                                        [ Attr.class "form-text text-muted"
                                        ]
                                        [ text "S3 region used (ie. us-west-1 for AWS, or nyc3 for Digital Ocean)" ]
                                    ]
                                ]
                            , div
                                [ Attr.attribute "data-v" ""
                                , Attr.class "field"
                                ]
                                [ label
                                    [ Attr.class "label"
                                    ]
                                    [ text "Is Digital Ocean" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right"
                                    ]
                                    [ select
                                        [ Attr.class "form-control"
                                        , Attr.id "exampleFormControlSelect1"
                                        , Attr.value (stringFromBool model.account.isDigitalOcean)
                                        , onInput ChangeIsDigitalOcean
                                        ]
                                        [ option []
                                            [ text "False" ]
                                        , option []
                                            [ text "True" ]
                                        ]
                                    , small
                                        [ Attr.class "form-text text-muted"
                                        ]
                                        [ text "Indicate whether the bucket is hosted on AWS or Digital Ocean" ]
                                    ]
                                ]
                            , div
                                [ Attr.attribute "data-v" ""
                                , Attr.class "field"
                                ]
                                [ label
                                    [ Attr.class "label"
                                    ]
                                    [ text "Bucket Name" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right"
                                    ]
                                    [ input
                                        [ Attr.type_ "text"
                                        , Attr.class "input"
                                        , Attr.value (case (head model.account.buckets) of
                                            Just item -> item
                                            Nothing -> ""
                                            )
                                        , onInput ChangeBucket
                                        ]
                                        []
                                    ]
                                ]
                            , div
                                [ Attr.attribute "data-v" ""
                                , Attr.class "field"
                                ]
                                [ label
                                    [ Attr.class "label"
                                    ]
                                    [ text "Access Key" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right"
                                    ]
                                    [ input
                                        [ Attr.type_ "text"
                                        , Attr.class "input"
                                        , Attr.value model.account.accessKey
                                        , onInput ChangeAccessKey
                                        ]
                                        []
                                    ]
                                ]
                            , div
                                [ Attr.attribute "data-v" ""
                                , Attr.class "field"
                                ]
                                [ label
                                    [ Attr.class "label"
                                    ]
                                    [ text "Secret Key" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right is-clearfix"
                                    ]
                                    [ input
                                        [ if model.secretKeyHidden then
                                            Attr.type_ "password"
                                          else
                                            Attr.type_ "text"
                                        , Attr.class "input"
                                        , Attr.value model.account.secretKey
                                        , onInput ChangeSecretKey
                                        ]
                                        []
                                    ,
                                    span
                                        [ Attr.class "icon is-right has-text-primary is-clickable"
                                        , onClick ClickedHideSecretKey
                                        ]
                                        [ i
                                            [ if model.secretKeyHidden then
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
                                , Attr.class "field"
                                ]
                                [ label
                                    [ Attr.class "label"
                                    ]
                                    [ text "Rclone Password" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right is-clearfix"
                                    ]
                                    [ input
                                        [ if model.rclonePasswordHidden then
                                            Attr.type_ "password"
                                          else
                                            Attr.type_ "text"
                                        , Attr.class "input"
                                        , Attr.value model.rclonePassword
                                        , onInput ChangeRclonePassword
                                        ]
                                        []
                                    , small
                                        [ Attr.class "form-text text-muted"
                                        ]
                                        [ text "Must be the password found in the rclone config file because it is padded" ]
                                    , span
                                        [ Attr.class "icon is-right has-text-primary is-clickable"
                                        , onClick ClickedHideRclonePassword
                                        ]
                                        [ i
                                            [if model.rclonePasswordHidden then
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
                                , Attr.class "field"
                                ]
                                [ label
                                    [ Attr.class "label"
                                    ]
                                    [ text "Rclone Salt" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right is-clearfix"
                                    ]
                                    [ input
                                        [ if model.rcloneSaltHidden then
                                            Attr.type_ "password"
                                          else
                                            Attr.type_ "text"
                                        , Attr.class "input"
                                        , Attr.value model.rcloneSalt
                                        , onInput ChangeRcloneSalt
                                        ]
                                        []
                                    , small
                                        [ Attr.class "form-text text-muted"
                                        ]
                                        [ text "Optional (leave blank for empty), but must be the salt found in the rclone config file (named Password2) because it is padded" ]
                                    , span
                                        [ Attr.class "icon is-right has-text-primary is-clickable"
                                        , onClick ClickedHideRcloneSalt
                                        ]
                                        [ i
                                            [ if model.rcloneSaltHidden then
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
                                        , Attr.value model.encryptionKey
                                        , onInput ChangeEncryptionKey
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
                                    , onClick ClickedDownloadConfig
                                    ]
                                    [ text "Download Config" ]
                                , button
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "button is-primary"
                                    , onClick ClickedSave
                                    ]
                                    [ text "Save" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
