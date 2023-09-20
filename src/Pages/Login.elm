module Pages.Login exposing (Model, Msg, page)

import Common.Alert exposing (viewAlertError)
import Common.Footer exposing (viewFooter)
import File exposing (File)
import File.Select as Select
import Gen.Route
import Html exposing (Html, a, br, button, div, footer, form, h1, hr, i, img, input, label, li, main_, ol, option, p, section, select, small, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
import S3.Types
import List exposing (head)
import Page
import Request exposing (Request)
import Shared
import Storage exposing (signIn)
import Task
import View exposing (View)

page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
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
    , urlModal: Bool
    , configUrl: String
    }

init : (Model, Cmd Msg)
init =
    ( { account = { name = "S3"
                  , region = Nothing
                  , customHost = Nothing
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
      , urlModal = False
      , configUrl = ""
      }
    , Cmd.none )

-- Update

type Msg
    = ChangeRegion String
    | ChangeCustomHost String
    | ChangeAccessKey String
    | ChangeSecretKey String
    | ChangeBucket String
    | ChangeRclonePassword String
    | ChangeRcloneSalt String
    | ChangeEncryptionKey String
    | ChangedConfigURL String
    | ClickedSignIn
    | ClickedHideSecretKey
    | ClickedHideEncryptionKey
    | ClickedHideRcloneSalt
    | ClickedHideRclonePassword
    | ClickedFetchURLConfig
    | ClickedCancelURLModal
    | ClickedLoadConfigurationFile
    | ClickedFetchConfigurationFileFromURL
    | ConfigurationFileSelected File
    | FileDecodedToString String
    | GotConfigFromURL (Result Http.Error String)

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

getConfig: String -> Cmd Msg
getConfig url =
    Http.get
        { url = url
        , expect = Http.expectString GotConfigFromURL
        }

update: Shared.Model -> Request -> Msg -> Model -> (Model, Cmd Msg)
update shared _ msg model =
    case msg of
        ChangeRegion region ->
            let
                oldAccount = model.account
                newAccount = { oldAccount | region = Just region }
            in
            ( { model | account = newAccount }, Cmd.none)

        ChangeCustomHost customHost ->
            let
                oldAccount = model.account
                newAccount = { oldAccount | customHost = Just customHost }
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

        ClickedSignIn ->
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

        ClickedLoadConfigurationFile ->
            ( model
            , Select.file ["applicaiton/json"] ConfigurationFileSelected
            )

        ConfigurationFileSelected file ->
            ( model
            , Task.perform FileDecodedToString (File.toString file)
            )

        FileDecodedToString string ->
            let
                config = Json.Decode.decodeString Storage.storageDecoder string
            in
            case config of
                Ok c ->
                    case c.account of
                        Just acc ->
                            ( model
                            , Storage.signIn acc c.password c.salt c.encryptionKey shared.storage
                            )

                        Nothing ->
                            ( { model | status = Error "Unable to decode config file" }, Cmd.none)

                Err _ ->
                    ( { model | status = Error "Unable to decode config file" }, Cmd.none)

        ClickedFetchConfigurationFileFromURL ->
            ( { model | urlModal = True }, Cmd.none )

        GotConfigFromURL result ->
            case result of
                Ok config ->
                    ( model
                    , Task.perform (always (FileDecodedToString config)) (Task.succeed ())
                    )

                Err _ ->
                    ( { model | status = Error "Unable to fetch config file" }, Cmd.none)


        ChangedConfigURL url ->
            ( { model | configUrl = url }, Cmd.none )

        ClickedFetchURLConfig ->
            ( model, getConfig model.configUrl )

        ClickedCancelURLModal ->
            ( { model | urlModal = False }, Cmd.none )




-- View

view : Shared.Model -> Model -> View Msg
view _ model =
    { title = "Cryptzilla | Login"
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
                                    [ text "Custom Host" ]
                                , div
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "control has-icons-right"
                                    ]
                                    [ input
                                        [ Attr.type_ "text"
                                        , Attr.class "input"
                                        , Attr.value ( case model.account.customHost of
                                                     Just host -> host
                                                     Nothing -> ""
                                                 )
                                             , onInput ChangeCustomHost
                                        ]
                                        []
                                    , small
                                        [ Attr.class "form-text text-muted"
                                        ]
                                        [ text "Indicate whether to use a custom host" ]
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
                                    , onClick ClickedFetchConfigurationFileFromURL
                                    ]
                                    [ text "Fetch configuration file from URL" ]
                                , button
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "button is-primary"
                                    , Attr.style "margin-right" "10px"
                                    , onClick ClickedLoadConfigurationFile
                                    ]
                                    [ text "Load configuration file" ]
                                , button
                                    [ Attr.attribute "data-v" ""
                                    , Attr.class "button is-primary"
                                    , onClick ClickedSignIn
                                    ]
                                    [ text "Log in" ]
                                ]
                            , if model.urlModal then
                                viewConfigURLModal model
                              else
                                div [] []
                            ]
                        ]
                    ]
                ]
            ]
        , viewFooter
        ]

viewConfigURLModal: Model -> Html Msg
viewConfigURLModal model =
    div
    [ Attr.class "dialog modal is-active"
    ]
    [ div
        [ Attr.class "modal-background"
        ]
        []
    , div
        [ Attr.class "modal-card animation-content"
        ]
        [
        section
            [ Attr.class "modal-card-body is-titleless"
            ]
            [ div
                [ Attr.class "media"
                ]
                [
                div
                    [ Attr.class "media-content"
                    ]
                    [ p []
                        []
                    , (case model.status of
                            Error msg -> viewAlertError msg
                            None -> div [] []
                        )
                    , div
                        [ Attr.class "field"
                        ]
                        [ div
                            [ Attr.class "control"
                            ]
                            [ input
                                [ Attr.placeholder "Config URL"
                                , Attr.class "input"
                                , onInput ChangedConfigURL
                                ]
                                []
                            ]
                        , p
                            [ Attr.class "help is-danger"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , footer
            [ Attr.class "modal-card-foot"
            ]
            [ button
                [ Attr.class "button"
                , onClick ClickedCancelURLModal
                ]
                [ text "Cancel" ]
            , button
                [ Attr.class "button is-primary"
                , onClick ClickedFetchURLConfig
                ]
                [ text "Fetch" ]
            ]
        ]
    ]
