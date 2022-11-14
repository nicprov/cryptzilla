module Pages.Login exposing (Model, Msg, page)

import Domain.Credentials exposing (Credentials)
import Common.Alert exposing (viewAlertError)
import Gen.Route
import Html exposing (Html, a, button, div, form, h1, hr, i, img, input, label, li, main_, ol, p, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import Page
import Request exposing (Request)
import Shared
import Storage exposing (signIn)
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
    { accessKey: String
    , endpoint: String
    , secret: String
    , status: Status
    }

init : (Model, Cmd Msg)
init =
    (Model "" "" "" None, Cmd.none)

-- Update

type Msg
    = ChangeAccessKey String
    | ChangeEndoint String
    | ChangeSecret String
    | ClickedSignIn

update: Shared.Model -> Request -> Msg -> Model -> (Model, Cmd Msg)
update shared req msg model =
    case msg of
        ChangeAccessKey accessKey ->
            ( { model | accessKey = accessKey }, Cmd.none)

        ChangeEndoint endpoint ->
            ( { model | endpoint = endpoint }, Cmd.none)

        ChangeSecret secret ->
            ( { model | secret = secret }, Cmd.none)

        ClickedSignIn ->
            if model.accessKey == "" then
                ( { model | status = Error "Access Key cannot be empty"}, Cmd.none)
            else if model.endpoint == "" then
                ( { model | status = Error "Endpoint cannot be empty"}, Cmd.none)
            else if model.secret == "" then
                ( { model | status = Error "Secret cannot be empty"}, Cmd.none)
            else
                (model, Cmd.batch [ Storage.signIn (Credentials model.accessKey model.endpoint model.secret) shared.storage
                                  ,  Request.replaceRoute Gen.Route.Home_ req
                                  ]
                )

-- View

view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Login"
    , body = [ viewMain model
             ]
    }

viewMain: Model -> Html Msg
viewMain model =
    div
        [ Attr.class "form-signin"
        ]
        [ h1
            [ Attr.class "h3 mb-3 font-weight-normal"
            ]
            [ text "Please sign in" ]
        , (case model.status of
            Error msg -> viewAlertError msg
            None -> div [] []
        )
        , label
            [ Attr.class "sr-only"
            ]
            [ text "Access Key" ]
        , input
            [ Attr.class "form-control"
            , Attr.placeholder "Access Key"
            , Attr.required True
            , Attr.autofocus True
            , Attr.value model.accessKey
            , onInput ChangeAccessKey
            ]
            []
        , label
            [ Attr.class "sr-only"
            ]
            [ text "Endpoint" ]
        , input
            [ Attr.class "form-control"
            , Attr.placeholder "Endpoint"
            , Attr.required True
            , Attr.autofocus True
            , Attr.value model.endpoint
            , onInput ChangeEndoint
            ]
            []
        , label
            [ Attr.class "sr-only"
            ]
            [ text "Secret" ]
        , input
            [ Attr.type_ "password"
            , Attr.class "form-control"
            , Attr.placeholder "Secret"
            , Attr.required True
            , Attr.value model.secret
            , onInput ChangeSecret
            ]
            []
        , div
            [ Attr.class "checkbox mb-3"
            ]
            [ label []
                [ input
                    [ Attr.type_ "checkbox"
                    , Attr.value "remember-me"
                    ]
                    []
                , text " Remember me" ]
            ]
        , button
            [ Attr.class "btn btn-lg btn-primary btn-block"
            , Attr.type_ "submit"
            , onClick ClickedSignIn
            ]
            [ text "Sign in" ]
        , p
            [ Attr.class "mt-5 mb-3 text-muted"
            ]
            [ text "© 2022" ]
        ]


