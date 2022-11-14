module Pages.Login exposing (Model, Msg, page)

import Html exposing (Html, a, button, div, form, h1, hr, i, img, input, label, li, main_, ol, p, span, text)
import Html.Attributes as Attr
import Page
import Request exposing (Request)
import Shared
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update shared
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }

type alias Model =
    {
    }

init : (Model, Cmd Msg)
init =
    (Model, Cmd.none)

-- Update

type Msg
    = None

update: Shared.Model -> Msg -> Model -> (Model, Cmd Msg)
update _ msg model =
    case msg of
        None ->
            (model, Cmd.none)

-- View

view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Login"
    , body = [ viewMain
             ]
    }

viewMain: Html Msg
viewMain =
    form
        [ Attr.class "form-signin"
        ]
        [ h1
            [ Attr.class "h3 mb-3 font-weight-normal"
            ]
            [ text "Please sign in" ]
        , label
            [ Attr.for "inputEmail"
            , Attr.class "sr-only"
            ]
            [ text "Email address" ]
        , input
            [ Attr.type_ "email"
            , Attr.id "inputEmail"
            , Attr.class "form-control"
            , Attr.placeholder "Email address"
            , Attr.required True
            , Attr.autofocus True
            ]
            []
        , label
            [ Attr.for "inputPassword"
            , Attr.class "sr-only"
            ]
            [ text "Password" ]
        , input
            [ Attr.type_ "password"
            , Attr.id "inputPassword"
            , Attr.class "form-control"
            , Attr.placeholder "Password"
            , Attr.required True
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
                , text "Remember me" ]
            ]
        , button
            [ Attr.class "btn btn-lg btn-primary btn-block"
            , Attr.type_ "submit"
            ]
            [ text "Sign in" ]
        , p
            [ Attr.class "mt-5 mb-3 text-muted"
            ]
            [ text "© 2017-2020" ]
        ]

