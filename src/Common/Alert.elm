module Common.Alert exposing (viewAlertError, viewAlertSuccess)

import Html exposing (Html, div, text)
import Html.Attributes as Attr

viewAlertSuccess: String -> Html msg
viewAlertSuccess msg =
    div
        [ Attr.class "alert alert-success col-md-12"
        , Attr.attribute "role" "alert"
        ]
        [ text msg ]

viewAlertError: String -> Html msg
viewAlertError error =
     div
        [ Attr.class "alert alert-danger col-md-12"
        , Attr.attribute "role" "alert"
        ]
        [ text error ]