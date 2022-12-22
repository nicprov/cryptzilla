module Common.Alert exposing (viewAlertError, viewAlertSuccess, viewAlertInfo)

import Html exposing (Html, div, text)
import Html.Attributes as Attr

viewAlertSuccess: String -> Html msg
viewAlertSuccess msg =
    div
        [ Attr.class "alert alert-success col-md-12"
        , Attr.attribute "role" "alert"
        ]
        [ text msg ]

viewAlertInfo: String -> Html msg
viewAlertInfo error =
     div
        [ Attr.class "alert alert-primary col-md-12"
        , Attr.attribute "role" "alert"
        ]
        [ text error ]

viewAlertError: String -> Html msg
viewAlertError error =
     div
        [ Attr.class "alert alert-danger col-md-12"
        , Attr.attribute "role" "alert"
        ]
        [ text error ]