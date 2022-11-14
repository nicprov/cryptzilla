module Pages.Home_ exposing (Model, Msg, page)

import Html exposing (Html, a, button, div, hr, i, input, label, li, ol, span, text)
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
    { title = "File Manager"
    , body = [ viewMain
             ]
    }

viewMain: Html Msg
viewMain =
    div
        [ Attr.class "container flex-grow-1 light-style container-p-y"
        ]
        [ div
            [ Attr.class "container-m-nx container-m-ny bg-lightest mb-3"
            ]
            [ ol
                [ Attr.class "breadcrumb text-big container-p-x py-3 m-0"
                ]
                [ li
                    [ Attr.class "breadcrumb-item"
                    ]
                    [ a
                        [ Attr.href "#"
                        ]
                        [ text "home" ]
                    ]
                , li
                    [ Attr.class "breadcrumb-item"
                    ]
                    [ a
                        [ Attr.href "#"
                        ]
                        [ text "projects" ]
                    ]
                , li
                    [ Attr.class "breadcrumb-item active"
                    ]
                    [ text "site" ]
                ]
            , hr
                [ Attr.class "m-0"
                ]
                []
            , div
                [ Attr.class "file-manager-actions container-p-x py-2"
                ]
                [ div []
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-primary mr-2"
                        ]
                        [ i
                            [ Attr.class "ion ion-md-cloud-upload"
                            ]
                            []
                        , text " Upload" ]
                    , button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-secondary icon-btn mr-2"
                        , Attr.disabled True
                        ]
                        [ i
                            [ Attr.class "ion ion-md-cloud-download"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "btn-group mr-2"
                        ]
                        [ button
                            [ Attr.type_ "button"
                            , Attr.class "btn btn-default md-btn-flat dropdown-toggle px-2"
                            , Attr.attribute "data-toggle" "dropdown"
                            ]
                            [ i
                                [ Attr.class "ion ion-ios-settings"
                                ]
                                []
                            ]
                        , div
                            [ Attr.class "dropdown-menu"
                            ]
                            [ a
                                [ Attr.class "dropdown-item"
                                , Attr.href "#"
                                ]
                                [ text "Move" ]
                            , a
                                [ Attr.class "dropdown-item"
                                , Attr.href "#"
                                ]
                                [ text "Copy" ]
                            , a
                                [ Attr.class "dropdown-item"
                                , Attr.href "#"
                                ]
                                [ text "Remove" ]
                            ]
                        ]
                    ]
                , div []
                    [ div
                        [ Attr.class "btn-group btn-group-toggle"
                        , Attr.attribute "data-toggle" "buttons"
                        ]
                        [ label
                            [ Attr.class "btn btn-default icon-btn md-btn-flat active"
                            ]
                            [ input
                                [ Attr.type_ "radio"
                                , Attr.name "file-manager-view"
                                , Attr.value "file-manager-col-view"
                                , Attr.checked True
                                ]
                                []
                            , span
                                [ Attr.class "ion ion-md-apps"
                                ]
                                []
                            ]
                        , label
                            [ Attr.class "btn btn-default icon-btn md-btn-flat"
                            ]
                            [ input
                                [ Attr.type_ "radio"
                                , Attr.name "file-manager-view"
                                , Attr.value "file-manager-row-view"
                                ]
                                []
                            , span
                                [ Attr.class "ion ion-md-menu"
                                ]
                                []
                            ]
                        ]
                    ]
                ]
            , hr
                [ Attr.class "m-0"
                ]
                []
            ]
        , div
            [ Attr.class "file-manager-container file-manager-col-view"
            ]
            [ div
                [ Attr.class "file-manager-row-header"
                ]
                [ div
                    [ Attr.class "file-item-name pb-2"
                    ]
                    [ text "Filename" ]
                , div
                    [ Attr.class "file-item-changed pb-2"
                    ]
                    [ text "Changed" ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-icon file-item-level-up fas fa-level-up-alt text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text ".." ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-folder text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Images" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/13/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-folder text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Scripts" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/14/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-folder text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Utils" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/15/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-file-archive text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Archive.zip" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/16/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon fab fa-js text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Build.js" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/17/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-file-word text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Checklist.doc" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/18/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon fab fa-html5 text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Index.html" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/19/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Image-1.jpg" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/20/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Image-2.png" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/21/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Image-3.gif" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/22/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon fab fa-js text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Main.js" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/23/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-file text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "MAKEFILE" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/24/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-file-pdf text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Presentation.pdf" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/25/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-file-alt text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "README.txt" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/26/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon fab fa-css3 text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Style.css" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/27/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-file-audio text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Test.mp3" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "02/28/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            , div
                [ Attr.class "file-item"
                ]
                [ div
                    [ Attr.class "file-item-select-bg bg-primary"
                    ]
                    []
                , label
                    [ Attr.class "file-item-checkbox custom-control custom-checkbox"
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.class "custom-control-input"
                        ]
                        []
                    , span
                        [ Attr.class "custom-control-label"
                        ]
                        []
                    ]
                , div
                    [ Attr.class "file-item-icon far fa-file-video text-secondary"
                    ]
                    []
                , a
                    [ Attr.href "#"
                    , Attr.class "file-item-name"
                    ]
                    [ text "Tutorial.avi" ]
                , div
                    [ Attr.class "file-item-changed"
                    ]
                    [ text "03/01/2018" ]
                , div
                    [ Attr.class "file-item-actions btn-group"
                    ]
                    [ button
                        [ Attr.type_ "button"
                        , Attr.class "btn btn-default btn-sm rounded-pill icon-btn borderless md-btn-flat hide-arrow dropdown-toggle"
                        , Attr.attribute "data-toggle" "dropdown"
                        ]
                        [ i
                            [ Attr.class "ion ion-ios-more"
                            ]
                            []
                        ]
                    , div
                        [ Attr.class "dropdown-menu dropdown-menu-right"
                        ]
                        [ a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Rename" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Move" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Copy" ]
                        , a
                            [ Attr.class "dropdown-item"
                            , Attr.href "#"
                            ]
                            [ text "Remove" ]
                        ]
                    ]
                ]
            ]
        ]
