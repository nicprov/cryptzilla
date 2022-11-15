module Pages.Home_ exposing (Model, Msg, page)

import Html exposing (Html, a, button, div, hr, i, input, label, li, ol, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import List exposing (head)
import Page
import Request exposing (Request)
import S3
import S3.Types exposing (Error, KeyList, QueryElement(..))
import Shared
import Task
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
    { display: String
    , keyList: Maybe KeyList
    , currentDir: String
    }

init : (Model, Cmd Msg)
init =
    (Model "" Nothing "", Cmd.none)

-- Update

type Msg
    = ReceiveListBucket (Result Error KeyList)
    | ListBucket
    | ClickFolder String
    | ClickedBack

listBucket : S3.Types.Account -> Cmd Msg
listBucket account =
    let
        bucket = (case (head account.buckets) of
            Just b -> b
            Nothing -> ""
            )
    in
    S3.listKeys bucket
        |> S3.addQuery [ MaxKeys 100 ]
        |> S3.send account
        |> Task.attempt ReceiveListBucket

update: Shared.Model -> Msg -> Model -> (Model, Cmd Msg)
update shared msg model =
    case msg of

        ListBucket ->
            case shared.storage.account of
                Just acc ->
                    ( { model | display = "Getting bucket listing..." }
                    , listBucket acc
                    )
                Nothing -> (model, Cmd.none)

        ReceiveListBucket result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok keys ->
                    ( { model
                        | display = "Bucket listing received."
                        , keyList = Just keys
                      }
                    , Cmd.none
                    )

        ClickFolder folder ->
            ( { model | currentDir = folder }, Cmd.none)


        ClickedBack ->
            let
                tempList = List.drop 2 (List.reverse (String.split "/" model.currentDir))
                newDirList =  List.map (\m -> m ++ "/") tempList
            in
            ( { model | currentDir = String.concat newDirList }, Cmd.none)

-- View

view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "File Manager"
    , body = [ viewMain model
             ]
    }

viewMain: Model -> Html Msg
viewMain model =
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
                        , onClick ListBucket
                        ]
                        [ i
                            [ Attr.class "ion ion-md-cloud-upload"
                            ]
                            []
                        , text " Fetch" ]
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
            (List.append viewBack
                (case model.keyList of
                    Just keyList -> List.map (viewItem model) keyList.keys
                    Nothing -> []
                )
            )
        ]

viewItem: Model -> S3.Types.KeyInfo -> Html Msg
viewItem model key =
    if String.contains model.currentDir key.key then
        let
            name = String.replace model.currentDir "" key.key
            file = String.split "/" name
        in
        if name /= "" then
            if (List.length file) == 1 then
                viewFile model key
            else if (List.length file) == 2 then
                case List.head (List.reverse file) of
                    Just element ->
                        if element == "" then
                            viewFolder model key
                        else
                            div [] []
                    Nothing ->
                        div [] []
            else
                div [] []
        else
            div [] []
    else
        div [] []

viewFile: Model -> S3.Types.KeyInfo -> Html Msg
viewFile model key =
    let
        name = String.replace model.currentDir "" key.key
    in
    div
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
            [ Attr.class "file-item-icon far fa-file text-secondary" ]
            []
        , a
            [ Attr.href "#"
            , Attr.class "file-item-name"
            ]
            [ text name ]
        , div
            [ Attr.class "file-item-changed"
            ]
            [ text key.lastModified ]
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

viewFolder: Model -> S3.Types.KeyInfo -> Html Msg
viewFolder model key =
    let
        name = String.replace model.currentDir "" key.key
    in
    div
        [ Attr.class "file-item"
        , onClick (ClickFolder key.key)
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
            [ Attr.class "file-item-icon far fa-folder text-secondary" ]
            []
        , a
            [ Attr.href "#"
            , Attr.class "file-item-name"
            ]
            [ text (String.left ((String.length name) - 1) name) ]
        , div
            [ Attr.class "file-item-changed"
            ]
            [ text key.lastModified ]
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

viewBack: List (Html Msg)
viewBack =
    [ div
        [ Attr.class "file-item"
        , onClick ClickedBack
        ]
        [ div
            [ Attr.class "file-item-icon file-item-level-up fas fa-level-up-alt text-secondary"
            ]
            []
        , a
            [ Attr.href "javascript:void(0)"
            , Attr.class "file-item-name"
            ]
            [ text ".." ]
        ]
    ]
