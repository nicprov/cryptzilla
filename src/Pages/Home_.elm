module Pages.Home_ exposing (Model, Msg, page)

import Common.Alert exposing (viewAlertError)
import Compare exposing (Comparator)
import Gen.Route
import Html exposing (Html, a, button, div, hr, i, input, label, li, ol, span, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import List exposing (head)
import List.Extra exposing (uniqueBy)
import Page
import Request exposing (Request)
import S3
import S3.Types exposing (Error, KeyList, QueryElement(..))
import Set exposing (Set)
import Shared
import Storage
import Task
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared
        , update = update shared req
        , view = view shared
        , subscriptions = \_ -> Sub.none
        }

type alias Model =
    { display: String
    , keyList: Maybe KeyList
    , currentDir: String
    , folderList: List(S3.Types.KeyInfo)
    }

init : Shared.Model -> (Model, Cmd Msg)
init shared =
    (Model "" Nothing "" []
    , case shared.storage.account of
        Just account ->
            listBucket account
        Nothing ->
            Cmd.none
    )

-- Update

type Msg
    = ReceiveListBucket (Result Error KeyList)
    | ListBucket
    | ClickFolder String
    | ClickedBack
    | ClickedLogout

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

removeFiles: S3.Types.KeyInfo ->  S3.Types.KeyInfo
removeFiles key =
    let
        file = String.split "/" key.key
    in
    case List.head (List.reverse file) of
        Just element ->
            if element == "" then -- already a folder, do nothing
                key
            else
                let
                    tempName = List.drop 1 (List.reverse file) -- drop file name
                    fixedName = List.map (\m -> m ++ "/") tempName
                in
                { key | key = String.concat fixedName}
        Nothing ->
            key


isFolder: S3.Types.KeyInfo -> Bool
isFolder key =
    let
        file = String.split "/" key.key
    in
    if (List.length file) >= 2 then
        True
    else
        False


update: Shared.Model -> Request -> Msg -> Model -> (Model, Cmd Msg)
update shared req msg model =
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
                    let
                        reducedFolder = List.map removeFiles keys.keys
                        folders = List.filter isFolder reducedFolder
                    in
                    ( { model
                        | display = "Bucket listing received."
                        , keyList = Just keys
                        , folderList = uniqueBy (\k -> k.key) folders
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

        ClickedLogout ->
            ( model, Cmd.batch [ Storage.signOut shared.storage
                               , Request.replaceRoute Gen.Route.Login req
                               ]
            )

-- View

view : Shared.Model -> Model -> View Msg
view _ model =
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
                (List.map (viewFilePath model) (String.split "/" model.currentDir))
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
                        [ button
                            [ Attr.type_ "button"
                            , Attr.class "btn btn-danger mr-2"
                            , onClick ClickedLogout
                            ]
                            [ i
                                [ Attr.class "icon ion-log-out"
                                ]
                                []
                            , text "Logout" ]
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
            (case model.keyList of
                Just keyList ->
                    if List.length keyList.keys /= 0 then
                        (List.append
                            (List.append viewBack (List.map (viewFolderItem model) model.folderList))
                            (List.map (viewFileItem model) keyList.keys)
                        )
                    else
                        [viewAlertError "No files to show"]
                Nothing -> []
            )
        ]

viewFilePath: Model -> String -> Html Msg
viewFilePath model dir =
    li
        [ Attr.class "breadcrumb-item"
        ]
        [ a
            [ Attr.href "#"
            ]
            (if model.currentDir == "" then
                [ text "home" ]
             else
                [ text dir ]
            )
        ]


viewFileItem: Model -> S3.Types.KeyInfo -> Html Msg
viewFileItem model key =
    if String.contains model.currentDir key.key then
        let
            name = String.replace model.currentDir "" key.key
            file = String.split "/" name
        in
        if name /= "" then
            if (List.length file) == 1 then
                viewFile model key
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

viewFolderItem: Model -> S3.Types.KeyInfo -> Html Msg
viewFolderItem model key =
    if String.contains model.currentDir key.key then
        if model.currentDir /= key.key then
            viewFolder model key
        else
            div [] []
    else
        div [] []

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
