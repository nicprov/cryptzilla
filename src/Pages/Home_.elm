module Pages.Home_ exposing (Model, Msg, page)

import Base64
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Common.Alert exposing (viewAlertError, viewAlertInfo, viewAlertSuccess)
import Dict exposing (Dict)
import File exposing (File, name)
import File.Download as Download
import File.Select as Select
import Gen.Route
import Html exposing (Html, a, button, div, footer, hr, i, img, input, label, li, nav, ol, option, p, section, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import List exposing (head)
import List.Extra as LE exposing (uniqueBy)
import Page
import Request exposing (Request)
import S3
import S3.Types exposing (Error, KeyList, QueryElement(..))
import Shared exposing (EncryptedFile, FileDescriptionMessage, KeyInfoDecrypted, KeyListDecrypted, KeyListDescriptionMessage, decryptFile, decryptKeyList, encryptFile, encryptFileName)
import Storage
import Task
import View exposing (View)


page : Shared.Model -> Request -> Page.With Model Msg
page shared req =
    Page.element
        { init = init shared
        , update = update shared req
        , view = view shared
        , subscriptions = subscriptions
        }

type alias Model =
    { keyList: Maybe KeyListDecrypted
    , currentDir: String
    , folderList: List(KeyInfoDecrypted)
    , selectedList: List(KeyInfoDecrypted)
    , expandedItem: String
    , key: String
    , text: String
    , headers : List ( String, String )
    , status: Status
    , folderModal: Bool
    , folderName: String
    }

type Status
    = Success String
    | Loading String
    | Failure String
    | None

init : Shared.Model -> (Model, Cmd Msg)
init shared =
    (Model Nothing "" [] [] "" "" "" [] (Loading "Loading...") False ""
    , case shared.storage.account of
        Just account ->
            listBucket account
        Nothing ->
            Cmd.none
    )

-- Update

type Msg
    = ReceiveListBucket (Result Error KeyList)
    | ReceiveDeleteObject (Result Error String)
    | ReceiveGetObjectBytes (Result Error ( Bytes ))
    | ReceivePutObjectBytes (Result Error String)
    | ReceivedPutFolder (Result Error String)
    | ListBucket
    | FileConvertedToBytes Bytes
    | ClickedFolder String
    | ClickedBack
    | ClickedLogout
    | ClickedUploadFile
    | ClickedRefresh
    | ClickedNewFolder
    | ClickedCancelFolderModal
    | ClickedCreateFolder
    | ClickedSelected KeyInfoDecrypted
    | ClickedFilePath String
    | ClickedDropdown String
    | ClickedDownload KeyInfoDecrypted
    | ClickedDelete String
    | ClickedCopyLink String
    | ChangedFolderName String
    | FileSelected File
    | ReceivedDecryptedKeyList KeyListDecrypted
    | ReceivedDecryptedFile (String, String)
    | ReceivedEncryptedFile EncryptedFile
    | ReceivedEncryptedFileName (String, String)

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

deleteObject : S3.Types.Account -> String -> Cmd Msg
deleteObject account key =
    let
        bucket = (case (head account.buckets) of
            Just b -> b
            Nothing -> ""
            )
    in
    S3.deleteObject bucket key
        |> S3.send account
        |> Task.attempt ReceiveDeleteObject

getBytesObject : S3.Types.Account -> String -> Cmd Msg
getBytesObject account key  =
    let
        bucket = (case (head account.buckets) of
            Just b -> b
            Nothing -> ""
            )
    in
    S3.getBytesObject bucket key
        |> S3.sendBytes account
        |> Task.attempt ReceiveGetObjectBytes

putFolder : S3.Types.Account -> String -> Cmd Msg
putFolder account encryptedFileName=
    let
        bucket = (case (head account.buckets) of
            Just b -> b
            Nothing -> ""
            )
        body = S3.stringBody "" ""
    in
    S3.putObject bucket encryptedFileName body
        |> S3.send account
        |> Task.attempt ReceivedPutFolder

putBytesObject : S3.Types.Account -> Bytes -> String -> String -> Cmd Msg
putBytesObject account encryptedFile encryptedFileName sha256=
    let
        bucket = (case (head account.buckets) of
            Just b -> b
            Nothing -> ""
            )
        body = S3.bytesBody "application/octet-stream" encryptedFile sha256
    in
    S3.putBytesObject bucket encryptedFileName body
        |> S3.send account
        |> Task.attempt ReceivePutObjectBytes

removeFiles: KeyInfoDecrypted ->  KeyInfoDecrypted
removeFiles key =
    let
        file = String.split "/" key.keyDecrypted
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
                { key | keyDecrypted = String.concat fixedName}
        Nothing ->
            key


isFolder: KeyInfoDecrypted -> Bool
isFolder key =
    let
        file = String.split "/" key.keyDecrypted
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
                    ( { model | status = Loading "Getting bucket listing..." }
                    , listBucket acc
                    )
                Nothing -> (model, Cmd.none)

        ReceiveListBucket result ->
            case result of
                Err err ->
                    ( { model | status = Failure (Debug.toString err) }
                    , Cmd.none
                    )

                Ok keys ->
                    ( model, decryptKeyList (KeyListDescriptionMessage keys shared.storage.encryptionKey shared.storage.salt))

        ClickedFolder folder ->
            ( { model | currentDir = folder, expandedItem = "" }, Cmd.none)


        ClickedBack ->
            let
                tempList = List.reverse (List.drop 2 (List.reverse (String.split "/" model.currentDir)))
                newDirList =  List.map (\m -> m ++ "/") tempList
            in
            ( { model | currentDir = String.concat newDirList }, Cmd.none)

        ClickedLogout ->
            ( model, Cmd.batch [ Storage.signOut shared.storage
                               , Request.replaceRoute Gen.Route.Login req
                               ]
            )

        ClickedFilePath dir ->
            case shared.storage.account of
                Just acc ->
                    case List.head acc.buckets of
                        Just bucket ->
                            if dir == bucket then
                                ( { model | currentDir = "" }, Cmd.none )
                            else
                                (model, Cmd.none)
                        Nothing ->
                            (model, Cmd.none)
                Nothing ->
                    (model, Cmd.none)

        ReceivedDecryptedKeyList keyList ->
            if keyList.error == "" then -- No error
                let
                    reducedFolder = List.map removeFiles keyList.keys
                    folders = List.filter isFolder reducedFolder
                in
                ( { model
                    | keyList = Just keyList
                    , folderList = uniqueBy (\k -> k.keyDecrypted) folders
                    , status = (if (List.length keyList.keys == 0) then
                                    Failure "No files to show"
                                else
                                    None
                                )
                  }
                , Cmd.none
                )
            else
                ( { model | status = Failure keyList.error }, Cmd.none)

        ClickedSelected keyInfo ->
            if List.member keyInfo model.selectedList then
                ( { model | selectedList = (List.filter (\x -> x /= keyInfo) model.selectedList) }, Cmd.none ) -- Remove item from list
            else
                ( { model | selectedList = (List.append [keyInfo] model.selectedList) }, Cmd.none ) -- Add item to list

        ClickedDropdown item ->
            if model.expandedItem == item then
                ( { model | expandedItem = "" }, Cmd.none)
            else
                ( { model | expandedItem = item }, Cmd.none )

        ClickedDownload key ->
            case shared.storage.account of
                Just acc ->
                    ( { model | expandedItem = "", key = key.keyDecrypted, status = Loading "Downloading file..." }
                    , getBytesObject acc key.keyEncrypted
                    )
                Nothing -> (model, Cmd.none)

        ClickedDelete key ->
            case shared.storage.account of
                Just acc ->
                    ( { model | expandedItem = "", status = Loading "Deleting file..." }
                    , deleteObject acc key
                    )
                Nothing -> (model, Cmd.none)


        ClickedCopyLink key ->
            case shared.storage.account of
                Just acc ->
                    ( { model | expandedItem = "" }
                    , Cmd.none
                    )
                Nothing -> (model, Cmd.none)

        ReceiveDeleteObject result ->
            case result of
                Err err ->
                    ( { model | status = Failure ("Unable to delete file: " ++ (Debug.toString err)) }
                    , Cmd.none
                    )

                Ok res ->
                    case shared.storage.account of
                        Just acc ->
                            ( { model | expandedItem = "", status = Success "Successfully deleted object" }
                            , listBucket acc
                            )
                        Nothing -> (model, Cmd.none)

        ReceivedDecryptedFile decryptedFile ->
            let
                ext = List.head (List.reverse (String.split "." model.key))
            in
            if (Tuple.second decryptedFile) == "" then -- No errors
                case ext of
                    Just e ->
                        if e == "txt" then
                            case Base64.toString (Tuple.first decryptedFile) of
                                Just t ->
                                    ( { model | status = None }, Download.string model.key "text/plain" t)
                                Nothing ->
                                    ( { model | status = None }, Cmd.none)
                        else
                            case Base64.toBytes (Tuple.first decryptedFile) of
                                Just b ->
                                    ( { model | status = None }, Download.bytes model.key "application/octet-stream" b)
                                Nothing ->
                                    ( { model | status = None }, Cmd.none)

                    Nothing ->
                        ( { model | status = None }, Cmd.none)
            else -- There's an error
                ( { model | status = Failure (Tuple.second decryptedFile)}, Cmd.none)

        ReceiveGetObjectBytes result ->
            case result of
                Err err ->
                    ( { model | status = Failure ("Unable to download file: " ++ (Debug.toString err)) }
                    , Cmd.none
                    )

                Ok ( res ) ->
                    case Base64.fromBytes res of
                        Just s ->
                            ( { model | status = Loading "Decrypting file..."}, decryptFile (FileDescriptionMessage s "" shared.storage.encryptionKey shared.storage.salt))
                        Nothing ->
                            ( { model | status = None }, Cmd.none) -- TODO show error message

        ClickedUploadFile ->
            ( model
            , Select.file ["application/text"] FileSelected
            )

        FileSelected file ->
            ( ( { model | key = (name file)}, Task.perform FileConvertedToBytes (File.toBytes file)))

        FileConvertedToBytes bytes ->
            case Base64.fromBytes bytes of
                Just b ->
                    ( { model | status = Loading "Encrypting file..."}, encryptFile (FileDescriptionMessage b model.key shared.storage.encryptionKey shared.storage.salt))

                Nothing ->
                    ( { model | status = None }, Cmd.none) -- TODO show error message

        ReceivedEncryptedFile file ->
            case shared.storage.account of
                Just acc ->
                    case Base64.toBytes file.encryptedFile of
                        Just b ->
                            ( { model | key = file.encryptedPath, status = Loading "Uploading file..." }
                            , putBytesObject acc b file.encryptedPath file.sha256
                            )

                        Nothing ->
                            ( { model | status = None }, Cmd.none) -- TODO show error message

                Nothing -> ( { model | status = None}, Cmd.none)

        ReceivePutObjectBytes result ->
            case result of
                Err err ->
                    ( { model | status = Failure ("Unable to upload file: " ++ (Debug.toString err)) }
                    , Cmd.none
                    )

                Ok ( _ ) ->
                    case shared.storage.account of
                        Just acc ->
                            ( { model | status = Success "Successfully uploaded file" }
                            , listBucket acc
                            )

                        Nothing -> ( { model | status = None }, Cmd.none)

        ClickedNewFolder ->
            ( { model | expandedItem = "", folderModal = True}
            , Cmd.none
            )

        ReceivedEncryptedFileName encryptedFileName ->
            if (Tuple.second encryptedFileName) == "" then -- No error
                case shared.storage.account of
                    Just acc ->
                        ( { model | expandedItem = "", status = Loading "Encrypting folder name..." }
                        , putFolder acc (Tuple.first encryptedFileName)
                        )

                    Nothing -> (model, Cmd.none)
            else
                ( { model | status = Failure (Tuple.second encryptedFileName) }, Cmd.none)

        ReceivedPutFolder result ->
            case result of
                Err err ->
                    ( { model | status = Failure ("Unable to create folder: " ++ (Debug.toString err)) }
                    , Cmd.none
                    )

                Ok ( _ ) ->
                     case shared.storage.account of
                        Just acc ->
                            ( { model | status = Success "Successfully created folder" }
                            , listBucket acc
                            )

                        Nothing -> ( { model | status = None }, Cmd.none)

        ClickedCancelFolderModal ->
            ( { model | folderModal = False, status = None }, Cmd.none )

        ClickedCreateFolder ->
            if model.folderName /= "" then
                ( { model | folderModal = False, status = Loading "Creating folder..." }
                , encryptFileName (FileDescriptionMessage "" (model.currentDir ++ model.folderName ++ "/") shared.storage.encryptionKey shared.storage.salt)
                )
            else
                ( { model | status = Failure "Folder name cannot be empty" }, Cmd.none)

        ChangedFolderName folder ->
            ( { model | folderName = folder }, Cmd.none)

        ClickedRefresh ->
            case shared.storage.account of
                Just acc ->
                    ( { model | expandedItem = "", status = Loading "Reloading items" }
                    , listBucket acc
                    )
                Nothing -> (model, Cmd.none)

-- Listen for shared model changes
subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch [subscriptionEncryptFile model, subscriptionDecryptFile model , subscriptionKeyList model, subscriptionEncryptFileName model]

subscriptionEncryptFileName: Model -> Sub Msg
subscriptionEncryptFileName _ =
    Shared.encryptedFileName ReceivedEncryptedFileName

subscriptionEncryptFile: Model -> Sub Msg
subscriptionEncryptFile _ =
    Shared.encryptedFile ReceivedEncryptedFile

subscriptionDecryptFile: Model -> Sub Msg
subscriptionDecryptFile _ =
    Shared.decryptedFile ReceivedDecryptedFile

subscriptionKeyList : Model -> Sub Msg
subscriptionKeyList _ =
    Shared.decryptedKeyList ReceivedDecryptedKeyList


-- View

view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "File Manager"
    , body = [ viewMain model shared.storage.account
             ]
    }

viewMain: Model -> Maybe S3.Types.Account-> Html Msg
viewMain model account =
    div
        [ Attr.id "wrapper"
        ]
        [ div
            [ Attr.id "inner"
            ]
            [ div
                [ Attr.attribute "data-v-081c0a81" ""
                , Attr.id "dropzone"
                , Attr.class "container"
                ]
                [ div
                    [ Attr.attribute "data-v-07f55d0a" ""
                    , Attr.attribute "data-v-081c0a81" ""
                    ]
                    []
                , div
                    [ Attr.attribute "data-v-081c0a81" ""
                    , Attr.class "container"
                    ]
                    [ nav
                        [ Attr.attribute "data-v-cd57c856" ""
                        , Attr.attribute "data-v-081c0a81" ""
                        , Attr.attribute "role" "navigation"
                        , Attr.attribute "aria-label" "main navigation"
                        , Attr.class "navbar"
                        ]
                        [ div
                            [ Attr.attribute "data-v-cd57c856" ""
                            , Attr.class "navbar-brand"
                            ]
                            [ a
                                [ Attr.attribute "data-v-cd57c856" ""
                                , Attr.class "navbar-item logo"
                                ]
                                [ img
                                    [ Attr.attribute "data-v-cd57c856" ""
                                    , Attr.src "/img/logo.png"
                                    ]
                                    []
                                ]
                            , a
                                [ Attr.attribute "data-v-cd57c856" ""
                                , Attr.attribute "role" "button"
                                , Attr.attribute "aria-label" "menu"
                                , Attr.attribute "aria-expanded" "false"
                                , Attr.class "navbar-burger burger"
                                ]
                                [ span
                                    [ Attr.attribute "data-v-cd57c856" ""
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    []
                                , span
                                    [ Attr.attribute "data-v-cd57c856" ""
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    []
                                , span
                                    [ Attr.attribute "data-v-cd57c856" ""
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    []
                                ]
                            ]
                        , div
                            [ Attr.attribute "data-v-cd57c856" ""
                            , Attr.class "navbar-menu"
                            ]
                            [ div
                                [ Attr.attribute "data-v-cd57c856" ""
                                , Attr.class "navbar-end"
                                ]
                                [ a
                                    [ Attr.attribute "data-v-cd57c856" ""
                                    , Attr.style "color" "#253b6e"
                                    , Attr.style "font-weight" "600"
                                    , Attr.class "navbar-item logout"
                                    ]
                                    [ text "Log out" ]
                                ]
                            ]
                        ]
                    , div
                        [ Attr.attribute "data-v-081c0a81" ""
                        , Attr.id "browser"
                        ]
                        [ div
                            [ Attr.attribute "data-v-081c0a81" ""
                            , Attr.class "is-flex is-justify-between"
                            ]
                            [ div
                                [ Attr.attribute "data-v-081c0a81" ""
                                , Attr.attribute "aria-label" "breadcrumbs"
                                , Attr.class "breadcrumb"
                                , Attr.style "background-color" "white"
                                , Attr.style "padding-left" "0"
                                ]
                                [ ul
                                    [ Attr.attribute "data-v-081c0a81" ""
                                    ]
                                    ( case account of
                                        Just a ->
                                            ( List.append [viewFilePath
                                                (case (List.head a.buckets) of
                                                    Just bucket ->
                                                        bucket
                                                    Nothing ->
                                                        ""
                                                )
                                                ]
                                              (List.map viewFilePath (String.split "/" model.currentDir))
                                            )
                                        Nothing ->
                                            (List.map viewFilePath (String.split "/" model.currentDir))
                                    )
                                ]
                            , div
                                [ Attr.attribute "data-v-081c0a81" ""
                                ]
                                [ a
                                    [ Attr.attribute "data-v-081c0a81" ""
                                    , Attr.id "search"
                                    , Attr.class "search-btn"
                                    , Attr.href "#"
                                    ]
                                    [ span
                                        [ Attr.attribute "data-v-081c0a81" ""
                                        , Attr.class "icon is-small"
                                        ]
                                        [ i
                                            [ Attr.class "fas fa-search"
                                            ]
                                            []
                                        ]
                                    ]
                                ]
                            ]
                        , section
                            [ Attr.attribute "data-v-081c0a81" ""
                            , Attr.id "multi-actions"
                            , Attr.class "is-flex is-justify-between"
                            ]
                            [ div
                                [ Attr.attribute "data-v-081c0a81" ""
                                ]
                                [ div
                                    [ Attr.attribute "data-v-081c0a81" ""
                                    , Attr.class "field file is-inline-block"
                                    ]
                                    [ label
                                        [ Attr.attribute "data-v-081c0a81" ""
                                        , Attr.class "upload control"
                                        ]
                                        [ a
                                            [ Attr.attribute "data-v-081c0a81" ""
                                            , Attr.class "is-inline-block"
                                            , Attr.href "#"
                                            , onClick ClickedUploadFile
                                            ]
                                            [ span
                                                [ Attr.attribute "data-v-081c0a81" ""
                                                , Attr.class "icon is-small"
                                                ]
                                                [ i
                                                    [ Attr.class "fas fa-upload"
                                                    ]
                                                    []
                                                ]
                                            , text " Upload file" ]
                                        , input
                                            [ Attr.type_ "file"
                                            , Attr.multiple True
                                            , Attr.class ""
                                            ]
                                            []
                                        ]
                                    ]
                                , a
                                    [ Attr.attribute "data-v-081c0a81" ""
                                    , Attr.class "add-new is-inline-block"
                                    , Attr.href "#"
                                    ]
                                    [ div
                                        [ Attr.attribute "data-v-081c0a81" ""
                                        , Attr.class "dropdown is-mobile-modal"
                                        ]
                                        [ div
                                            [ Attr.attribute "role" "button"
                                            , Attr.attribute "aria-haspopup" "true"
                                            , Attr.class "dropdown-trigger"
                                            ]
                                            [ span
                                                [ Attr.attribute "data-v-081c0a81" ""
                                                , Attr.href "#"
                                                , onClick ClickedNewFolder
                                                ]
                                                [ span
                                                    [ Attr.attribute "data-v-081c0a81" ""
                                                    , Attr.class "icon is-small"
                                                    ]
                                                    [ i
                                                        [ Attr.class "fas fa-plus"
                                                        ]
                                                        []
                                                    ]
                                                , text " New folder" ]
                                            ]
                                        ]
                                    ]
                                , a
                                    [ Attr.attribute "data-v-081c0a81" ""
                                    , Attr.class "add-new is-inline-block"
                                    , Attr.href "#"
                                    ]
                                    [ div
                                        [ Attr.attribute "data-v-081c0a81" ""
                                        , Attr.class "dropdown is-mobile-modal"
                                        ]
                                        [ div
                                            [ Attr.attribute "role" "button"
                                            , Attr.attribute "aria-haspopup" "true"
                                            , Attr.class "dropdown-trigger"
                                            ]
                                            [ span
                                                [ Attr.attribute "data-v-081c0a81" ""
                                                , Attr.href "#"
                                                , onClick ClickedRefresh
                                                ]
                                                [ span
                                                    [ Attr.attribute "data-v-081c0a81" ""
                                                    , Attr.class "icon is-small"
                                                    ]
                                                    [ i
                                                        [ Attr.class "fas fa-redo-alt"
                                                        ]
                                                        []
                                                    ]
                                                , text " Refresh" ]
                                            ]
                                        ]
                                    ]
                                ]
                            , div
                                [ Attr.attribute "data-v-081c0a81" ""
                                , Attr.id "pagination"
                                ]
                                [ div
                                    [ Attr.attribute "data-v-081c0a81" ""
                                    ]
                                    [ div
                                        [ Attr.class "control"
                                        ]
                                        [ span
                                            [ Attr.class "select is-small"
                                            ]
                                            [ select []
                                                [ option
                                                    [ Attr.value ""
                                                    ]
                                                    [ text "No pagination" ]
                                                , option
                                                    [ Attr.value "5"
                                                    ]
                                                    [ text "5 Per Page" ]
                                                , option
                                                    [ Attr.value "10"
                                                    ]
                                                    [ text "10 Per Page" ]
                                                , option
                                                    [ Attr.value "15"
                                                    ]
                                                    [ text "15 Per Page" ]
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        , div
                            [ Attr.attribute "data-v-081c0a81" ""
                            , Attr.class "b-table"
                            ]
                            [ div
                                [ Attr.class "field table-mobile-sort"
                                ]
                                [ div
                                    [ Attr.class "field has-addons"
                                    ]
                                    [ div
                                        [ Attr.class "control is-expanded"
                                        ]
                                        [ span
                                            [ Attr.class "select is-fullwidth"
                                            ]
                                            [ select []
                                                [ option
                                                    [ Attr.value "[object Object]"
                                                    ]
                                                    [ text "Name" ]
                                                , option
                                                    [ Attr.value "[object Object]"
                                                    ]
                                                    [ text "Size" ]
                                                , option
                                                    [ Attr.value "[object Object]"
                                                    ]
                                                    [ text "Time" ]
                                                ]
                                            ]
                                        ]
                                    , div
                                        [ Attr.class "control"
                                        ]
                                        [ button
                                            [ Attr.class "button is-primary"
                                            ]
                                            [ span
                                                [ Attr.class "icon is-small"
                                                ]
                                                [ i
                                                    [ Attr.class "fas fa-arrow-up"
                                                    ]
                                                    []
                                                ]
                                            ]
                                        ]
                                    ]
                                ]
                            , (case model.status of
                                Success msg ->
                                    viewAlertSuccess msg
                                Loading msg ->
                                    viewAlertInfo msg
                                Failure error ->
                                    viewAlertError error
                                _ ->
                                    div [] []
                            )
                            , div
                                [ Attr.class "table-wrapper"
                                ]
                                [ table
                                    [ Attr.class "table has-mobile-cards is-hoverable"
                                    ]
                                    [ thead []
                                        [ tr []
                                            [ th
                                                [ Attr.class "checkbox-cell"
                                                ]
                                                [ label
                                                    [ Attr.class "b-checkbox checkbox"
                                                    ]
                                                    [ input
                                                        [ Attr.type_ "checkbox"
                                                        , Attr.attribute "true-value" "true"
                                                        , Attr.value "false"
                                                        ]
                                                        []
                                                    , span
                                                        [ Attr.class "check"
                                                        ]
                                                        []
                                                    , span
                                                        [ Attr.class "control-label"
                                                        ]
                                                        []
                                                    ]
                                                ]
                                            , th
                                                [ Attr.class "is-current-sort is-sortable"
                                                ]
                                                [ div
                                                    [ Attr.class "th-wrap"
                                                    ]
                                                    [ text "Name", span
                                                        [ Attr.class "icon is-small"
                                                        ]
                                                        [ i
                                                            [ Attr.class "fas fa-arrow-up"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                ]
                                            , th
                                                [ Attr.class "is-sortable"
                                                , Attr.style "width" "150px"
                                                ]
                                                [ div
                                                    [ Attr.class "th-wrap is-numeric"
                                                    ]
                                                    [ text "Size", span
                                                        [ Attr.class "icon is-small"
                                                        , Attr.style "display" "none"
                                                        ]
                                                        [ i
                                                            [ Attr.class "fas fa-arrow-up"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                ]
                                            , th
                                                [ Attr.class "is-sortable"
                                                , Attr.style "width" "200px"
                                                ]
                                                [ div
                                                    [ Attr.class "th-wrap is-numeric"
                                                    ]
                                                    [ text "Time", span
                                                        [ Attr.class "icon is-small"
                                                        , Attr.style "display" "none"
                                                        ]
                                                        [ i
                                                            [ Attr.class "fas fa-arrow-up"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                ]
                                            , th
                                                [ Attr.class ""
                                                , Attr.style "width" "51px"
                                                ]
                                                [ div
                                                    [ Attr.class "th-wrap"
                                                    ]
                                                    [ span
                                                        [ Attr.class "icon is-small"
                                                        , Attr.style "display" "none"
                                                        ]
                                                        [ i
                                                            [ Attr.class "fas fa-arrow-up"
                                                            ]
                                                            []
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    , tbody []
                                        (case model.keyList of
                                            Just keyList ->
                                                if List.length keyList.keys /= 0 then
                                                    (List.append
                                                        (List.append (viewBack model) (List.map (viewFolderItem model) model.folderList))
                                                        (List.map (viewFileItem model) keyList.keys)
                                                    )
                                                else
                                                    [div [] []]
                                            Nothing -> []
                                        )
                                    ]
                                ]
                            ]
                        , section
                            [ Attr.attribute "data-v-081c0a81" ""
                            , Attr.id "bottom-info"
                            , Attr.class "is-flex is-justify-between"
                            ]
                            [ div
                                [ Attr.attribute "data-v-081c0a81" ""
                                ]
                                [ span
                                    [ Attr.attribute "data-v-081c0a81" ""
                                    ]
                                    [ text ("Selected: " ++ (String.fromInt (List.length model.selectedList))) ]
                                ]
                            ]
                        , if model.folderModal then
                            viewFolderModal
                          else
                            div [] []
                        ]
                    ]
                ]
            ]
        ]

viewFolderModal: Html Msg
viewFolderModal =
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
                    , div
                        [ Attr.class "field"
                        ]
                        [ div
                            [ Attr.class "control"
                            ]
                            [ input
                                [ Attr.placeholder "MyFolder"
                                , Attr.maxlength 100
                                , Attr.class "input"
                                , onInput ChangedFolderName
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
                , onClick ClickedCancelFolderModal
                ]
                [ text "Cancel" ]
            , button
                [ Attr.class "button is-primary"
                , onClick ClickedCreateFolder
                ]
                [ text "Create" ]
            ]
        ]
    ]

viewFilePath: String -> Html Msg
viewFilePath dir =
    li
    [ Attr.attribute "data-v-081c0a81" ""
    ]
    [ a
        [ Attr.attribute "data-v-081c0a81" ""
        , Attr.href "#"
        , onClick (ClickedFilePath dir)
        ]
        [ text dir ]
    ]


viewFileItem: Model -> KeyInfoDecrypted -> Html Msg
viewFileItem model key =
    if String.contains model.currentDir key.keyDecrypted then
        let
            name = String.replace model.currentDir "" key.keyDecrypted
            file = String.split "/" name
        in
        if name /= "" && (List.length file) == 1 then
            viewFile model key
        else
            div [] []
    else
        div [] []

viewFile: Model -> KeyInfoDecrypted -> Html Msg
viewFile model key =
    let
        name = String.replace model.currentDir "" key.keyDecrypted
    in
    tr
        [ Attr.draggable "false"
        , Attr.class "file-row type-file"
        ]
        [ td
            [ Attr.class "checkbox-cell"
            ]
            [ label
                [ Attr.class "b-checkbox checkbox"
                ]
                [ input
                    [ Attr.type_ "checkbox"
                    , Attr.attribute "true-value" "true"
                    , Attr.value "false"
                    , onClick (ClickedSelected key)
                    ]
                    []
                , span
                    [ Attr.class "check"
                    ]
                    []
                , span
                    [ Attr.class "control-label"
                    ]
                    []
                ]
            ]
        , td
            [ Attr.attribute "data-v-081c0a81" ""
            , Attr.attribute "data-label" "Name"
            , Attr.class ""
            ]
            [ span []
                [ a
                    [ Attr.attribute "data-v-081c0a81" ""
                    , Attr.class "is-block name"
                    , Attr.href "#"
                    ]
                    [ text name ]
                ]
            ]
        , td
            [ Attr.attribute "data-v-081c0a81" ""
            , Attr.attribute "data-label" "Size"
            , Attr.class "has-text-right"
            ]
            [ span []
                [ text (String.fromInt key.size ++ " Bytes") ]
            ]
        , td
            [ Attr.attribute "data-v-081c0a81" ""
            , Attr.attribute "data-label" "Time"
            , Attr.class "has-text-right"
            ]
            [ span []
                [ text key.lastModified ]
            ]
        , td
            [ Attr.attribute "data-v-081c0a81" ""
            , Attr.class ""
            , Attr.id "single-actions"
            ]
            [ span []
                (viewDropdown model key)
            ]
        ]

viewDropdown: Model -> KeyInfoDecrypted -> List (Html Msg)
viewDropdown model key =
    [ div
        [ Attr.attribute "data-v-081c0a81" ""
        , if key.keyDecrypted == model.expandedItem then
            Attr.class "dropdown is-bottom-left is-active is-mobile-modal"
         else
            Attr.class "dropdown is-bottom-left is-mobile-modal"
        ]
        [ div
            [ Attr.attribute "role" "button"
            , Attr.attribute "aria-haspopup" "true"
            , Attr.class "dropdown-trigger"
            ]
            [ button
                [ Attr.attribute "data-v-081c0a81" ""
                , Attr.class "button is-small"
                , onClick (ClickedDropdown key.keyDecrypted)
                ]
                [ span
                    [ Attr.attribute "data-v-081c0a81" ""
                    , Attr.class "icon is-small"
                    ]
                    [ i
                        [ Attr.class "fas fa-ellipsis-h"
                        ]
                        []
                    ]
                ]
            ]
        , div
            [ Attr.attribute "aria-hidden" "true"
            , Attr.class "background"
            , Attr.style "display" "none"
            ]
            []
        , div
            [ Attr.attribute "aria-hidden" "true"
            , Attr.class "dropdown-menu"
            , if key.keyDecrypted == model.expandedItem then
                Attr.style "" ""
              else
               Attr.style "display" "none"
            ]
            [ div
                [ Attr.attribute "role" "list"
                , Attr.class "dropdown-content"
                ]
                [ a
                    [ Attr.attribute "data-v-081c0a81" ""
                    , Attr.attribute "role" "listitem"
                    , Attr.tabindex 0
                    , Attr.class "dropdown-item"
                    , Attr.href "#"
                    , onClick (ClickedDownload key)
                    ]
                    [ span
                        [ Attr.attribute "data-v-081c0a81" ""
                        , Attr.class "icon is-small"
                        ]
                        [ i
                            [ Attr.class "fas fa-download"
                            ]
                            []
                        ]
                    , text " Download" ]
                , a
                    [ Attr.attribute "data-v-081c0a81" ""
                    , Attr.attribute "role" "listitem"
                    , Attr.tabindex 0
                    , Attr.class "dropdown-item"
                    , Attr.href "#"
                    , onClick (ClickedDelete key.keyEncrypted)
                    ]
                    [ span
                        [ Attr.attribute "data-v-081c0a81" ""
                        , Attr.class "icon is-small"
                        ]
                        [ i
                            [ Attr.class "fas fa-trash-alt"
                            ]
                            []
                        ]
                    , text " Delete" ]
                , a
                    [ Attr.attribute "data-v-081c0a81" ""
                    , Attr.attribute "role" "listitem"
                    , Attr.tabindex 0
                    , Attr.class "dropdown-item"
                    , Attr.href "#"
                    ]
                    [ span
                        [ Attr.attribute "data-v-081c0a81" ""
                        , Attr.class "icon is-small"
                        ]
                        [ i
                            [ Attr.class "fas fa-clipboard"
                            ]
                            []
                        ]
                    , text " Copy link" ]
                ]
            ]
        ]
    ]

viewFolderItem: Model -> KeyInfoDecrypted -> Html Msg
viewFolderItem model key =
    if String.contains model.currentDir key.keyDecrypted then
        let
            tempFolder = String.replace model.currentDir "" key.keyDecrypted
        in

        if model.currentDir /= key.keyDecrypted && (List.length (String.split "/" tempFolder)) == 2 then
            viewFolder model key
        else
            div [] []
    else
        div [] []

viewFolder: Model -> KeyInfoDecrypted -> Html Msg
viewFolder model key =
    let
        name = String.replace model.currentDir "" key.keyDecrypted
    in
    tr
    [ Attr.draggable "false"
    , Attr.class "file-row type-dir"
    ]
    [ td
        [ Attr.class "checkbox-cell"
        ]
        [ label
            [ Attr.class "b-checkbox checkbox"
            ]
            [ input
                [ Attr.type_ "checkbox"
                , Attr.attribute "true-value" "true"
                , Attr.value "false"
                ]
                []
            , span
                [ Attr.class "check"
                ]
                []
            , span
                [ Attr.class "control-label"
                ]
                []
            ]
        ]
    , td
        [ Attr.attribute "data-v-081c0a81" ""
        , Attr.attribute "data-label" "Name"
        , Attr.class ""
        ]
        [ span []
            [ a
                [ Attr.attribute "data-v-081c0a81" ""
                , Attr.class "is-block name"
                , Attr.href "#"
                , onClick (ClickedFolder key.keyDecrypted)
                ]
                [ text (String.left ((String.length name) - 1) name) ]
            ]
        ]
    , td
        [ Attr.attribute "data-v-081c0a81" ""
        , Attr.attribute "data-label" "Size"
        , Attr.class "has-text-right"
        ]
        [ span []
            [ text "Folder" ]
        ]
    , td
        [ Attr.attribute "data-v-081c0a81" ""
        , Attr.attribute "data-label" "Time"
        , Attr.class "has-text-right"
        ]
        [ span []
            [ text key.lastModified ]
        ]
    , td
        [ Attr.attribute "data-v-081c0a81" ""
        , Attr.class ""
        , Attr.id "single-actions"
        ]
        [ span []
            (viewDropdown model key)
        ]
    ]

viewBack: Model -> List (Html Msg)
viewBack model =
    if model.currentDir == "" then
        []
    else
        [ tr
            [ Attr.draggable "false"
            , Attr.class "file-row type-back"
            ]
            [ td
                [ Attr.class "checkbox-cell"
                ]
                [ label
                    [ Attr.class "b-checkbox checkbox is-disabled"
                    , Attr.disabled True
                    ]
                    [ input
                        [ Attr.type_ "checkbox"
                        , Attr.attribute "true-value" "true"
                        , Attr.value "false"
                        , Attr.disabled True
                        ]
                        []
                    , span
                        [ Attr.class "check"
                        ]
                        []
                    , span
                        [ Attr.class "control-label"
                        ]
                        []
                    ]
                ]
            , td
                [ Attr.attribute "data-v-081c0a81" ""
                , Attr.attribute "data-label" "Name"
                , Attr.class ""
                , onClick ClickedBack
                ]
                [ span []
                    [ a
                        [ Attr.attribute "data-v-081c0a81" ""
                        , Attr.class "is-block name"
                        , Attr.href "#"
                        ]
                        [ text ".." ]
                    ]
                ]
            , td
                [ Attr.attribute "data-v-081c0a81" ""
                , Attr.attribute "data-label" "Size"
                , Attr.class "has-text-right"
                ]
                [ span []
                    [ text "Folder" ]
                ]
            , td
                [ Attr.attribute "data-v-081c0a81" ""
                , Attr.attribute "data-label" "Time"
                , Attr.class "has-text-right"
                ]
                [ span []
                    []
                ]
            , td
                [ Attr.attribute "data-v-081c0a81" ""
                , Attr.class ""
                , Attr.id "single-actions"
                ]
                [ span []
                    []
                ]
            ]
        ]