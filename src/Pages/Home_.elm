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
import Html exposing (Html, a, button, div, footer, hr, i, img, input, label, li, nav, node, ol, option, p, section, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import List exposing (head)
import List.Extra as LE exposing (intercalate, uniqueBy)
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
        { init = init req shared
        , update = update shared req
        , view = view shared
        , subscriptions = subscriptions
        }

type alias Model =
    { keyList: Maybe KeyListDecrypted
    , tempKeyList: Maybe KeyListDecrypted -- Used for search purposes to not overwrite original file list
    , currentDir: String
    , folderList: List(KeyInfoDecrypted)
    , tempFolderList: List(KeyInfoDecrypted) -- Used for search purposes to not overwrite original folder list
    , selectedList: List(KeyInfoDecrypted)
    , expandedItem: String
    , key: String
    , text: String
    , headers : List ( String, String )
    , status: Status
    , folderModal: Bool
    , folderName: String
    , fileNameEncrypted: Bool
    , search: String
    }

type Status
    = Success String
    | Loading String
    | Failure String
    | None

init : Request -> Shared.Model -> (Model, Cmd Msg)
init req shared =
    let
        tmpModel = { keyList = Nothing
                   , tempKeyList = Nothing
                   , currentDir = ""
                   , folderList = []
                   , tempFolderList = []
                   , selectedList = []
                   , expandedItem = ""
                   , key = ""
                   , text = ""
                   , headers = []
                   , status = Loading "Loading..."
                   , folderModal = False
                   , folderName = ""
                   , fileNameEncrypted = False
                   , search = ""
                   }
    in
    if shared.storage.encryptionKey /= "" then
        ( { tmpModel | status = Loading "Loading..." }
        , case shared.storage.account of
            Just account ->
                listBucket account
            Nothing ->
                Request.replaceRoute Gen.Route.Login req -- Redirect to login page if account is none
        )
    else
        ( tmpModel
        , Request.replaceRoute Gen.Route.Authenticate req
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
    | ClickedSettings
    | ClickedUploadFile
    | ClickedRefresh
    | ClickedNewFolder
    | ClickedCancelFolderModal
    | ClickedCreateFolder
    | ClickedDeleteSelected
    | ClickedCopyURL
    | ClickedToggleFileNameEncryption
    | ClickedSelected KeyInfoDecrypted
    | ClickedFilePath String
    | ClickedDropdown String
    | ClickedDownload KeyInfoDecrypted
    | ClickedDelete String
    | ChangedSearch String
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

putBytesObject : S3.Types.Account -> Bytes -> String -> Cmd Msg
putBytesObject account encryptedFile encryptedFileName =
    let
        bucket = (case (head account.buckets) of
            Just b -> b
            Nothing -> ""
            )
        body = S3.bytesBody "application/octet-stream" encryptedFile
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
                    tempName = List.reverse (List.drop 1 (List.reverse file)) -- drop file name
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

checkContainsSearch: String -> KeyInfoDecrypted-> Bool
checkContainsSearch search item =
    if String.contains search item.keyDecrypted then
        True
    else
        False


{- Takes a path and creates all permutations of it (excluding the path itself)
For example: If the path = "test/test2/test3/", the output
would be ["test/test2/, "test/"] except for KeyInfoDecrypted objects
-}
permutePaths: KeyInfoDecrypted -> List(KeyInfoDecrypted)
permutePaths keyInfo  =
    if keyInfo.keyDecrypted == "" then
        [] -- base case
    else
        let
            -- Decrypted Key
            splitPathDecrypted = String.split "/" keyInfo.keyDecrypted
            dropLastItemDecrypted = List.take ((List.length splitPathDecrypted) - 2) splitPathDecrypted -- drop the last item in the list + empty item (because split on "/" adds "" as last item)

            -- Encrypted Key
            splitPathEncrypted = String.split "/" keyInfo.keyEncrypted
            dropLastItemEncrypted = List.take ((List.length splitPathEncrypted) - 2) splitPathEncrypted -- drop the last item in the list + empty item (because split on "/" adds "" as last item)
        in
        if List.length dropLastItemDecrypted == 0 then
            permutePaths { keyInfo | keyDecrypted = "" }
        else
            let
                tmpPathDecrypted = (String.join "/" dropLastItemDecrypted) ++ "/" -- decrypted key
                tmpPathEncrypted = (String.join "/" dropLastItemEncrypted) ++ "/" -- encrypted key
                newKeyInfo = { keyEncrypted = tmpPathEncrypted
                             , keyDecrypted = tmpPathDecrypted
                             , lastModified = keyInfo.lastModified
                             , eTag = keyInfo.eTag
                             , size = keyInfo.size
                             , storageClass = keyInfo.storageClass
                             , owner = keyInfo.owner
                             }
            in
            newKeyInfo :: permutePaths newKeyInfo

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
                    ( { model | status = Failure "Unable to list files, invalid credentials" }
                    , Cmd.none
                    )

                Ok keys ->
                    ( model, decryptKeyList (KeyListDescriptionMessage keys shared.storage.password shared.storage.salt))

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
                                ( { model | currentDir = (String.dropLeft ((String.length bucket) + 1) dir) ++ "/" }, Cmd.none)
                        Nothing ->
                            (model, Cmd.none)
                Nothing ->
                    (model, Cmd.none)

        ReceivedDecryptedKeyList keyList ->
            if keyList.error == "" then -- No error
                let
                    reducedFolder = List.map removeFiles keyList.keys
                    folders = List.filter isFolder reducedFolder
                    permutationsOfFolder = intercalate [] (List.map permutePaths folders)
                    allFoldersJoined = List.append folders permutationsOfFolder
                    folderList = uniqueBy (\k -> k.keyDecrypted) allFoldersJoined
                in
                ( { model
                    | keyList = Just keyList
                    , tempKeyList = Just keyList
                    , folderList = folderList
                    , tempFolderList = folderList
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


        ReceiveDeleteObject result ->
            case result of
                Err err ->
                    ( { model | status = Failure ("Unable to delete file") }
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
                    ( { model | status = Failure ("Unable to download file") }
                    , Cmd.none
                    )

                Ok ( res ) ->
                    case Base64.fromBytes res of
                        Just s ->
                            ( { model | status = Loading "Decrypting file..."}, decryptFile (FileDescriptionMessage s "" shared.storage.password shared.storage.salt))
                        Nothing ->
                            ( { model | status = None }, Cmd.none) -- TODO show error message

        ClickedUploadFile ->
            ( model
            , Select.file ["application/text"] FileSelected
            )

        FileSelected file ->
            ( { model | key = (name file), status = Loading "Converting to bytes..."}, Task.perform FileConvertedToBytes (File.toBytes file))

        FileConvertedToBytes bytes ->
            case Base64.fromBytes bytes of
                Just b ->
                    ( { model | status = Loading "Encrypting file..."}, encryptFile (FileDescriptionMessage b (model.currentDir ++ model.key) shared.storage.password shared.storage.salt))

                Nothing ->
                    ( { model | status = None }, Cmd.none) -- TODO show error message

        ReceivedEncryptedFile file ->
            case shared.storage.account of
                Just acc ->
                    case Base64.toBytes file.encryptedFile of
                        Just b ->
                            ( { model | key = file.encryptedPath, status = Loading "Uploading file..." }
                            , putBytesObject acc b file.encryptedPath
                            )

                        Nothing ->
                            ( { model | status = None }, Cmd.none) -- TODO show error message

                Nothing -> ( { model | status = None}, Cmd.none)

        ReceivePutObjectBytes result ->
            case result of
                Err err ->
                    ( { model | status = Failure ("Unable to upload file") }
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
                    ( { model | status = Failure ("Unable to create folder") }
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
                , encryptFileName (FileDescriptionMessage "" (model.currentDir ++ model.folderName ++ "/") shared.storage.password shared.storage.salt)
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

        ClickedDeleteSelected ->
            let
                listDeleteCmd = List.map (\k -> case shared.storage.account of
                                                  Just acc -> deleteObject acc k.keyEncrypted
                                                  Nothing -> Cmd.none
                                         ) model.selectedList
            in
            ( { model | selectedList = [] }
            , Cmd.batch listDeleteCmd
            )

        ClickedCopyURL ->
            ( { model | status = Success "Copied URL", expandedItem = "" }, Cmd.none )

        ClickedSettings ->
            ( model, Request.replaceRoute Gen.Route.Settings req )

        ClickedToggleFileNameEncryption ->
            if model.fileNameEncrypted then
                ( { model | fileNameEncrypted = False }, Cmd.none )
            else
                ( { model | fileNameEncrypted = True }, Cmd.none )

        ChangedSearch search ->
            case model.keyList of
                Just keyList ->
                    let
                        filteredKeys = List.filter (checkContainsSearch search) keyList.keys
                        newKeyList = { keyList | keys = filteredKeys }
                        reducedFolder = List.map removeFiles filteredKeys
                        folders = List.filter isFolder reducedFolder
                        folderList = uniqueBy (\k -> k.keyDecrypted) folders
                    in
                    ( { model | tempKeyList = Just newKeyList, tempFolderList = folderList, search = search }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


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
    { title = "Cryptzilla | Home"
    , body = [ viewMain shared model shared.storage.account
             ]
    }

viewMain: Shared.Model -> Model -> Maybe S3.Types.Account-> Html Msg
viewMain shared model account =
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
                                    , Attr.href "#"
                                    , onClick ClickedSettings
                                    ]
                                    [ text "Settings" ]
                                , a
                                    [ Attr.attribute "data-v-cd57c856" ""
                                    , Attr.style "color" "#253b6e"
                                    , Attr.style "font-weight" "600"
                                    , Attr.class "navbar-item logout"
                                    , Attr.href "#"
                                    , onClick ClickedLogout
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
                                    (
                                    case account of
                                        Just a ->
                                            case (List.head a.buckets) of
                                                Just bucket ->
                                                    let
                                                        indexedDirs = (List.indexedMap Tuple.pair (String.split "/" (bucket ++ "/" ++ model.currentDir)))
                                                    in
                                                    List.map (viewFilePath indexedDirs) indexedDirs
                                                Nothing ->
                                                    [div [] []]

                                        Nothing ->
                                            [div [] []]
                                    )
                                ]
                            , div
                                [ Attr.attribute "data-v-081c0a81" ""
                                ]
                                [ input
                                    [ Attr.attribute "data-v-081c0a81" ""
                                    , Attr.id "search"
                                    , Attr.class "search-btn"
                                    , Attr.placeholder "search"
                                    , Attr.href "#"
                                    , Attr.style "margin-right" "0px"
                                    , Attr.value model.search
                                    , onInput ChangedSearch
                                    ] []
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
                                                , onClick ClickedToggleFileNameEncryption
                                                ]
                                                [ span
                                                    [ Attr.attribute "data-v-081c0a81" ""
                                                    , Attr.class "icon is-small"
                                                    ]
                                                    [ i
                                                        [ if model.fileNameEncrypted then
                                                            Attr.class "fas fa-eye"
                                                          else
                                                            Attr.class "fas fa-eye-slash"
                                                        ]
                                                        []
                                                    ]
                                                , text " Toggle encryption" ]
                                            ]
                                        ]
                                    ]
                                , viewSelectedDelete model
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
                                            , th
                                                [ Attr.class "is-sortable"
                                                ]
                                                [ div
                                                    [ Attr.class "th-wrap"
                                                    ]
                                                    [ text "Name", span
                                                        [ Attr.class "icon is-small"
                                                        ]
                                                        []
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
                                        (case model.tempKeyList of
                                            Just keyList ->
                                                if List.length keyList.keys /= 0 then
                                                    (List.append
                                                        (List.append (viewBack model) (List.map (viewFolderItem shared model) model.tempFolderList))
                                                        (List.map (viewFileItem shared model) keyList.keys)
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

viewFilePath: List((Int, String)) -> (Int, String) -> Html Msg
viewFilePath listDirs dir =
    let
        fullPath = String.join "/" (List.map (\t -> Tuple.second t) (List.filter (\t -> (Tuple.first t) <= (Tuple.first dir)) listDirs))
    in
    li
    [ Attr.attribute "data-v-081c0a81" ""
    ]
    [ a
        [ Attr.attribute "data-v-081c0a81" ""
        , Attr.href "#"
        , onClick (ClickedFilePath fullPath)
        ]
        [ text (Tuple.second dir) ]
    ]


viewFileItem: Shared.Model -> Model -> KeyInfoDecrypted -> Html Msg
viewFileItem shared model key =
    if String.contains model.currentDir key.keyDecrypted then
        let
            name = String.replace model.currentDir "" key.keyDecrypted
            file = String.split "/" name
        in
        if name /= "" && (List.length file) == 1 then
            viewFile shared model key
        else
            div [] []
    else
        div [] []

viewFile: Shared.Model -> Model -> KeyInfoDecrypted -> Html Msg
viewFile shared model key =
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
                    , Attr.href "#"
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
                    [ text (if model.fileNameEncrypted then
                                String.replace model.currentDir "" key.keyEncrypted
                            else
                                String.replace model.currentDir "" key.keyDecrypted )
                    ]
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
                (viewDropdown shared model key)
            ]
        ]

viewDropdown: Shared.Model -> Model -> KeyInfoDecrypted -> List (Html Msg)
viewDropdown shared model key =
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
                , a [ Attr.attribute "data-v-081c0a81" ""
                    , Attr.attribute "role" "listitem"
                    , Attr.tabindex 0
                    , Attr.class "dropdown-item"
                    , Attr.href "#"
                    , onClick ClickedCopyURL
                    ]
                    [ node "clipboard-copy"
                        [ (case shared.storage.account of
                             Just acc ->
                                 case (List.head acc.buckets) of
                                     Just bucket ->
                                         case acc.region of
                                             Just region ->
                                                     Attr.value ("https://" ++ bucket ++ "." ++ region ++ "digitaloceanspaces.com/" ++ key.keyEncrypted) -- https://test-onintime.nyc3.digitaloceanspaces.com/
                                             Nothing ->
                                                 Attr.value ""
                                     Nothing ->
                                         Attr.value ""
                             Nothing ->
                                 Attr.value ""

                         )
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
    ]

viewFolderItem: Shared.Model -> Model -> KeyInfoDecrypted -> Html Msg
viewFolderItem shared model key =
    if String.contains model.currentDir key.keyDecrypted then
        let
            tempFolder = String.replace model.currentDir "" key.keyDecrypted
        in

        if model.currentDir /= key.keyDecrypted && (List.length (String.split "/" tempFolder)) == 2 then
            viewFolder shared model key
        else
            div [] []
    else
        div [] []

viewFolder: Shared.Model -> Model -> KeyInfoDecrypted -> Html Msg
viewFolder _ model key =
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
        ]
        [ span []
            [ a
                [ Attr.attribute "data-v-081c0a81" ""
                , Attr.class "is-block name"
                , Attr.href "#"
                , onClick (ClickedFolder key.keyDecrypted)
                ]
                [ text (if model.fileNameEncrypted then
                            let
                                name = String.replace model.currentDir "" key.keyEncrypted
                            in
                            (String.left ((String.length name) - 1) name)
                        else
                            let
                                name = String.replace model.currentDir "" key.keyDecrypted
                            in
                            (String.left ((String.length name) - 1) name))
                ]
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
        , Attr.disabled True
        ]
        []
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

viewSelectedDelete: Model -> Html Msg
viewSelectedDelete model =
    if List.length model.selectedList > 0 then
        a
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
                        , onClick ClickedDeleteSelected
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
                        , text " Delete Selected" ]
                    ]
               ]
            ]
    else
        div [] []
