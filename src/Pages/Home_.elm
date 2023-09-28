module Pages.Home_ exposing (Model, Msg, page)

import Base64
import Browser.Navigation as Navigation
import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Bytes.Encode as Encode
import Common.Alert exposing (viewAlertError, viewAlertInfo, viewAlertSuccess)
import Common.Footer exposing (viewFooter)
import Dict exposing (Dict)
import File exposing (File, name)
import File.Download as Download
import File.Select as Select
import Gen.Route
import Html exposing (Attribute, Html, a, button, div, footer, hr, i, img, input, label, li, nav, node, ol, option, p, section, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import List exposing (head)
import List.Extra as LE exposing (intercalate, uniqueBy)
import Page
import Process
import Request exposing (Request)
import S3
import S3.Types exposing (CommonPrefixes, Error, KeyInfo, KeyList, QueryElement(..))
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
    { keys: List KeyInfoDecrypted
    , tempKeys: List KeyInfoDecrypted -- Used for search purposes to not overwrite original file list
    , loadingKeys: List KeyInfo -- Used to gradually load all keys to display
    , currentDir: CurrentDir
    , folders: List KeyInfoDecrypted
    , tempFolders: List KeyInfoDecrypted -- Used for search purposes to not overwrite original folder list
    , selectedList: List KeyInfoDecrypted
    , expandedItem: String
    , key: String
    , text: String
    , headers : List ( String, String )
    , status: Status
    , folderModal: Bool
    , folderName: String
    , fileNameEncrypted: Bool
    , search: String
    , sortAttribute: SortAttribute
    , sortType: SortType
    }

type alias CurrentDir =
    { dirEncrypted: String
    , dirDecrypted: String
    }

type SortAttribute
    = Name
    | Size
    | Time

type SortType
    = Ascending
    | Descending

type Status
    = Success String
    | Loading String
    | Failure String
    | None

init : Request -> Shared.Model -> (Model, Cmd Msg)
init req shared =
    let
        tmpModel = { keys = []
                   , tempKeys = []
                   , loadingKeys = []
                   , currentDir = { dirEncrypted = ""
                                  , dirDecrypted = ""
                                  }
                   , folders = []
                   , tempFolders = []
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
                   , sortType = Ascending
                   , sortAttribute = Name
                   }
    in
    if shared.storage.encryptionKey /= "" then
        ( { tmpModel | status = Loading "Loading..." }
        , case shared.storage.account of
            Just account ->
                Cmd.batch [ listBucket account "" (CurrentDir "" "")
                          , Process.sleep (toFloat shared.storage.timeout)
                                |> Task.perform (\_ -> ClickedLock)
                          ]
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
    | ClickedFolder KeyInfoDecrypted
    | ClickedBack
    | ClickedLogout
    | ClickedSettings
    | ClickedLock
    | ClickedUploadFile
    | ClickedRefresh
    | ClickedNewFolder
    | ClickedCancelFolderModal
    | ClickedCreateFolder
    | ClickedDeleteSelected
    | ClickedCopyURL
    | ClickedToggleFileNameEncryption
    | ClickedSortName
    | ClickedSortSize
    | ClickedSortTime
    | ClickedSelected KeyInfoDecrypted
    | ClickedFilePath (String, String)
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

listBucket : S3.Types.Account -> String -> CurrentDir -> Cmd Msg
listBucket account marker currentDir =
    let
        bucket = (case (head account.buckets) of
            Just b -> b
            Nothing -> ""
            )
    in
    S3.listKeys bucket
        |> S3.addQuery [ Delimiter "/" ]
        |> S3.addQuery [ Prefix currentDir.dirEncrypted ]
        |> S3.addQuery [ Marker marker ]
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

commonPrefixToKeyInfo: CommonPrefixes -> KeyInfo
commonPrefixToKeyInfo commonPrefix =
    { key = commonPrefix.prefix
    , lastModified = ""
    , eTag = ""
    , size = 0
    , storageClass = ""
    , owner = Just { id = ""
                   , displayName = ""
                   }
    }

update: Shared.Model -> Request -> Msg -> Model -> (Model, Cmd Msg)
update shared req msg model =
    case msg of

        ListBucket ->
            case shared.storage.account of
                Just acc ->
                    ( { model | status = Loading "Getting bucket listing..." }
                    , listBucket acc "" { dirEncrypted = "", dirDecrypted = "" }
                    )
                Nothing -> (model, Cmd.none)

        ReceiveListBucket result ->
            case result of
                Err _ ->
                    ( { model | status = Failure "Unable to list files, invalid credentials" }
                    , Cmd.none
                    )

                Ok keyList ->
                    if keyList.isTruncated then -- Need to keep fetching keys if isTruncated is True
                        case shared.storage.account of
                            Just acc ->
                                case keyList.nextMarker of
                                    Just marker ->
                                        let
                                            folderList = List.map commonPrefixToKeyInfo keyList.prefixes
                                        in
                                        ( { model | loadingKeys = keyList.keys ++ folderList ++ model.loadingKeys }, listBucket acc marker (CurrentDir "" ""))

                                    Nothing ->
                                        ( { model | status = Failure "Unable to get nextMarker" }, Cmd.none )

                            Nothing -> (model, Cmd.none)
                    else
                        let
                            folderList = List.map commonPrefixToKeyInfo keyList.prefixes
                            tempKeys = keyList.keys ++ folderList ++ model.loadingKeys
                        in
                        ( model, decryptKeyList (KeyListDescriptionMessage tempKeys shared.storage.password shared.storage.salt))

        ClickedFolder folder ->
            case shared.storage.account of
                Just acc ->
                    let
                        currentDir = { dirEncrypted = folder.keyEncrypted
                                     , dirDecrypted = folder.keyDecrypted
                                     }
                    in
                    ( { model | currentDir = currentDir , status = Loading "Loading..." }
                    , listBucket acc "" currentDir
                    )
                Nothing -> (model, Cmd.none)


        ClickedBack ->
            case shared.storage.account of
                Just acc ->
                    let
                        -- Decrypted dir
                        tempListDecrypted = List.reverse (List.drop 2 (List.reverse (String.split "/" model.currentDir.dirDecrypted)))
                        newDirListDecrypted =  List.map (\m -> m ++ "/") tempListDecrypted
                        currentDirDecrypted = String.concat newDirListDecrypted
                        -- Encrypted dir
                        tempListEncrypted = List.reverse (List.drop 2 (List.reverse (String.split "/" model.currentDir.dirEncrypted)))
                        newDirListEncrypted =  List.map (\m -> m ++ "/") tempListEncrypted
                        currentDirEncrypted = String.concat newDirListEncrypted

                        currentDir = { dirDecrypted = currentDirDecrypted, dirEncrypted = currentDirEncrypted }
                    in
                    ( { model | currentDir = currentDir, status = Loading "Loading..." }
                    , listBucket acc "" currentDir
                    )
                Nothing -> (model, Cmd.none)


        ClickedLogout ->
            ( model, Cmd.batch [ Storage.signOut shared.storage
                               , Request.replaceRoute Gen.Route.Login req
                               ]
            )

        ClickedFilePath dir ->
            case shared.storage.account of
                Just acc ->
                    if (Tuple.first dir) == "/" then
                        let
                            currentDir = CurrentDir "" ""
                        in
                        ( { model | currentDir = currentDir, status = Loading "Loading..." }
                        , listBucket acc "" currentDir
                        )
                    else
                        let
                            currentDir = { dirEncrypted = Tuple.first dir
                                         , dirDecrypted = Tuple.second dir
                                         }
                        in
                        ( { model | currentDir = currentDir, status = Loading "Loading..." }
                        , listBucket acc "" currentDir
                        )
                Nothing ->
                    (model, Cmd.none)

        ReceivedDecryptedKeyList decryptedKeys ->
            if decryptedKeys.error == "" then -- No error
                let
                    reducedFolder = List.map removeFiles decryptedKeys.keys
                    folders = List.filter isFolder reducedFolder
                    folderList = uniqueBy (\k -> k.keyDecrypted) folders
                in
                case model.sortAttribute of
                    Name ->
                        let
                            tempKeys = List.sortBy .keyDecrypted decryptedKeys.keys
                            tempFolders = List.sortBy .keyDecrypted folderList
                        in
                        case model.sortType of
                            Ascending ->
                                ( { model
                                    | keys = decryptedKeys.keys
                                    , tempKeys = tempKeys
                                    , folders = folderList
                                    , tempFolders = tempFolders
                                    , status = (if (List.length decryptedKeys.keys == 0) then
                                                    Failure "No files to show"
                                                else
                                                    None
                                                )
                                  }
                                , Cmd.none
                                )

                            Descending ->
                                ( { model
                                    | keys = decryptedKeys.keys
                                    , tempKeys = List.reverse tempKeys
                                    , folders = folderList
                                    , tempFolders = List.reverse tempFolders
                                    , status = (if (List.length decryptedKeys.keys == 0) then
                                                    Failure "No files to show"
                                                else
                                                    None
                                                )
                                  }
                                , Cmd.none
                                )

                    Size ->
                        let
                            tempKeys = List.sortBy .size decryptedKeys.keys
                        in
                        case model.sortType of
                            Ascending ->
                                ( { model
                                    | keys = decryptedKeys.keys
                                    , tempKeys = tempKeys
                                    , folders = folderList
                                    , tempFolders = folderList
                                    , status = (if (List.length decryptedKeys.keys == 0) then
                                                    Failure "No files to show"
                                                else
                                                    None
                                                )
                                  }
                                , Cmd.none
                                )

                            Descending ->
                                ( { model
                                    | keys = decryptedKeys.keys
                                    , tempKeys = List.reverse tempKeys
                                    , folders = folderList
                                    , tempFolders = folderList
                                    , status = (if (List.length decryptedKeys.keys == 0) then
                                                    Failure "No files to show"
                                                else
                                                    None
                                                )
                                  }
                                , Cmd.none
                                )

                    Time ->
                        let
                            tempKeys = List.sortBy .lastModified decryptedKeys.keys
                            tempFolders = List.sortBy .lastModified folderList
                        in
                        case model.sortType of
                            Ascending ->
                                ( { model
                                    | keys = decryptedKeys.keys
                                    , tempKeys = tempKeys
                                    , folders = folderList
                                    , tempFolders = tempFolders
                                    , status = (if (List.length decryptedKeys.keys == 0) then
                                                    Failure "No files to show"
                                                else
                                                    None
                                                )
                                  }
                                , Cmd.none
                                )

                            Descending ->
                                ( { model
                                    | keys = decryptedKeys.keys
                                    , tempKeys = List.reverse tempKeys
                                    , folders = folderList
                                    , tempFolders = List.reverse tempFolders
                                    , status = (if (List.length decryptedKeys.keys == 0) then
                                                    Failure "No files to show"
                                                else
                                                    None
                                                )
                                  }
                                , Cmd.none
                                )
                else
                ( { model | status = Failure decryptedKeys.error }, Cmd.none)

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
                    ( { model | expandedItem = "", selectedList = [], status = Loading "Deleting file..." }
                    , deleteObject acc key
                    )
                Nothing -> (model, Cmd.none)


        ReceiveDeleteObject result ->
            case result of
                Err _ ->
                    ( { model | status = Failure ("Unable to delete file") }
                    , Cmd.none
                    )

                Ok _ ->
                    case shared.storage.account of
                        Just acc ->
                            ( { model | expandedItem = "", status = Success "Successfully deleted object" }
                            , listBucket acc "" model.currentDir
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
                Err _ ->
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
                    ( { model | status = Loading "Encrypting file..."}, encryptFile (FileDescriptionMessage b (model.currentDir.dirDecrypted ++ model.key) shared.storage.password shared.storage.salt))

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
                Err _ ->
                    ( { model | status = Failure ("Unable to upload file") }
                    , Cmd.none
                    )

                Ok ( _ ) ->
                    case shared.storage.account of
                        Just acc ->
                            ( { model | status = Success "Successfully uploaded file" }
                            , listBucket acc "" model.currentDir
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
                Err _ ->
                    ( { model | status = Failure ("Unable to create folder") }
                    , Cmd.none
                    )

                Ok ( _ ) ->
                     case shared.storage.account of
                        Just acc ->
                            ( { model | status = Success "Successfully created folder" }
                            , listBucket acc "" model.currentDir
                            )

                        Nothing -> ( { model | status = None }, Cmd.none)

        ClickedCancelFolderModal ->
            ( { model | folderModal = False, status = None }, Cmd.none )

        ClickedCreateFolder ->
            if model.folderName /= "" then
                ( { model | folderModal = False, status = Loading "Creating folder..." }
                , encryptFileName (FileDescriptionMessage "" (model.currentDir.dirDecrypted ++ model.folderName ++ "/") shared.storage.password shared.storage.salt)
                )
            else
                ( { model | status = Failure "Folder name cannot be empty" }, Cmd.none)

        ChangedFolderName folder ->
            ( { model | folderName = folder }, Cmd.none)

        ClickedRefresh ->
            case shared.storage.account of
                Just acc ->
                    ( { model | expandedItem = "", status = Loading "Reloading items" }
                    , listBucket acc "" model.currentDir
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
            let
                filteredKeys = List.filter (checkContainsSearch search) model.keys
                reducedFolder = List.map removeFiles filteredKeys
                folders = List.filter isFolder reducedFolder
                folderList = uniqueBy (\k -> k.keyDecrypted) folders
            in
            ( { model | tempKeys = filteredKeys, tempFolders = folderList, search = search }, Cmd.none )

        ClickedSortName ->
            case model.sortAttribute of
                Name ->
                    case model.sortType of
                        Ascending ->
                            ( { model | sortType = Descending, tempKeys = List.reverse model.tempKeys, tempFolders = List.reverse model.tempFolders }, Cmd.none )

                        Descending ->
                            ( { model | sortType = Ascending, tempKeys = List.reverse model.tempKeys, tempFolders = List.reverse model.tempFolders }, Cmd.none )
                _ ->
                    let
                        keys = List.sortBy .keyDecrypted model.tempKeys
                        folders = List.sortBy .keyDecrypted model.tempFolders
                    in
                    ( { model | sortAttribute = Name, sortType = Ascending, tempKeys = keys, tempFolders = folders }, Cmd.none )

        ClickedSortSize ->
            case model.sortAttribute of
                Size ->
                    case model.sortType of
                        Ascending ->
                            ( { model | sortType = Descending, tempKeys = List.reverse model.tempKeys }, Cmd.none )

                        Descending ->
                            ( { model | sortType = Ascending, tempKeys = List.reverse model.tempKeys }, Cmd.none )
                _ ->
                    let
                        keys = List.sortBy .size model.tempKeys
                    in
                    ( { model | sortAttribute = Size, sortType = Ascending, tempKeys = keys }, Cmd.none )

        ClickedSortTime ->
            case model.sortAttribute of
                Time ->
                    case model.sortType of
                        Ascending ->
                            ( { model | sortType = Descending, tempKeys = List.reverse model.keys, tempFolders = List.reverse model.folders }, Cmd.none )

                        Descending ->
                            ( { model | sortType = Ascending, tempKeys = List.reverse model.keys, tempFolders = List.reverse model.folders }, Cmd.none )
                _ ->
                    let
                        keys = List.sortBy .lastModified model.tempKeys
                        folders = List.sortBy .lastModified model.tempFolders
                    in
                    ( { model | sortAttribute = Time, sortType = Ascending, tempKeys = keys, tempFolders = folders }, Cmd.none )

        ClickedLock ->
            ( model, Navigation.reload )



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
                                    , onClick ClickedLock
                                    ]
                                    [ text "Lock" ]
                                , a
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
                                                        decrypted = (String.split "/" (bucket ++ "/" ++ model.currentDir.dirDecrypted))
                                                        encrypted = (String.split "/" (bucket ++ "/" ++ model.currentDir.dirEncrypted))
                                                        indexedDirs = (List.indexedMap Tuple.pair (List.map2 Tuple.pair decrypted encrypted))
                                                    in
                                                    List.map (viewFilePath indexedDirs model) indexedDirs
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
                                                    []
                                                    [ text "Name" ]
                                                , option
                                                    []
                                                    [ text "Size" ]
                                                , option
                                                    []
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
                                                [ case model.sortAttribute of
                                                    Name -> Attr.class "is-current-sort is-sortable"
                                                    _ -> Attr.class "is-sortable"
                                                ]
                                                [ div
                                                    [ Attr.class "th-wrap"
                                                    , onClick ClickedSortName
                                                    ]
                                                    [ text "Name", span
                                                        [ Attr.class "icon is-small"
                                                        , case model.sortAttribute of
                                                            Name -> Attr.class ""
                                                            _ -> Attr.style "display" "none"
                                                        ]
                                                        [ i
                                                            (viewSortArrow model)
                                                            []
                                                        ]
                                                    ]
                                                ]
                                            , th
                                                [ case model.sortAttribute of
                                                  Size -> Attr.class "is-current-sort is-sortable"
                                                  _ -> Attr.class "is-sortable"
                                                , Attr.style "width" "150px"
                                                ]
                                                [ div
                                                    [ Attr.class "th-wrap is-numeric"
                                                    , onClick ClickedSortSize
                                                    ]
                                                    [ text "Size", span
                                                        [ Attr.class "icon is-small"
                                                        , case model.sortAttribute of
                                                            Size -> Attr.class ""
                                                            _ -> Attr.style "display" "none"
                                                        ]
                                                        [ i
                                                            (viewSortArrow model)
                                                            []
                                                        ]
                                                    ]
                                                ]
                                            , th
                                                [ case model.sortAttribute of
                                                  Time -> Attr.class "is-current-sort is-sortable"
                                                  _ -> Attr.class "is-sortable"
                                                , Attr.style "width" "200px"
                                                ]
                                                [ div
                                                    [ Attr.class "th-wrap is-numeric"
                                                    , onClick ClickedSortTime
                                                    ]
                                                    [ text "Time", span
                                                        [ Attr.class "icon is-small"
                                                        , case model.sortAttribute of
                                                            Time -> Attr.class ""
                                                            _ -> Attr.style "display" "none"
                                                        ]
                                                        [ i
                                                            (viewSortArrow model)
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
                                                            (viewSortArrow model)
                                                            []
                                                        ]
                                                    ]
                                                ]
                                            ]
                                        ]
                                    , tbody []
                                        (if List.length model.tempKeys /= 0 then
                                             (List.append
                                                 (List.append (viewBack model) (List.map (viewFolderItem shared model) model.tempFolders))
                                                 (List.map (viewFileItem shared model) model.tempKeys)
                                             )
                                         else
                                             [div [] []]
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
                        , viewFooter
                        ]
                    ]
                ]
            ]
        ]

viewSortArrow: Model -> List(Attribute Msg)
viewSortArrow model =
    [ case model.sortType of
        Ascending -> Attr.class "fas fa-arrow-down"
        Descending -> Attr.class "fas fa-arrow-up"
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

viewFilePath: List((Int, (String, String))) -> Model -> (Int, (String, String)) -> Html Msg
viewFilePath listDirs model dir =
    let
        --fullPathEncrypted = String.join "/" (List.map (\t -> Tuple.second (Tuple.first t)) (List.filter (\t -> (Tuple.first (Tuple.first t)) <= (Tuple.first dir)) listDirs))
        removeBucketName = List.drop 1 listDirs
        fullPathEncrypted = (String.join "/" (List.map (\t -> Tuple.second (Tuple.second t)) (List.filter (\t -> (Tuple.first t) <= (Tuple.first dir)) removeBucketName))) ++ "/"
        fullPathDecrypted = (String.join "/" (List.map (\t -> Tuple.first (Tuple.second t)) (List.filter (\t -> (Tuple.first t) <= (Tuple.first dir)) removeBucketName))) ++ "/"
    in
    li
    [ Attr.attribute "data-v-081c0a81" ""
    ]
    [ a
        [ Attr.attribute "data-v-081c0a81" ""
        , Attr.href "#"
        , onClick (ClickedFilePath (fullPathEncrypted, fullPathDecrypted))
        ]
        [ if model.fileNameEncrypted then
            text (Tuple.second (Tuple.second dir))
          else
            text (Tuple.first (Tuple.second dir))
        ]
    ]


viewFileItem: Shared.Model -> Model -> KeyInfoDecrypted -> Html Msg
viewFileItem shared model key =
    if String.contains model.currentDir.dirDecrypted key.keyDecrypted then
        let
            name = String.replace model.currentDir.dirDecrypted "" key.keyDecrypted
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
                                let
                                    encryptedFileNameAsList = String.split "/" key.keyEncrypted
                                    currentDirAsList = String.split "/" model.currentDir.dirDecrypted
                                    onlyName = List.drop ((List.length currentDirAsList) - 1) encryptedFileNameAsList
                                in
                                case List.head onlyName of
                                    Just head -> head
                                    Nothing -> key.keyEncrypted
                            else
                                String.replace model.currentDir.dirDecrypted "" key.keyDecrypted )
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
                (viewDropdown shared model key False)
            ]
        ]

viewDropdown: Shared.Model -> Model -> KeyInfoDecrypted -> Bool -> List (Html Msg)
viewDropdown shared model key isAFolder =
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
                [ if not isAFolder then
                    a
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
                 else
                    div [] []
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
                , if not isAFolder then
                    a [ Attr.attribute "data-v-081c0a81" ""
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
                 else
                    div [] []
                ]
            ]
        ]
    ]

viewFolderItem: Shared.Model -> Model -> KeyInfoDecrypted -> Html Msg
viewFolderItem shared model key =
    if String.contains model.currentDir.dirDecrypted key.keyDecrypted then
        let
            tempFolder = String.replace model.currentDir.dirDecrypted "" key.keyDecrypted
        in

        if model.currentDir.dirDecrypted /= key.keyDecrypted && (List.length (String.split "/" tempFolder)) == 2 then
            viewFolder shared model key
        else
            div [] []
    else
        div [] []

viewFolder: Shared.Model -> Model -> KeyInfoDecrypted -> Html Msg
viewFolder shared model key =
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
                , onClick (ClickedFolder key)
                ]
                [ text (if model.fileNameEncrypted then
                            let
                                encryptedFolderNameAsList = String.split "/" key.keyEncrypted
                                encryptedFolderNameWithoutTailingWhiteSpace = List.take (List.length encryptedFolderNameAsList - 1) encryptedFolderNameAsList
                                currentDirAsList = String.split "/" model.currentDir.dirDecrypted
                                onlyName = List.drop ((List.length currentDirAsList) - 1) encryptedFolderNameWithoutTailingWhiteSpace
                            in
                            case List.head onlyName of
                                Just head -> head
                                Nothing -> key.keyEncrypted
                        else
                            let
                                name = String.replace model.currentDir.dirDecrypted "" key.keyDecrypted
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
        ]
        [ span []
            (viewDropdown shared model key True)
        ]
    ]

viewBack: Model -> List (Html Msg)
viewBack model =
    if model.currentDir.dirDecrypted == "" then
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
