port module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , decryptKeyList
    , decryptedKeyList
    , decryptFile
    , decryptedFile
    , encryptFile
    , encryptedFile
    , encryptFileName
    , encryptedFileName
    , KeyListDescriptionMessage
    , FileDescriptionMessage
    , KeyListDecrypted
    , KeyInfoDecrypted
    , EncryptedFile
    )

import Bytes exposing (Bytes)
import Bytes.Decode as Decode
import Gen.Route
import Json.Decode as Json exposing (Decoder)
import S3.Types
import Request exposing (Request)
import Storage exposing (Storage)


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage
    }

type alias KeyListDescriptionMessage =
    { keyList: S3.Types.KeyList
    , key: String
    , salt: String
    }

type alias FileDescriptionMessage =
    { file: String
    , name: String
    , key: String
    , salt: String
    }

type alias KeyInfoDecrypted =
    { keyEncrypted : S3.Types.Key
    , keyDecrypted: S3.Types.Key
    , lastModified : String
    , eTag : String
    , size : Int
    , storageClass : S3.Types.StorageClass
    , owner : Maybe S3.Types.Owner
    }

type alias KeyListDecrypted =
    { name : String
    , prefix : Maybe String
    , marker : Maybe String
    , nextMarker : Maybe String
    , maxKeys : Int
    , isTruncated : Bool
    , keys : List KeyInfoDecrypted
    , error: String
    }

type alias EncryptedFile =
    { encryptedFile: String
    , encryptedPath: String
    , sha256: String
    , error: String
    }

type Msg
    = StorageUpdated Storage



-- Ports
port encryptFileName: FileDescriptionMessage -> Cmd msg
port encryptedFileName: ((String, String) -> msg) -> Sub msg

port encryptFile: FileDescriptionMessage -> Cmd msg
port encryptedFile: (EncryptedFile -> msg) -> Sub msg

port decryptFile: FileDescriptionMessage -> Cmd msg
port decryptedFile: ((String, String) -> msg) -> Sub msg

port decryptKeyList: KeyListDescriptionMessage -> Cmd msg
port decryptedKeyList: (KeyListDecrypted -> msg) -> Sub msg

init : Request -> Flags -> ( Model, Cmd Msg )
init req flags =
    let
        model =  { storage = Storage.storageFromJson flags }
    in
    ( model
    , if model.storage.account /= Nothing && req.route == Gen.Route.Login then
        Request.replaceRoute Gen.Route.Home_ req
      else if model.storage.account == Nothing && req.route == Gen.Route.Home_ then
        Request.replaceRoute Gen.Route.Login req
      else if req.route == Gen.Route.Logout then
        Request.replaceRoute Gen.Route.Login req
      else
        Cmd.none
    )

update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        StorageUpdated storage ->
            ( { model | storage = storage }
            , Cmd.none
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Storage.onChange StorageUpdated