port module Shared exposing
    ( Flags
    , Model
    , Msg
    , init
    , subscriptions
    , update
    , decryptKeyList
    , decryptedKeyList
    , DecryptionMessage
    )

import Gen.Route
import Json.Decode as Json
import Request exposing (Request)
import S3.Types
import Storage exposing (Storage)


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage
    }

type alias DecryptionMessage =
    { keyList: S3.Types.KeyList
    , key: String
    , salt: String
    }

type Msg
    = StorageUpdated Storage



-- Ports

port decryptKeyList: DecryptionMessage -> Cmd msg
port decryptedKeyList: (S3.Types.KeyList -> msg) -> Sub msg

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