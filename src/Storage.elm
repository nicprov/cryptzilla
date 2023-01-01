port module Storage exposing
    ( Storage
    , storageToJson
    , storageFromJson
    , storageDecoder
    , init
    , onChange
    , signIn
    , signOut
    )

import Json.Decode as Decode exposing (Decoder, decodeValue, field, int, map, map2, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value, encode, list, string)
import S3 as S3
import S3.Types
import List exposing (concatMap)


-- Model

type alias Storage =
    { account: Maybe S3.Types.Account
    , password: String
    , salt: String
    , encryptionKey: String
    }

-- Ports

port save: Decode.Value -> Cmd msg
port load: (Decode.Value -> msg) -> Sub msg


-- Convert to JSON

storageToJson: Storage -> Decode.Value
storageToJson storage =
    case storage.account of
        Just account ->
            Encode.object
                [ ("account", S3.encodeAccount account)
                , ("rclonePassword", Encode.string storage.password)
                , ("rcloneSalt", Encode.string storage.salt)
                ]
        Nothing ->
            Encode.object []

-- Convert from JSON

storageFromJson: Decode.Value -> Storage
storageFromJson json =
    json
        |> Decode.decodeValue storageDecoder
        |> Result.withDefault init


-- Decoders

storageDecoder: Decoder Storage
storageDecoder =
    Decode.succeed Storage
        |> required "account" (nullable S3.accountDecoder)
        |> required "rclonePassword" Decode.string
        |> required "rcloneSalt" Decode.string
        |> hardcoded ""

-- Auth

signIn: S3.Types.Account -> String -> String -> String -> Storage -> Cmd msg
signIn account password salt encryptionKey storage =
    { storage | account = Just account, password = password, salt = salt, encryptionKey = encryptionKey }
        |> storageToJson
        |> save

signOut: Storage -> Cmd msg
signOut storage =
    { storage | account = Nothing, password = "", salt = ""}
        |> storageToJson
        |> save


-- Init

init: Storage
init =
    { account = Nothing
    , password = ""
    , salt = ""
    , encryptionKey = ""
    }

-- Listen for storage updates

onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> storageFromJson json |> fromStorage)
