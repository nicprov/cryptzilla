port module Storage exposing
    ( Storage
    , storageToJson
    , storageFromJson
    , storageDecoder
    , init
    , onChange
    , signIn
    , signOut
    , authenticate
    , decrypt
    )

import Json.Decode as Decode exposing (Decoder, decodeValue, field, int, map, map2, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value, encode, list, string)
import S3 as S3
import S3.Types
import List exposing (concatMap)
import Crypto.Strings as Strings exposing (decrypt, encrypt)
import Random exposing (Seed, initialSeed)

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
                , ("encryptionKey", Encode.string storage.encryptionKey)
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
        |> required "encryptionKey" Decode.string

-- Auth

signIn: S3.Types.Account -> String -> String -> String -> Storage -> Cmd msg
signIn account password salt encryptionKey storage =
    let
        tmpStorage = { storage | account = Just account, password = password, salt = salt, encryptionKey = encryptionKey }
        encryptedStorage = encrypt tmpStorage
    in
    encryptedStorage
        |> storageToJson
        |> save
    --{ storage | account = Just account, password = password, salt = salt, encryptionKey = encryptionKey }
    --    |> storageToJson
    --    |> save

signOut: Storage -> Cmd msg
signOut storage =
    { storage | account = Nothing, password = "", salt = "", encryptionKey = ""}
        |> storageToJson
        |> save


authenticate: S3.Types.Account -> String -> String -> String -> Storage -> Cmd msg
authenticate account password salt encryptionKey storage =
    { storage | account = Just account, password = password, salt = salt, encryptionKey = encryptionKey }
        |> storageToJson
        |> save

-- Encrypt storage
encrypt: Storage -> Storage
encrypt storage =
    case storage.account of
        Just acc ->
            case Strings.encrypt (initialSeed 0) storage.encryptionKey acc.accessKey of -- encrypt access key
                Ok encryptedAccessKey ->
                    case Strings.encrypt (initialSeed 0) storage.encryptionKey acc.secretKey of -- encrypt secret key
                        Ok encryptedSecretKey ->
                            case Strings.encrypt (initialSeed 0) storage.encryptionKey storage.password of -- encrypt rclone password
                                Ok encryptedPassword ->
                                    case Strings.encrypt (initialSeed 0) storage.encryptionKey storage.salt of -- encrypt rclone salt
                                        Ok encryptedSalt ->
                                            let
                                                tmpAccount = acc
                                                newAccount = { tmpAccount | accessKey = Tuple.first encryptedAccessKey, secretKey = Tuple.first encryptedSecretKey }
                                            in
                                            { storage | account = Just newAccount, password = Tuple.first encryptedPassword, salt = Tuple.first encryptedSalt }
                                        Err _ ->
                                            storage
                                Err _ ->
                                    storage
                        Err _ ->
                            storage
                Err _ ->
                    storage
        Nothing ->
            storage


-- Decrypt storage
decrypt: Storage -> Storage
decrypt storage =
    case storage.account of
        Just acc ->
            case Strings.decrypt storage.encryptionKey acc.accessKey of -- decrypt access key
                Ok decryptedAccessKey ->
                    case Strings.decrypt storage.encryptionKey acc.secretKey of -- decrypt secret key
                        Ok decryptedSecretKey ->
                            case Strings.decrypt storage.encryptionKey storage.password of -- decrypt rclone password
                                Ok decryptedPassword ->
                                    case Strings.decrypt storage.encryptionKey storage.salt of -- decrypt rclone salt
                                        Ok decryptedSalt ->
                                            let
                                                tmpAccount = acc
                                                newAccount = { tmpAccount | accessKey = decryptedAccessKey, secretKey = decryptedSecretKey }
                                            in
                                            { storage | account = Just newAccount, password = decryptedPassword, salt = decryptedSalt }
                                        Err _ ->
                                            storage
                                Err _ ->
                                    storage
                        Err _ ->
                            storage
                Err _ ->
                    storage
        Nothing ->
            storage

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
