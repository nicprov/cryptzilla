port module Storage exposing
    ( Storage
    , storageToJson
    , storageFromJson
    , storageDecoder
    , init
    , onChange
    , signIn
    , signOut
    , lock
    , authenticate
    , decrypt
    , encrypt
    )

import Json.Decode as Decode exposing (Decoder, decodeValue, field, int, map, map2, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value, encode, list, string)
import S3 as S3
import S3.Types
import Crypto.Strings as Strings exposing (decrypt, encrypt)
import Random exposing (Seed, initialSeed)
import String exposing (dropLeft, left)

-- Model

type alias Storage =
    { account: Maybe S3.Types.Account
    , password: String
    , salt: String
    , timeout: Int -- In seconds
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
                , ("timeout", Encode.int storage.timeout)
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
        |> required "timeout" Decode.int

-- Auth

signIn: String -> S3.Types.Account -> String -> String -> Int -> Storage -> Cmd msg
signIn encryptionKey account password salt timeout storage =
    let
        tmpStorage = { storage | account = Just account, password = password, salt = salt, timeout = timeout }
        encryptedStorage = encrypt encryptionKey tmpStorage
    in
    case encryptedStorage of
        Just encrypted ->
            encrypted
                |> storageToJson
                |> save

        Nothing ->
            Cmd.none

lock: String -> Storage -> Cmd msg
lock encryptionKey storage =
    let
        encryptedStorage = encrypt encryptionKey storage
    in
    case encryptedStorage of
        Just encrypted ->
            encrypted
                |> storageToJson
                |> save

        Nothing ->
            Cmd.none

signOut: Storage -> Cmd msg
signOut storage =
    { storage | account = Nothing, password = "", salt = ""}
        |> storageToJson
        |> save


authenticate: S3.Types.Account -> String -> String -> Storage -> Cmd msg
authenticate account password salt storage =
    { storage | account = Just account, password = password, salt = salt }
        |> storageToJson
        |> save

-- Encrypt storage
encrypt: String -> Storage -> Maybe Storage
encrypt encryptionKey storage =
    case storage.account of
        Just acc ->
            case Strings.encrypt (initialSeed 0) encryptionKey ("valid" ++ acc.accessKey) of -- encrypt access key
                Ok encryptedAccessKey ->
                    case Strings.encrypt (initialSeed 0) encryptionKey ("valid" ++ acc.secretKey) of -- encrypt secret key
                        Ok encryptedSecretKey ->
                            case Strings.encrypt (initialSeed 0) encryptionKey ("valid" ++ storage.password) of -- encrypt rclone password
                                Ok encryptedPassword ->
                                    case Strings.encrypt (initialSeed 0) encryptionKey ("valid" ++ storage.salt) of -- encrypt rclone salt
                                        Ok encryptedSalt ->
                                            let
                                                tmpAccount = acc
                                                newAccount = { tmpAccount | accessKey = Tuple.first encryptedAccessKey, secretKey = Tuple.first encryptedSecretKey }
                                            in
                                            Just { storage | account = Just newAccount, password = Tuple.first encryptedPassword, salt = Tuple.first encryptedSalt }
                                        Err _ ->
                                            Nothing
                                Err _ ->
                                    Nothing
                        Err _ ->
                            Nothing
                Err _ ->
                    Nothing
        Nothing ->
            Nothing


-- Decrypt storage
decrypt: String -> Storage -> Maybe Storage
decrypt encryptionKey storage =
    case storage.account of
        Just acc ->
            case Strings.decrypt encryptionKey acc.accessKey of -- decrypt access key
                Ok decryptedAccessKey ->
                    case Strings.decrypt encryptionKey acc.secretKey of -- decrypt secret key
                        Ok decryptedSecretKey ->
                            case Strings.decrypt encryptionKey storage.password of -- decrypt rclone password
                                Ok decryptedPassword ->
                                    case Strings.decrypt encryptionKey storage.salt of -- decrypt rclone salt
                                        Ok decryptedSalt ->
                                            let
                                                -- verify if the first 5 characters = valid, if not, will be gibberish (invalid passphrase)
                                                validVerificationAccessKey = left 5 decryptedAccessKey
                                                validVerificationSecretKey = left 5 decryptedSecretKey
                                                validVerificationPassword = left 5 decryptedPassword
                                                validVerificationSalt = left 5 decryptedSalt

                                                -- drop the validation keyword "valid" from the decrypted text
                                                strippedDecryptedAccessKey = dropLeft 5 decryptedAccessKey
                                                strippedDecryptedSecretKey = dropLeft 5 decryptedSecretKey
                                                strippedDecryptedPassword = dropLeft 5 decryptedPassword
                                                strippedDecryptedSalt = dropLeft 5 decryptedSalt

                                                tmpAccount = acc
                                                newAccount = { tmpAccount | accessKey = strippedDecryptedAccessKey, secretKey = strippedDecryptedSecretKey }
                                            in
                                            if validVerificationAccessKey == "valid" && validVerificationSecretKey == "valid" && validVerificationPassword == "valid" && validVerificationSalt == "valid" then
                                                Just { storage | account = Just newAccount, password = strippedDecryptedPassword, salt = strippedDecryptedSalt }
                                            else
                                                Nothing
                                        Err _ ->
                                            Nothing
                                Err _ ->
                                    Nothing
                        Err _ ->
                            Nothing
                Err _ ->
                    Nothing
        Nothing ->
            Nothing

-- Init

init: Storage
init =
    { account = Nothing
    , password = ""
    , salt = ""
    , timeout = 300000
    }

-- Listen for storage updates

onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> storageFromJson json |> fromStorage)
