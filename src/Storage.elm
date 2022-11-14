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

import Domain.Credentials exposing (Credentials, credDecoder, credEncoder)
import Json.Decode as Decode exposing (Decoder, decodeValue, field, int, map, map2, nullable, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value, encode, list, string)
import List exposing (concatMap)

-- Model

type alias Storage =
    { credentials: Maybe Credentials
    }

-- Ports

port save: Decode.Value -> Cmd msg
port load: (Decode.Value -> msg) -> Sub msg


-- Convert to JSON

storageToJson: Storage -> Decode.Value
storageToJson storage =
    Encode.object
        [ ("credentials", credEncoder storage.credentials)
        ]


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
        |> required "credentials" (nullable credDecoder)


-- Auth

signIn: Credentials -> Storage -> Cmd msg
signIn cred storage =
    { storage | credentials = Just cred }
        |> storageToJson
        |> save

signOut: Storage -> Cmd msg
signOut storage =
    { storage | credentials = Nothing}
        |> storageToJson
        |> save


-- Init

init: Storage
init =
    { credentials = Nothing
    }

-- Listen for storage updates

onChange : (Storage -> msg) -> Sub msg
onChange fromStorage =
    load (\json -> storageFromJson json |> fromStorage)