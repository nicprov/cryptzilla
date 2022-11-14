module Domain.Credentials exposing (Credentials, credDecoder, credEncoder)

import Json.Decode as Decode exposing (Decoder, Value, bool, oneOf, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode

type alias Credentials =
    { accessKey: String
    , endpoint: String
    , secret: String
    }


credDecoder : Decoder Credentials
credDecoder =
    Decode.succeed Credentials
        |> required "accessKey" string
        |> required "endpoint" string
        |> required "secret" string

credEncoder : Maybe Credentials -> Value
credEncoder cred =
    case cred of
        Just cred_ ->
            Encode.object
                [ ( "accessKey", Encode.string cred_.accessKey)
                , ( "endpoint", Encode.string cred_.endpoint)
                , ( "secret", Encode.string cred_.secret)
                ]
        Nothing ->
            Encode.object []