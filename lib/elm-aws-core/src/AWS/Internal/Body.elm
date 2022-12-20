module AWS.Internal.Body exposing
    ( Body
    , empty
    , explicitMimetype
    , json
    , bytes
    , string
    , toHttp
    , toString
    )

import Bytes exposing (Bytes)
import Hex.Convert
import Http
import Json.Encode
import SHA256

type Body
    = Empty
    | Json Json.Encode.Value
    | String String String
    | Bytes String Bytes String


toHttp : Body -> Http.Body
toHttp body =
    case body of
        Empty ->
            Http.emptyBody

        Json value ->
            Http.jsonBody value

        String mimetype val ->
            Http.stringBody mimetype val

        Bytes mimetype val _ ->
            Http.bytesBody mimetype val


explicitMimetype : Body -> Maybe String
explicitMimetype body =
    case body of
        String typ _ ->
            Just typ

        _ ->
            Nothing


toString : Body -> String
toString body =
    case body of
        Json value ->
            Json.Encode.encode 0 value

        Empty ->
            ""

        String _ val ->
            val

        Bytes _ _ sha256 ->
            "UNSIGNED-PAYLOAD"

empty : Body
empty =
    Empty


json : Json.Encode.Value -> Body
json =
    Json


string : String -> String -> Body
string =
    String

bytes: String -> Bytes -> String -> Body
bytes =
    Bytes