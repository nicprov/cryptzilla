module AWS.Internal.Request exposing
    ( ErrorDecoder
    , BytesErrorDecoder
    , Request
    , ResponseDecoder
    , BytesResponseDecoder
    , BytesRequest
    , unsigned
    , bytesUnsigned
    )

{-| Internal representation of a request.
-}

import AWS.Internal.Body as Body exposing (Body)
import AWS.Internal.Service as Service exposing (Service)
import Bytes exposing (Bytes)
import Http exposing (Error, Metadata, Response)
import Json.Decode exposing (Decoder)



-- Types from Http module for reference.
--
-- type Response body
--     = BadUrl_ String --> Map to BadUrl
--     | Timeout_ --> Map to Timeout
--     | NetworkError_ --> Map to NetworkError
--     | BadStatus_ Metadata body
--     | GoodStatus_ Metadata body
--
-- type alias Metadata =
--     { url : String
--     , statusCode : Int
--     , statusText : String
--     , headers : Dict String String
--     }
--
-- type Error
--     = BadUrl String
--     | Timeout
--     | NetworkError
--     | BadStatus Int
--     | BadBody String -- Overloaded. Should introduce custom error type?


type alias ResponseDecoder a =
    Metadata -> String -> Result String a

type alias BytesResponseDecoder a =
    Metadata -> Bytes -> Result Bytes a

type alias ErrorDecoder err =
    Metadata -> String -> Result String err

type alias BytesErrorDecoder err =
    Metadata -> Bytes -> Result Bytes err

type alias Request err a =
    { name : String
    , method : String
    , path : String
    , body : Body
    , headers : List ( String, String )
    , query : List ( String, String )
    , decoder : ResponseDecoder a
    , errorDecoder : ErrorDecoder err
    }

type alias BytesRequest err a =
    { name : String
    , method : String
    , path : String
    , body : Body
    , headers : List ( String, String )
    , query : List ( String, String )
    , decoder : BytesResponseDecoder a
    , errorDecoder : BytesErrorDecoder err
    }

unsigned :
    String
    -> String
    -> String
    -> Body
    -> ResponseDecoder a
    -> ErrorDecoder err
    -> Request err a
unsigned name method uri body decoder errorDecoder =
    { name = name
    , method = method
    , path = uri
    , body = body
    , headers = []
    , query = []
    , decoder = decoder
    , errorDecoder = errorDecoder
    }

bytesUnsigned :
    String
    -> String
    -> String
    -> Body
    -> BytesResponseDecoder a
    -> BytesErrorDecoder err
    -> BytesRequest err a
bytesUnsigned name method uri body decoder errorDecoder =
    { name = name
    , method = method
    , path = uri
    , body = body
    , headers = []
    , query = []
    , decoder = decoder
    , errorDecoder = errorDecoder
    }
