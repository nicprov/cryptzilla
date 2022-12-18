module AWS.Http exposing
    ( send, sendBytes, sendUnsigned
    , Method(..), Path, Request, BytesRequest
    , request, requestBytes
    , Body, MimeType
    , emptyBody, stringBody, jsonBody
    , addHeaders, addQuery, addHeadersBytes
    , ResponseDecoder
    , fullDecoder, jsonFullDecoder, stringBodyDecoder, bytesBodyDecoder, jsonBodyDecoder, constantDecoder
    , Error(..), AWSAppError, awsAppErrDecoder, awsAppErrDecoderBytes, neverAppErrDecoder
    )

{-| Handling of HTTP requests to AWS Services.


# Tasks for sending requests to AWS.

@docs send, sendUnsigned


# Build a Request

@docs Method, Path, Request
@docs request


# Build the HTTP Body of a Request

@docs Body, MimeType
@docs emptyBody, stringBody, jsonBody


# Add headers or query parameters to a Request

@docs addHeaders, addQuery


# Build decoders to interpret the response.

@docs ResponseDecoder
@docs fullDecoder, jsonFullDecoder, stringBodyDecoder, jsonBodyDecoder, constantDecoder


# HTTP and Application level errors

@docs Error, AWSAppError, awsAppErrDecoder, neverAppErrDecoder

-}

import AWS.Config exposing (Protocol(..), Signer(..))
import AWS.Credentials exposing (Credentials)
import AWS.Internal.Body
import AWS.Internal.Error as Error
import AWS.Internal.Request exposing (BytesRequest, ErrorDecoder, BytesErrorDecoder, Request, ResponseDecoder)
import AWS.Internal.Service as Service exposing (Service)
import AWS.Internal.Unsigned as Unsigned
import AWS.Internal.V4 as V4
import Bytes exposing (Bytes)
import Http exposing (Metadata)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as JDP
import Json.Encode
import Task exposing (Task)
import Time exposing (Posix)



--=== Tasks for sending requests to AWS.


{-| Signs and sends a `Request` to a `Service`.
-}
send :
    Service
    -> Credentials
    -> Request err a
    -> Task.Task (Error err) a
send service credentials req =
    let
        prepareRequest : Request err a -> Request err a
        prepareRequest innerReq =
            case service.protocol of
                JSON ->
                    addHeaders
                        [ ( "x-amz-target", service.targetPrefix ++ "." ++ innerReq.name ) ]
                        innerReq

                _ ->
                    innerReq

        signWithTimestamp : Request err a -> Posix -> Task (Error.Error err) a
        signWithTimestamp innerReq posix =
            case service.signer of
                SignV4 ->
                    V4.sign service credentials posix innerReq

                SignS3 ->
                    Task.fail (Http.BadBody "TODO: S3 Signing Scheme not implemented." |> Error.HttpError)
    in
    Time.now
        |> Task.andThen (prepareRequest req |> signWithTimestamp)
        |> Task.mapError internalErrToErr

sendBytes :
    Service
    -> Credentials
    -> BytesRequest err a
    -> Task.Task (Error err) a
sendBytes service credentials req =
    let
        prepareRequest : BytesRequest err a -> BytesRequest err a
        prepareRequest innerReq =
            case service.protocol of
                JSON ->
                    addHeadersBytes
                        [ ( "x-amz-target", service.targetPrefix ++ "." ++ innerReq.name ) ]
                        innerReq

                _ ->
                    innerReq

        signWithTimestamp : BytesRequest err a -> Posix -> Task (Error.Error err) a
        signWithTimestamp innerReq posix =
            case service.signer of
                SignV4 ->
                    V4.signBytes service credentials posix innerReq

                SignS3 ->
                    Task.fail (Http.BadBody "TODO: S3 Signing Scheme not implemented." |> Error.HttpError)
    in
    Time.now
        |> Task.andThen (prepareRequest req |> signWithTimestamp)
        |> Task.mapError internalErrToErr


{-| Sends a `Request` to a `Service` without signing it.
-}
sendUnsigned :
    Service
    -> Request err a
    -> Task.Task (Error err) a
sendUnsigned service req =
    let
        prepareRequest : Request err a -> Request err a
        prepareRequest innerReq =
            case service.protocol of
                JSON ->
                    addHeaders
                        [ ( "x-amz-target", service.targetPrefix ++ "." ++ innerReq.name ) ]
                        innerReq

                _ ->
                    innerReq

        withTimestamp : Request err a -> Posix -> Task (Error.Error err) a
        withTimestamp innerReq posix =
            Unsigned.prepare service posix innerReq
    in
    Time.now
        |> Task.andThen (prepareRequest req |> withTimestamp)
        |> Task.mapError internalErrToErr



--=== Build a request


{-| Holds an unsigned AWS HTTP request.
-}
type alias Request err a =
    AWS.Internal.Request.Request err a

type alias BytesRequest err a =
    AWS.Internal.Request.BytesRequest err a

{-| HTTP request methods.
-}
type Method
    = DELETE
    | GET
    | HEAD
    | OPTIONS
    | POST
    | PUT


{-| Request path.
-}
type alias Path =
    String


{-| Creates an unsigned HTTP request to an AWS service.
-}
request :
    String
    -> Method
    -> Path
    -> Body
    -> ResponseDecoder a
    -> ErrorDecoder err
    -> Request err a
request name method path body decoder errorDecoder =
    AWS.Internal.Request.unsigned name (methodToString method) path body decoder errorDecoder

requestBytes :
    String
    -> Method
    -> Path
    -> Body
    -> BytesResponseDecoder a
    -> BytesErrorDecoder err
    -> BytesRequest err a
requestBytes name method path body decoder errorDecoder =
    AWS.Internal.Request.bytesUnsigned name (methodToString method) path body decoder errorDecoder

--=== Build th HTTP Body of a Request


{-| Holds a request body.
-}
type alias Body =
    AWS.Internal.Body.Body


{-| MIME type.

See <https://en.wikipedia.org/wiki/Media_type>

-}
type alias MimeType =
    String


{-| Create an empty body.
-}
emptyBody : Body
emptyBody =
    AWS.Internal.Body.empty


{-| Create a body containing a JSON value.

This will automatically add the `Content-Type: application/json` header.

-}
jsonBody : Json.Encode.Value -> Body
jsonBody =
    AWS.Internal.Body.json


{-| Create a body with a custom MIME type and the given string as content.

    stringBody "text/html" "<html><body><h1>Hello</h1></body></html>"

-}
stringBody : MimeType -> String -> Body
stringBody =
    AWS.Internal.Body.string



--=== Add headers or query parameters to a Request


{-| Appends headers to an AWS HTTP unsigned request.

See the `AWS.KVEncode` for encoder functions to build the headers with.

-}
addHeaders : List ( String, String ) -> Request err a -> Request err a
addHeaders headers req =
    { req | headers = List.append req.headers headers }


addHeadersBytes : List ( String, String ) -> BytesRequest err a -> BytesRequest err a
addHeadersBytes headers req =
    { req | headers = List.append req.headers headers }

{-| Appends query arguments to an AWS HTTP unsigned request.

See the `AWS.KVEncode` for encoder functions to build the query parameters with.

-}
addQuery : List ( String, String ) -> Request err a -> Request err a
addQuery query req =
    { req | query = List.append req.query query }



--=== Build decoders to interpret the response.


{-| Decoders that interpret responses.
-}
type alias ResponseDecoder a =
    AWS.Internal.Request.ResponseDecoder a

type alias BytesResponseDecoder a =
    AWS.Internal.Request.BytesResponseDecoder a

{-| A full decoder for the response that can look at the status code, metadata
including headers and so on. The body is presented as a `String` for parsing.

It is possible to report an error as a String when interpreting the response, and
this will be mapped onto `Http.BadBody` when present.

-}
fullDecoder : (Metadata -> String -> Result String a) -> ResponseDecoder a
fullDecoder decodeFn =
    \metadata body ->
        decodeFn metadata body


{-| A full JSON decoder for the response that can look at the status code, metadata
including headers and so on. The body is presented as a JSON `Value` for decoding.

Any decoder error is mapped onto `Http.BadBody` as a `String` when present using
`Decode.errorToString`.

-}
jsonFullDecoder : (Metadata -> Decoder a) -> ResponseDecoder a
jsonFullDecoder decodeFn =
    \metadata body ->
        case Decode.decodeString (decodeFn metadata) body of
            Ok val ->
                Ok val

            Err err ->
                Decode.errorToString err |> Err


{-| A decoder for the response that uses only the body presented as a `String`
for parsing.

It is possible to report an error as a String when interpreting the response, and
this will be mapped onto `Http.BadBody` when present.

Note that this decoder is only used when the response is Http.GoodStatus\_. An
Http.BadStatus\_ is always mapped to Http.BadStatus without attempting to decode
the body. If you need to handle things that Elm HTTP regards as BadStatus\_, use
one of the 'full' decoders.

-}
stringBodyDecoder : (String -> Result String a) -> ResponseDecoder a
stringBodyDecoder decodeFn =
    \metadata body ->
        decodeFn body

bytesBodyDecoder : (Bytes -> Result Bytes a) -> BytesResponseDecoder a
bytesBodyDecoder decodeFn =
    \metadata body ->
        decodeFn body

{-| A decoder for the response that uses only the body presented as a JSON `Value`
for decoding.

Any decoder error is mapped onto `Http.BadBody` as a `String` when present using
`Decode.errorToString`.

Note that this decoder is only used when the response is Http.GoodStatus\_. An
Http.BadStatus\_ is always mapped to Http.BadStatus without attempting to decode
the body. If you need to handle things that Elm HTTP regards as BadStatus\_, use
one of the 'full' decoders.

-}
jsonBodyDecoder : Decoder a -> ResponseDecoder a
jsonBodyDecoder decodeFn =
    \metadata body ->
        case Decode.decodeString decodeFn body of
            Ok val ->
                Ok val

            Err err ->
                Decode.errorToString err |> Err


{-| Not all AWS service produce a response that contains useful information.

The `constantDecoder` is helpful in those situations and just produces whatever
value you give it once AWS has responded.

Note that this decoder is only used when the response is Http.GoodStatus\_. An
Http.BadStatus\_ is always mapped to Http.BadStatus without attempting to decode
the body. If you need to handle things that Elm HTTP regards as BadStatus\_, use
one of the 'full' decoders.

-}
constantDecoder : a -> ResponseDecoder a
constantDecoder val =
    \metadata _ ->
        Ok val



-- Error Reporting


{-| The HTTP calls made to AWS can produce errors in two ways. The first is the
normal `Http.Error` responses. The second is an error message at the application
level from one of the AWS service endpoints.

Only some endpoints can produce application level errors, in which case their error
type can be given as `Never`.

-}
type Error err
    = HttpError Http.Error
    | AWSError err


{-| AWS application level errors consist of a 'type' giving the name of an 'exception'
and possibly a message string.
-}
type alias AWSAppError =
    { type_ : String
    , message : Maybe String
    , statusCode : Int
    }


{-| The default decoder for the standard AWS application level errors.

Use this, or define your own decoder to interpret these errors.

-}
awsAppErrDecoder : ErrorDecoder AWSAppError
awsAppErrDecoder metadata body =
    let
        bodyDecoder =
            Decode.succeed
                (\type_ message ->
                    { type_ = type_
                    , message = message
                    , statusCode = metadata.statusCode
                    }
                )
                |> JDP.required "__type" Decode.string
                |> JDP.required "message" (Decode.maybe Decode.string)
    in
    Decode.decodeString bodyDecoder body
        |> Result.mapError (\_ -> body)

awsAppErrDecoderBytes : BytesErrorDecoder AWSAppError
awsAppErrDecoderBytes metadata body =
    let
        bodyDecoder =
            Decode.succeed
                (\type_ message ->
                    { type_ = type_
                    , message = message
                    , statusCode = metadata.statusCode
                    }
                )
                |> JDP.required "__type" Decode.string
                |> JDP.required "message" (Decode.maybe Decode.string)
    in
    Decode.decodeString bodyDecoder "body"
        |> Result.mapError (\_ -> body)

{-| The never error decoder for AWS endpoints that are not expected to produce
any application level errors.

If this decoder were to be called, it will simply return the body undecoded as
an error and this should be mapped onto Http.BadBody.

-}
neverAppErrDecoder : ErrorDecoder Never
neverAppErrDecoder _ body =
    Err body


internalErrToErr : Error.Error a -> Error a
internalErrToErr error =
    case error of
        Error.HttpError err ->
            HttpError err

        Error.AWSError err ->
            AWSError err


methodToString : Method -> String
methodToString meth =
    case meth of
        DELETE ->
            "DELETE"

        GET ->
            "GET"

        HEAD ->
            "HEAD"

        OPTIONS ->
            "OPTIONS"

        POST ->
            "POST"

        PUT ->
            "PUT"
