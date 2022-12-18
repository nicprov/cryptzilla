--------------------------------------------------------------------
--
-- S3.elm
-- Elm client library for Amazon's S3 (Simple Storage Service)
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module S3 exposing
    ( Request
    , send, sendBytes
    , listKeys
    , getObject, getFullObject, getHeaders, getObjectWithHeaders, getBytesObject
    , putHtmlObject, putPublicObject, putObject
    , deleteObject
    , htmlBody, jsonBody, stringBody
    , addQuery, addHeaders
    , readAccounts, decodeAccounts, accountDecoder, encodeAccount
    , objectPath, parserRequest, stringRequest, requestUrl
    )

{-| Pure Elm client for the [AWS Simple Storage Service](https://aws.amazon.com/s3/) (S3) or [Digital Ocean Spaces](https://developers.digitalocean.com/documentation/spaces/).


# Types

@docs Request


# Turning a Request into a Task

@docs send


# Creating S3 requests

@docs listKeys
@docs getObject, getFullObject, getHeaders, getObjectWithHeaders
@docs putHtmlObject, putPublicObject, putObject
@docs deleteObject


# Creating Body values

@docs htmlBody, jsonBody, stringBody


# Adding queries and headers to a request

@docs addQuery, addHeaders


# Reading accounts into Elm

@docs readAccounts, decodeAccounts, accountDecoder, encodeAccount


# Low-level functions

@docs objectPath, parserRequest, stringRequest, requestUrl

-}

import AWS.Config as Config exposing (Endpoint(..))
import AWS.Credentials
    exposing
        ( Credentials
        , fromAccessKeys
        )
import AWS.Http
    exposing
        ( AWSAppError
        , Body
        , Method(..)
        , Path
        , emptyBody
        , request
        , requestBytes
        )
import AWS.Service as Service exposing (Service)
import Bytes exposing (Bytes)
import Dict exposing (Dict)
import Http exposing (Metadata)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE
import S3.Parser
    exposing
        ( parseListBucketResponse
        )
import S3.Types
    exposing
        ( Account
        , Bucket
        , CannedAcl(..)
        , Error(..)
        , Key
        , KeyList
        , Mimetype
        , Query
        , QueryElement(..)
        , StorageClass
        , aclToString
        )
import Task exposing (Task)


defaultAccountsUrl : String
defaultAccountsUrl =
    "accounts.json"


{-| Read JSON from a URL and turn it into a list of `Account`s.

If `Nothing` is passed for the first arg (the URL), will use the default of `"accounts.json"`.

You're not going to want to store the secret keys in this JSON in plain text anywhere but your development machine. I'll add support eventually for encryption of the accounts JSON.

Example JSON (the `buckets` are used only by the example code):

    [{"name": "Digital Ocean",
      "region": "nyc3",
      "is-digital-ocean": true,
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "buckets": ["bucket1","bucket2"]
     },
     {"name": "Amazon S3",
      "region": "us-east-1",
      "access-key": "<20-character access key>",
      "secret-key": "<40-character secret key>",
      "buckets": ["bucket3","bucket4","bucket5"]
     }
    ]

-}
readAccounts : Maybe String -> Task Error (List Account)
readAccounts maybeUrl =
    let
        url =
            case maybeUrl of
                Just u ->
                    u

                Nothing ->
                    defaultAccountsUrl

        getTask =
            getStringTask url
    in
    Task.andThen decodeAccountsTask <|
        Task.onError handleHttpError getTask


getStringTask : String -> Task Http.Error String
getStringTask url =
    Http.task
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , resolver = Http.stringResolver stringResponseToResult
        , timeout = Nothing
        }


stringResponseToResult : Http.Response String -> Result Http.Error String
stringResponseToResult response =
    case response of
        Http.BadUrl_ s ->
            Err <| Http.BadUrl s

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err <| Http.BadStatus metadata.statusCode

        Http.GoodStatus_ _ body ->
            Ok body


decodeAccountsTask : String -> Task Error (List Account)
decodeAccountsTask json =
    case decodeAccounts json of
        Ok accounts ->
            Task.succeed accounts

        Err error ->
            Task.fail error


handleHttpError : Http.Error -> Task Error String
handleHttpError error =
    Task.fail <| HttpError error


makeCredentials : Account -> Credentials
makeCredentials account =
    fromAccessKeys account.accessKey account.secretKey


{-| Encode an account as a JSON value.
-}
encodeAccount : Account -> JE.Value
encodeAccount account =
    JE.object <|
        List.concat
            [ [ ( "name", JE.string account.name ) ]
            , case account.region of
                Nothing ->
                    []

                Just region ->
                    [ ( "region", JE.string region ) ]
            , if account.isDigitalOcean then
                [ ( "is-digital-ocean", JE.bool True ) ]

              else
                []
            , [ ( "access-key", JE.string account.accessKey )
              , ( "secret-key", JE.string account.secretKey )
              , ( "buckets", JE.list JE.string account.buckets )
              ]
            ]


{-| A `Decoder` for the `Account` type.
-}
accountDecoder : Decoder Account
accountDecoder =
    JD.map6 Account
        (JD.field "name" JD.string)
        (JD.oneOf
            [ JD.field "region" (JD.nullable JD.string)
            , JD.succeed Nothing
            ]
        )
        (JD.oneOf
            [ JD.field "is-digital-ocean" JD.bool
            , JD.succeed False
            ]
        )
        (JD.field "access-key" JD.string)
        (JD.field "secret-key" JD.string)
        (JD.field "buckets" (JD.list JD.string))


accountsDecoder : Decoder (List Account)
accountsDecoder =
    JD.list accountDecoder


{-| Decode a JSON string encoding a list of `Account`s
-}
decodeAccounts : String -> Result Error (List Account)
decodeAccounts json =
    case JD.decodeString accountsDecoder json of
        Err s ->
            Err <| DecodeError (JD.errorToString s)

        Ok accounts ->
            Ok accounts


awsEndpointPrefix : String
awsEndpointPrefix =
    "s3"


apiVersion : Config.ApiVersion
apiVersion =
    "2017-07-10"


protocol : Config.Protocol
protocol =
    Config.REST_XML


{-| Make an `AWS.Service.Service` for an `S3.Account`.

Sometimes useful for the `hostResolver`.

-}
makeService : Account -> Service
makeService { region, isDigitalOcean } =
    let
        prefix =
            -- Changed by `send` to the bucket for Digital Ocean.
            awsEndpointPrefix

        service =
            case region of
                Nothing ->
                    Service.service <|
                        Config.defineGlobal
                            prefix
                            apiVersion
                            protocol
                            Config.SignV4

                Just reg ->
                    Service.service <|
                        Config.defineRegional
                            prefix
                            apiVersion
                            protocol
                            Config.SignV4
                            reg
    in
    if isDigitalOcean then
        -- regionResolver's default works for Digigal Ocean.
        { service | hostResolver = digitalOceanHostResolver }

    else
        service


digitalOceanHostResolver : Endpoint -> String -> String
digitalOceanHostResolver endpoint prefix =
    case endpoint of
        GlobalEndpoint ->
            prefix ++ ".digitaloceanspaces.com"

        RegionalEndpoint rgn ->
            prefix ++ "." ++ rgn ++ ".digitaloceanspaces.com"


{-| A request that can be turned into a Task by `S3.send`.

`a` is the type of the successful `Task` result from `S3.send`.

-}
type alias Request a =
    AWS.Http.Request AWSAppError a

type alias BytesRequest a =
    AWS.Http.BytesRequest AWSAppError a

fudgeRequest : Account -> Request a -> ( Service, Request a )
fudgeRequest account request =
    let
        service =
            makeService account
    in
    if not account.isDigitalOcean then
        ( service, request )

    else
        let
            ( bucket, key ) =
                pathBucketAndKey request.path
        in
        ( { service | endpointPrefix = bucket }
        , { request | path = "/" ++ key }
        )

fudgeBytesRequest : Account -> BytesRequest a -> ( Service, BytesRequest a )
fudgeBytesRequest account request =
    let
        service =
            makeService account
    in
    if not account.isDigitalOcean then
        ( service, request )

    else
        let
            ( bucket, key ) =
                pathBucketAndKey request.path
        in
        ( { service | endpointPrefix = bucket }
        , { request | path = "/" ++ key }
        )

{-| Create a `Task` to send a signed request over the wire.
-}
send : Account -> Request a -> Task Error a
send account request =
    let
        ( service, req ) =
            fudgeRequest account request

        credentials =
            makeCredentials account

        req2 =
            addHeaders [ AnyQuery "Accept" "*/*" ] req
    in
    AWS.Http.send service credentials req2
        |> Task.onError
            (\error ->
                (case error of
                    AWS.Http.HttpError err ->
                        HttpError err

                    AWS.Http.AWSError err ->
                        AWSError err
                )
                    |> Task.fail
            )

sendBytes : Account -> BytesRequest a -> Task Error a
sendBytes account request =
    let
        ( service, req ) =
            fudgeBytesRequest account request

        credentials =
            makeCredentials account

        req2 =
            addHeadersBytes [ AnyQuery "Accept" "*/*" ] req
    in
    AWS.Http.sendBytes service credentials req2
        |> Task.onError
            (\error ->
                (case error of
                    AWS.Http.HttpError err ->
                        HttpError err

                    AWS.Http.AWSError err ->
                        AWSError err
                )
                    |> Task.fail
            )

formatQuery : Query -> List ( String, String )
formatQuery query =
    let
        formatElement =
            \element ->
                case element of
                    AnyQuery k v ->
                        ( k, v )

                    Delimiter s ->
                        ( "delimiter", s )

                    Marker s ->
                        ( "marker", s )

                    MaxKeys cnt ->
                        ( "max-keys", String.fromInt cnt )

                    Prefix s ->
                        ( "prefix", s )

                    XAmzAcl acl ->
                        ( "x-amz-acl", aclToString acl )
    in
    List.map formatElement query


{-| Add headers to a `Request`.
-}
addHeaders : Query -> Request a -> Request a
addHeaders headers req =
    AWS.Http.addHeaders (formatQuery headers) req


addHeadersBytes : Query -> BytesRequest a -> BytesRequest a
addHeadersBytes headers req =
    AWS.Http.addHeadersBytes (formatQuery headers) req

{-| Add query parameters to a `Request`.
-}
addQuery : Query -> Request a -> Request a
addQuery query req =
    AWS.Http.addQuery (formatQuery query) req


{-| Low-level request creator.

    stringRequest : String -> Method -> Path -> Body -> Request String
    stringRequest method url body =
        parserRequest
            name
            method
            url
            body
            (identity >> Ok)
            Task.succeed

-}
parserRequest : String -> Method -> Path -> Body -> (String -> Result String a) -> Request a
parserRequest name method path body parser =
    request name
        method
        path
        body
        (AWS.Http.stringBodyDecoder parser)
        AWS.Http.awsAppErrDecoder

bytesParserRequest : String -> Method -> Path -> Body -> (Bytes -> Result Bytes a) -> BytesRequest a
bytesParserRequest name method path body parser =
    requestBytes name
        method
        path
        body
        (AWS.Http.bytesBodyDecoder parser)
        AWS.Http.awsAppErrDecoderBytes

{-| Create a `Request` that returns its response body as a string.

    getObject : Bucket -> Key -> Request String
    getObject bucket key =
        stringRequest "operation" GET (objectPath bucket key) emptyBody

-}
stringRequest : String -> Method -> Path -> Body -> Request String
stringRequest name method path body =
    parserRequest name method path body (identity >> Ok)


bytesRequest : String -> Method -> Path -> Body -> BytesRequest Bytes
bytesRequest name method path body =
    bytesParserRequest name method path body (identity >> Ok)

{-| Create a `Request` to list the keys for an S3 bucket.
-}
listKeys : Bucket -> Request KeyList
listKeys bucket =
    parserRequest
        "listKeys"
        GET
        ("/" ++ bucket ++ "/")
        emptyBody
        parseListBucketResponse


{-| Turn a bucket and a key into an object path.

    "/" ++ bucket ++ "/" ++ key

-}
objectPath : Bucket -> Key -> String
objectPath bucket key =
    "/" ++ bucket ++ "/" ++ key


pathBucketAndKey : String -> ( Bucket, Key )
pathBucketAndKey path =
    let
        slashes =
            String.indexes "/" path
    in
    case List.head slashes of
        Nothing ->
            ( "", path )

        Just pos ->
            if pos /= 0 then
                ( "", path )

            else
                case List.head <| List.drop 1 slashes of
                    Nothing ->
                        ( String.dropLeft 1 path, "" )

                    Just pos2 ->
                        ( String.slice 1 pos2 path
                        , String.dropLeft (pos2 + 1) path
                        )


{-| Read an S3 object.

The contents will be the successful result of the `Task` created by `S3.send`.

-}
getObject : Bucket -> Key -> Request String
getObject bucket key =
    stringRequest "getObject" GET (objectPath bucket key) emptyBody

getBytesObject : Bucket -> Key -> BytesRequest Bytes
getBytesObject bucket key =
    bytesRequest "getObject" GET (objectPath bucket key) emptyBody

{-| Return the URL string for a request.
-}
requestUrl : Account -> Request a -> String
requestUrl account req =
    let
        ( service, request ) =
            fudgeRequest account req

        { hostResolver, endpoint, endpointPrefix } =
            service

        host =
            hostResolver endpoint endpointPrefix
    in
    "https://"
        ++ host
        ++ request.path


{-| Read an object and process the entire Http Response.
-}
getFullObject : Bucket -> Key -> (Metadata -> String -> Result String a) -> Request a
getFullObject bucket key parser =
    request "getFullObject"
        GET
        (objectPath bucket key)
        emptyBody
        parser
        AWS.Http.awsAppErrDecoder


responseHeaders : Metadata -> String -> Result String ( String, Dict String String )
responseHeaders metadata body =
    Ok <| ( body, metadata.headers )


{-| Read an object with its HTTP response headers.
-}
getObjectWithHeaders : Bucket -> Key -> Request ( String, Dict String String )
getObjectWithHeaders bucket key =
    getFullObject bucket
        key
        responseHeaders


responseHeadersOnly : Metadata -> String -> Result String (Dict String String)
responseHeadersOnly metadata body =
    case responseHeaders metadata body of
        Ok ( _, headers ) ->
            Ok headers

        Err e ->
            Err e


{-| Do a HEAD request to get only an object's headers.
-}
getHeaders : Bucket -> Key -> Request (Dict String String)
getHeaders bucket key =
    getFullObject bucket
        key
        responseHeadersOnly


{-| Create an HTML body for `putObject` and friends.
-}
htmlBody : String -> Body
htmlBody =
    AWS.Http.stringBody "text/html;charset=utf-8"


{-| Create a JSON body for `putObject` and friends.
-}
jsonBody : JE.Value -> Body
jsonBody =
    AWS.Http.jsonBody


{-| Create a body with any mimetype for `putObject` and friends.

    stringBody "text/html" "Hello, World!"

-}
stringBody : Mimetype -> String -> Body
stringBody =
    AWS.Http.stringBody


{-| Write an object to S3, with default permissions (private).

The string resulting from a successful `send` isn't interesting.

-}
putObject : Bucket -> Key -> Body -> Request String
putObject bucket key body =
    stringRequest "putObject"
        PUT
        (objectPath bucket key)
        body


{-| Write an object to S3, with public-read permission.

The string resulting from a successful `send` isn't interesting.

-}
putPublicObject : Bucket -> Key -> Body -> Request String
putPublicObject bucket key body =
    putObject bucket key body
        |> addHeaders [ XAmzAcl AclPublicRead ]


{-| Write an Html string to S3, with public-read permission.

The string resulting from a successful `send` isn't interesting.

-}
putHtmlObject : Bucket -> Key -> String -> Request String
putHtmlObject bucket key html =
    putPublicObject bucket key <| htmlBody html


{-| Delete an S3 object.

The string resulting from a successful `send` isn't interesting.

-}
deleteObject : Bucket -> Key -> Request String
deleteObject bucket key =
    stringRequest "deleteObject"
        DELETE
        (objectPath bucket key)
        emptyBody
