module AWS.Config exposing
    ( ServiceConfig
    , ApiVersion, Protocol(..), Signer(..), TimestampFormat(..), Region, Endpoint(..)
    , defineGlobal, defineRegional
    , withJsonVersion, withSigningName, withTargetPrefix, withTimestampFormat, withXmlNamespace
    )

{-| A configuration for a service can be built directly, or by using the helper
functions for convenience.

To build an AWS service configuration directly:

    { endpointPrefix = "lambda"
    , apiVersion = "2015-03-31"
    , protocol = AWS.Config.REST_JSON
    , signer = AWS.Config.SignV4
    , endpoint = RegionalEndpoint "us-east-1"
    , jsonVersion = Nothing
    , signingName = Nothing
    , xmlNamespace = Nothing
    , timestampFormat = Nothing
    , targetPrefix = Nothing
    }

The same thing with the convenience functions would look like:

    AWS.Service.defineRegional
        "lambda"
        "2015-03-31"
        AWS.Config.REST_JSON
        AWS.Config.SignV4
        "us-east-1"

In some of the comments below you will see 'API metadata' mentioned. AWS
publishes a set of JSON files containing metadata describing each of its services,
and this is what is being referred to. You can find this metadata here:

<https://github.com/aws/aws-sdk-js/tree/master/apis>


# Service configuration.

@docs ServiceConfig
@docs ApiVersion, Protocol, Signer, TimestampFormat, Region, Endpoint


# Convenience functions to help with service configuration.

@docs defineGlobal, defineRegional
@docs withJsonVersion, withSigningName, withTargetPrefix, withTimestampFormat, withXmlNamespace

-}

import Enum exposing (Enum)


{-| Configures an AWS service.

Note that `timestampFormat` defaults to `UnixTimestamp` for JSON and REST\_JSON based
services and `ISO8601` for other services. Specifying `Nothing` will use the correct
default, unless it is set to something different in the API metadata.

Also note that `targetPrefix` will default to AWS ++ prefix ++ (apiVersion with the
'-' characters removed). This only needs to be set to a different value if it differs
in the API metadata.

-}
type alias ServiceConfig =
    { endpointPrefix : String
    , apiVersion : ApiVersion
    , protocol : Protocol
    , signer : Signer
    , endpoint : Endpoint
    , jsonVersion : Maybe String
    , signingName : Maybe String
    , xmlNamespace : Maybe String
    , timestampFormat : Maybe TimestampFormat
    , targetPrefix : Maybe String
    }


{-| Creates a global service definition.
-}
defineGlobal : String -> ApiVersion -> Protocol -> Signer -> ServiceConfig
defineGlobal prefix apiVersion proto signerType =
    { endpointPrefix = prefix
    , protocol = proto
    , signer = signerType
    , apiVersion = apiVersion
    , jsonVersion = Nothing
    , signingName = Nothing
    , targetPrefix = Nothing -- defaultTargetPrefix prefix apiVersion
    , timestampFormat = Nothing -- defaultTimestampFormat proto
    , xmlNamespace = Nothing
    , endpoint = GlobalEndpoint
    }


{-| Creates a regional service definition.
-}
defineRegional : String -> ApiVersion -> Protocol -> Signer -> Region -> ServiceConfig
defineRegional prefix apiVersion proto signerType rgn =
    let
        svc =
            defineGlobal prefix apiVersion proto signerType
    in
    { svc | endpoint = RegionalEndpoint rgn }


{-| Version of a service.
-}
type alias ApiVersion =
    String


{-| Defines the different protocols that AWS services can use.
-}
type Protocol
    = EC2
    | JSON
    | QUERY
    | REST_JSON
    | REST_XML


{-| Defines the different signing schemes that AWS services can use.
-}
type Signer
    = SignV4
    | SignS3


{-| Defines the different timestamp formats that AWS services can use.
-}
type TimestampFormat
    = ISO8601
    | RFC822
    | UnixTimestamp


{-| An AWS region string.

For example `"us-east-1"`.

-}
type alias Region =
    String


{-| Defines an AWS service endpoint.
-}
type Endpoint
    = GlobalEndpoint
    | RegionalEndpoint Region



--=== Optional properties that can be added to a Service.


{-| Set the JSON apiVersion.

Use this if `jsonVersion` is provided in the API metadata.

-}
withJsonVersion : String -> ServiceConfig -> ServiceConfig
withJsonVersion jsonVersion service =
    { service | jsonVersion = Just jsonVersion }


{-| Set the signing name for the service.

Use this if `signingName` is provided in the API metadata.

-}
withSigningName : String -> ServiceConfig -> ServiceConfig
withSigningName name service =
    { service | signingName = Just name }


{-| Set the target prefix for the service.

Use this if `targetPrefix` is provided in the API metadata.

Note that `targetPrefix` will default to AWS ++ prefix ++ (apiVersion with the
'-' characters removed). This only needs to be set to a different value if it differs
in the API metadata.

-}
withTargetPrefix : String -> ServiceConfig -> ServiceConfig
withTargetPrefix prefix service =
    { service | targetPrefix = Just prefix }


{-| Set the timestamp format for the service.

Use this if `timestampFormat` is provided in the API metadata.

Note that `timestampFormat` defaults to `UnixTimestamp` for JSON and REST\_JSON based
services and `ISO8601` for other services. Specifying `Nothing` will use the correct
default, unless it is set to something different in the API metadata.

-}
withTimestampFormat : TimestampFormat -> ServiceConfig -> ServiceConfig
withTimestampFormat format service =
    { service | timestampFormat = Just format }


{-| Set the XML namespace for the service.

Use this if `xmlNamespace` is provided in the API metadata.

-}
withXmlNamespace : String -> ServiceConfig -> ServiceConfig
withXmlNamespace namespace service =
    { service | xmlNamespace = Just namespace }



--=== Digital Ocean Services.
{- Use Digital Ocean Spaces as the backend service provider.

   Changes the way hostnames are resolved.

-}
-- toDigitalOceanService : ServiceConfig -> Service
-- toDigitalOceanService config =
--     { endpointPrefix = config.endpointPrefix
--     , apiVersion = config.apiVersion
--     , protocol = config.protocol
--     , signer = config.signer
--     , targetPrefix = config.targetPrefix
--     , timestampFormat = config.timestampFormat
--     , endpoint = config.endpoint
--     , jsonVersion = config.jsonVersion
--     , signingName = config.signingName
--     , xmlNamespace = config.xmlNamespace
--     , hostResolver =
--         \endpoint _ ->
--             case endpoint of
--                 GlobalEndpoint ->
--                     "nyc3.digitaloceanspaces.com"
--
--                 RegionalEndpoint rgn ->
--                     rgn ++ ".digitaloceanspaces.com"
--     , regionResolver =
--         \endpoint ->
--             case endpoint of
--                 GlobalEndpoint ->
--                     "nyc3"
--
--                 RegionalEndpoint rgn ->
--                     rgn
--     }
--=== Helpers
-- These not needed here. Put them in elm-aws-codegen.


timestampFormatEnum : Enum TimestampFormat
timestampFormatEnum =
    Enum.define
        [ ISO8601
        , RFC822
        , UnixTimestamp
        ]
        (\val ->
            case val of
                ISO8601 ->
                    "iso8601"

                RFC822 ->
                    "rfc822"

                UnixTimestamp ->
                    "unixTimestamp"
        )


protocolEnum : Enum Protocol
protocolEnum =
    Enum.define
        [ EC2
        , JSON
        , QUERY
        , REST_JSON
        , REST_XML
        ]
        (\val ->
            case val of
                EC2 ->
                    "ec2"

                JSON ->
                    "json"

                QUERY ->
                    "query"

                REST_JSON ->
                    "rest-json"

                REST_XML ->
                    "rest-xml"
        )


signerEnum : Enum Signer
signerEnum =
    Enum.define
        [ SignV4
        , SignS3
        ]
        (\val ->
            case val of
                SignV4 ->
                    "v4"

                SignS3 ->
                    "s3"
        )
