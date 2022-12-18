module AWS.Service exposing (Service, service)

{-| Build an AWS service definition from its configuration.

Use the `AWS.Config` module to create the configuration for the AWS
service to use.

@docs Service, service

-}

import AWS.Config exposing (ApiVersion, Endpoint(..), Protocol(..), ServiceConfig, TimestampFormat(..))
import AWS.Internal.Service exposing (Service)


{-| An AWS Service definition. This is needed to make HTTP calls to the service.
-}
type alias Service =
    AWS.Internal.Service.Service


{-| Build an AWS service.
-}
service : ServiceConfig -> Service
service config =
    { endpointPrefix = config.endpointPrefix
    , apiVersion = config.apiVersion
    , protocol = config.protocol
    , signer = config.signer
    , endpoint = config.endpoint
    , jsonVersion = config.jsonVersion
    , signingName = config.signingName
    , xmlNamespace = config.xmlNamespace
    , targetPrefix =
        Maybe.withDefault (defaultTargetPrefix config.endpointPrefix config.apiVersion)
            config.targetPrefix
    , timestampFormat =
        Maybe.withDefault (defaultTimestampFormat config.protocol)
            config.timestampFormat
    , hostResolver = defaultHostResolver
    , regionResolver = defaultRegionResolver
    }


defaultHostResolver : Endpoint -> String -> String
defaultHostResolver endpoint prefix =
    case endpoint of
        GlobalEndpoint ->
            prefix ++ ".amazonaws.com"

        RegionalEndpoint rgn ->
            prefix ++ "." ++ rgn ++ ".amazonaws.com"


defaultRegionResolver : Endpoint -> String
defaultRegionResolver endpoint =
    case endpoint of
        RegionalEndpoint rgn ->
            rgn

        GlobalEndpoint ->
            -- See http://docs.aws.amazon.com/general/latest/gr/sigv4_changes.html
            "us-east-1"


defaultTargetPrefix : String -> ApiVersion -> String
defaultTargetPrefix prefix apiVersion =
    "AWS"
        ++ String.toUpper prefix
        ++ "_"
        ++ (apiVersion |> String.split "-" |> String.join "")


{-| See aws-sdk-js

`lib/model/shape.js`: function TimestampShape

-}
defaultTimestampFormat : Protocol -> TimestampFormat
defaultTimestampFormat proto =
    case proto of
        JSON ->
            UnixTimestamp

        REST_JSON ->
            UnixTimestamp

        _ ->
            ISO8601
