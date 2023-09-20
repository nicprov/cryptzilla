----------------------------------------------------------------------
--
-- S3/Types.elm
-- Types for elm-s3
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module S3.Types exposing
    ( Error(..), Account
    , Bucket, Key, Mimetype
    , StorageClass, Owner, KeyInfo, KeyList, CommonPrefixes
    , Query, QueryElement(..), CannedAcl(..)
    , aclToString
    )

{-| Types for S3 module


# Types

@docs Error, Account
@docs Bucket, Key, Mimetype
@docs StorageClass, Owner, KeyInfo, KeyList
@docs Query, QueryElement, CannedAcl


# Functions

@docs aclToString

-}

import AWS.Http
import AWS.Service as Service exposing (Service)
import Http
import Xml.Extra exposing (DecodeDetails)


{-| Errors returned from S3 operations

`HttpError` is from the standard Elm `Http` module.

`AWSError` is from the AWS.Http module.

`DecodeError` denotes a Decoder error in parsing S3 account info.

-}
type Error
    = HttpError Http.Error
    | AWSError AWS.Http.AWSAppError
    | DecodeError String


{-| Information about an S3 account
-}
type alias Account =
    { name : String
    , region : Maybe String
    , customHost: Maybe String
    , accessKey : String
    , secretKey : String
    , buckets : List String
    }


{-| The StorageClass for a key returned from listing a bucket's contents.
-}
type alias StorageClass =
    String


{-| The owner of an object returned from listing a bucket's contents.
-}
type alias Owner =
    { id : String
    , displayName : String
    }


{-| The name of an S3 bucket.
-}
type alias Bucket =
    String


{-| The name of an S3 key.
-}
type alias Key =
    String


{-| An HTTP mimetype, e.g. "text/html".
-}
type alias Mimetype =
    String


{-| Information about a single key returned from listing a bucket's contents.
-}
type alias KeyInfo =
    { key : Key
    , lastModified : String
    , eTag : String
    , size : Int
    , storageClass : StorageClass
    , owner : Maybe Owner
    }


{-| All the information returned from listing a bucket's contents.

An Elm encoding of the ListBucketResult XML element.

-}
type alias KeyList =
    { name : String
    , prefix : Maybe String
    , marker : Maybe String
    , nextMarker : Maybe String
    , maxKeys : Int
    , isTruncated : Bool
    , keys : List KeyInfo
    , prefixes: List CommonPrefixes
    }

type alias CommonPrefixes =
    { prefix: String
    }

{-| Values for the XAmzAcl Query type.
-}



-- http://docs.aws.amazon.com/AmazonS3/latest/dev/acl-overview.html#canned-acl


type CannedAcl
    = AclPrivate
    | AclPublicRead
    | AclPublicReadWrite
    | AclAwsExecRead
    | AclAuthenticatedRead
    | AclBucketOwnerRead
    | AclBucketOwnerFullControl
    | AclLogDeliveryWrite


{-| Convert a `CannedAcl` to a String.
-}
aclToString : CannedAcl -> String
aclToString acl =
    case acl of
        AclPrivate ->
            "private"

        AclPublicRead ->
            "public-read"

        AclPublicReadWrite ->
            "public-read-write"

        AclAwsExecRead ->
            "aws-exec-read"

        AclAuthenticatedRead ->
            "authenticated-read"

        AclBucketOwnerRead ->
            "bucket-owner-read"

        AclBucketOwnerFullControl ->
            "bucket-owner-full-control"

        AclLogDeliveryWrite ->
            "log-delivery-write"


{-| An element of a `Query`, used for HTTP headers and query parameters.

`AnyQuery` allows you to encode any key/value pair.

`XAmzAcl` is used as a header with `S3.putObject`.

The others are used as query parameters with `S3.listKeys`.

-}
type QueryElement
    = AnyQuery String String
    | XAmzAcl CannedAcl
    | Delimiter String
    | Marker String
    | MaxKeys Int
    | Prefix String


{-| A list of `QueryElement`s.
-}
type alias Query =
    List QueryElement
