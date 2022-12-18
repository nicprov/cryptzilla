----------------------------------------------------------------------
--
-- S3/Parser.elm
-- XML parser for elm-s3.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module S3.Parser exposing (parseListBucketResponse)

import Json.Decode as JD exposing (Decoder)
import S3.Types
    exposing
        ( Error(..)
        , KeyInfo
        , KeyList
        , Owner
        , StorageClass
        )
import Xml.Extra
    exposing
        ( Required(..)
        , TagSpec
        , decodeXml
        , multipleTag
        , optionalTag
        , requiredTag
        )


makeError : Xml.Extra.Error -> String
makeError error =
    case error of
        Xml.Extra.XmlError msg ->
            "Malformed Xml: " ++ msg

        Xml.Extra.DecodeError details ->
            "Parse Error: " ++ details.msg


parseListBucketResponse : String -> Result String KeyList
parseListBucketResponse xml =
    case decodeXml xml "ListBucketResult" listBucketDecoder listBucketTagSpecs of
        Err err ->
            case decodeXml xml "ListBucketResult" listBucketDecoder doListBucketTagSpecs of
                Err err2 ->
                    Err <| makeError err

                Ok res ->
                    Ok res

        Ok res ->
            Ok res


intOrString : Decoder String
intOrString =
    JD.oneOf
        [ JD.string
        , JD.int |> JD.andThen (\int -> JD.succeed (String.fromInt int))
        ]


ownerDecoder : Decoder Owner
ownerDecoder =
    JD.map2 Owner
        (JD.field "ID" intOrString)
        (JD.field "DisplayName" intOrString)


ownerTagSpecs : List TagSpec
ownerTagSpecs =
    [ ( "ID", Required )
    , ( "DisplayName", Required )
    ]


defaultOwner : Owner
defaultOwner =
    { id = "nothing"
    , displayName = "nobody"
    }


bucketDecoder : Decoder KeyInfo
bucketDecoder =
    JD.map6 KeyInfo
        (JD.field "Key" JD.string)
        (JD.field "LastModified" JD.string)
        (JD.field "ETag" JD.string)
        (JD.field "Size" JD.int)
        (JD.field "StorageClass" JD.string)
        (optionalTag "Owner" ownerDecoder ownerTagSpecs)



-- DigitalOcean puts Owner after StorageClass


bucketTagSpecs : List TagSpec
bucketTagSpecs =
    [ ( "Key", Required )
    , ( "LastModified", Required )
    , ( "ETag", Required )
    , ( "Size", Required )
    , ( "StorageClass", Required )
    , ( "Owner", Optional )
    ]



-- Amazon S3 puts Owner before StorageClass


amazonBucketTagSpecs : List TagSpec
amazonBucketTagSpecs =
    [ ( "Key", Required )
    , ( "LastModified", Required )
    , ( "ETag", Required )
    , ( "Size", Required )
    , ( "Owner", Required )
    , ( "StorageClass", Required )
    ]


boolOrString : Decoder Bool
boolOrString =
    JD.oneOf
        [ JD.bool
        , JD.string
            |> JD.andThen
                (\s -> JD.succeed <| s == "true")
        ]


listBucketDecoder : Decoder KeyList
listBucketDecoder =
    JD.map7 KeyList
        (JD.field "Name" JD.string)
        (optionalTag "Prefix" JD.string [])
        (optionalTag "Marker" JD.string [])
        (optionalTag "NextMarker" JD.string [])
        (JD.field "MaxKeys" JD.int)
        (requiredTag "IsTruncated" boolOrString [])
        (JD.oneOf
            [ multipleTag "Contents" bucketDecoder bucketTagSpecs
            , multipleTag "Contents" bucketDecoder amazonBucketTagSpecs
            ]
        )


listBucketTagSpecs : List TagSpec
listBucketTagSpecs =
    [ ( "Name", Required )
    , ( "Prefix", Optional )
    , ( "Marker", Optional )
    , ( "NextMarker", Optional )
    , ( "MaxKeys", Required )
    , ( "IsTruncated", Required )
    , ( "Contents", Multiple )
    ]


doListBucketTagSpecs : List TagSpec
doListBucketTagSpecs =
    [ ( "Name", Required )
    , ( "Prefix", Optional )
    , ( "NextMarker", Optional )
    , ( "MaxKeys", Required )
    , ( "IsTruncated", Required )
    , ( "Contents", Multiple )
    , ( "Marker", Optional )
    ]
