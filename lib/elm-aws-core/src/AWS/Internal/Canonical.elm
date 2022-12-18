module AWS.Internal.Canonical exposing (canonical, canonicalHeaders, canonicalPayload, canonicalRaw, canonicalUri, canonicalUrlBuilder, encode2Tuple, joinHeader, mergeSameHeaders, normalizeHeader, resolveRelativePath, signedHeaders)

import AWS.Config exposing (Signer(..))
import AWS.Internal.Body exposing (Body)
import AWS.Internal.Service as Service exposing (Service)
import AWS.Uri
import Crypto.Hash exposing (sha256)
import Regex



-- http://docs.aws.amazon.com/general/latest/gr/sigv4-create-canonical-request.html


canonical : Signer -> String -> String -> List ( String, String ) -> List ( String, String ) -> Body -> String
canonical signer method path headers params body =
    canonicalRaw signer method path headers params body
        |> sha256


canonicalRaw : Signer -> String -> String -> List ( String, String ) -> List ( String, String ) -> Body -> String
canonicalRaw signer method path headers params body =
    [ String.toUpper method
    , canonicalUri signer path
    , canonicalUrlBuilder params
    , canonicalHeaders headers
    , ""
    , signedHeaders headers
    , canonicalPayload body
    ]
        |> String.join "\n"


canonicalUri : Signer -> String -> String
canonicalUri signer path =
    case signer of
        SignS3 ->
            -- Do not normalize URI paths for requests to Amazon S3.
            -- For example, if you have a bucket with an object named
            --
            --    my-object//example//photo.user
            --
            -- use that path. Normalizing the path to
            --    my-object/example/photo.user
            --
            -- will cause the request to fail. For more information, see
            -- Task 1: Create a Canonical Request in the Amazon Simple Storage
            -- Service API Reference:
            -- http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html#canonical-request
            path

        SignV4 ->
            if String.isEmpty path then
                "/"

            else
                path
                    |> Regex.replace (Regex.fromString "/{2,}" |> Maybe.withDefault Regex.never) (\_ -> "/")
                    |> resolveRelativePath
                    |> String.split "/"
                    |> List.map AWS.Uri.percentEncode
                    |> String.join "/"


canonicalUrlBuilder : List ( String, String ) -> String
canonicalUrlBuilder params =
    params
        |> List.sort
        |> List.map (encode2Tuple "=")
        |> String.join "&"


canonicalHeaders : List ( String, String ) -> String
canonicalHeaders headers =
    headers
        |> List.map normalizeHeader
        |> List.foldl mergeSameHeaders []
        |> List.map joinHeader
        |> List.sort
        |> String.join "\n"


signedHeaders : List ( String, String ) -> String
signedHeaders headers =
    headers
        |> List.foldl mergeSameHeaders []
        |> List.map (\( a, _ ) -> String.toLower a)
        |> List.sort
        |> String.join ";"


canonicalPayload : Body -> String
canonicalPayload =
    AWS.Internal.Body.toString >> sha256



-- HELPERS


resolveRelativePath : String -> String
resolveRelativePath path =
    let
        rel =
            Regex.fromString "(([^/]+)/[.]{2}|/[.])/?"
                |> Maybe.withDefault Regex.never
    in
    if Regex.contains rel path then
        path
            |> Regex.replace
                rel
                (\{ match } ->
                    if match == "/./" || match == "/." then
                        "/"

                    else
                        ""
                )
            |> resolveRelativePath

    else
        path


normalizeHeader : ( String, String ) -> ( String, String )
normalizeHeader ( key, val ) =
    ( String.toLower key
    , val
        |> Regex.replace (Regex.fromString "\\s*?\n\\s*" |> Maybe.withDefault Regex.never) (\_ -> ",")
        |> Regex.replace (Regex.fromString "(^\\s*|\\s*$)" |> Maybe.withDefault Regex.never) (\_ -> "")
        |> Regex.replace (Regex.fromString "\\s{2,}" |> Maybe.withDefault Regex.never) (\_ -> " ")
    )


mergeSameHeaders : ( String, String ) -> List ( String, String ) -> List ( String, String )
mergeSameHeaders ( key1, val1 ) acc =
    case acc of
        ( key0, val0 ) :: rest ->
            if key0 == key1 then
                ( key0, val0 ++ "," ++ val1 ) :: rest

            else
                ( key1, val1 ) :: ( key0, val0 ) :: rest

        _ ->
            ( key1, val1 ) :: acc


joinHeader : ( String, String ) -> String
joinHeader ( key, val ) =
    key ++ ":" ++ val


encode2Tuple : String -> ( String, String ) -> String
encode2Tuple separator ( a, b ) =
    [ AWS.Uri.percentEncode a, AWS.Uri.percentEncode b ] |> String.join separator
