module Crypto.HMAC exposing
    ( digest, Key, Message
    , digestBytes, KeyBytes, MessageBytes
    , Hash, sha224, sha256, sha384, sha512, sha512_224, sha512_256
    )

{-| Compute HMAC SHA message digests.

@docs digest, Key, Message


## Input and Output as Bytes

@docs digestBytes, KeyBytes, MessageBytes


## Hash Algorithms

@docs Hash, sha224, sha256, sha384, sha512, sha512_224, sha512_256

-}

import Array exposing (Array)
import Crypto.HMAC.Digest
import Crypto.SHA
import Crypto.SHA.Alg exposing (Alg(..))
import Word exposing (Word)
import Word.Bytes as Bytes
import Word.Hex as Hex



-- EXPOSED API


{-| Secret key
-}
type alias Key =
    String


{-| Secret key, as a list of bytes.

You must ensure each `Int` is an 8-bit value.

-}
type alias KeyBytes =
    List Int


{-| Message to be hashed
-}
type alias Message =
    String


{-| Message to be hashed, as a list of bytes.

You must ensure each `Int` is an 8-bit value.

-}
type alias MessageBytes =
    List Int


{-| Type of hash algorithm.
-}
type Hash
    = SHA Crypto.SHA.Alg.Alg


{-| Use SHA224 as the hash algorithm.
-}
sha224 : Hash
sha224 =
    SHA Crypto.SHA.Alg.SHA224


{-| Use SHA256 as the hash algorithm.
-}
sha256 : Hash
sha256 =
    SHA Crypto.SHA.Alg.SHA256


{-| Use SHA384 as the hash algorithm.
-}
sha384 : Hash
sha384 =
    SHA Crypto.SHA.Alg.SHA384


{-| Use SHA512 as the hash algorithm.
-}
sha512 : Hash
sha512 =
    SHA Crypto.SHA.Alg.SHA512


{-| Use SHA512/224 as the hash algorithm.
-}
sha512_224 : Hash
sha512_224 =
    SHA Crypto.SHA.Alg.SHA512_224


{-| Use SHA512/256 as the hash algorithm.
-}
sha512_256 : Hash
sha512_256 =
    SHA Crypto.SHA.Alg.SHA512_256


{-| HMAC digest using UTF-8 strings as input.

Outputs bytes encoded as a hexadecimal string. Prefer this function when your
key and message are UTF-8 encoded strings.

    Crypto.HMAC.digest sha256 "key" "The quick brown fox jumps over the lazy dog"
    --> "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"

    Crypto.HMAC.digest sha512 "key" "I ❤ cheese"
    --> "a885c96140f95cb0b326306edfba49afbb5d38d3a7ed6ccfd67153429cbd3c56d0c514fcaa53b710bb7ba6cc0dfedfdb4d53795acbeb48eb23aa93e5ce9760dd"

-}
digest : Hash -> Key -> Message -> String
digest type_ key message =
    Crypto.HMAC.Digest.digestBytes
        (hash type_)
        (blockSize type_)
        (Bytes.fromUTF8 key)
        (Bytes.fromUTF8 message)
        |> Hex.fromWordArray


{-| HMAC digest using raw bytes as input and output.

Prefer `digest` when your key and message are UTF-8 strings. This function
(`digestBytes`) is unsafe, in that it does not ensure each `Int` fits into an
8-bit value.

Prefer `digestBytes` when you need to chain digests. That is, to use the output
of a digest as the input (either key or message) to another digest.

See the [AWS Signature V4 Example](http://docs.aws.amazon.com/general/latest/gr/signature-v4-test-suite.html#signature-v4-test-suite-example)
for an explanation of the following algorithm:

    import Word.Bytes as Bytes
    import Word.Hex as Hex

    let
        digest =
            \message key ->
                Crypto.HMAC.digestBytes sha256
                    key
                    (Bytes.fromUTF8 message)
    in
    ("AWS4" ++ "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY")
        |> Bytes.fromUTF8
        |> digest "20150830"
        |> digest "us-east-1"
        |> digest "service"
        |> digest "aws4_request"
        |> digest "AWS4-HMAC-SHA256\n20150830T123600Z\n20150830/us-east-1/service/aws4_request\n816cd5b414d056048ba4f7c5386d6e0533120fb1fcfa93762cf0fc39e2cf19e0"
        |> Hex.fromByteList
    --> "b97d918cfa904a5beff61c982a1b6f458b799221646efd99d3219ec94cdf2500"

-}
digestBytes : Hash -> KeyBytes -> MessageBytes -> List Int
digestBytes type_ key message =
    Crypto.HMAC.Digest.digestBytes
        (hash type_)
        (blockSize type_)
        key
        message
        |> Word.toBytes


blockSize : Hash -> Int
blockSize (SHA alg) =
    case alg of
        SHA224 ->
            blockSize (SHA SHA256)

        SHA256 ->
            64

        SHA384 ->
            blockSize (SHA SHA512)

        SHA512 ->
            128

        SHA512_224 ->
            blockSize (SHA SHA512)

        SHA512_256 ->
            blockSize (SHA SHA512)


hash : Hash -> List Int -> Array Word
hash (SHA alg) =
    Crypto.SHA.digest alg
