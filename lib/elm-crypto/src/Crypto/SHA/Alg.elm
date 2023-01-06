module Crypto.SHA.Alg exposing (Alg(..), toString, wordSize)

import Word exposing (Size(..))


type Alg
    = SHA224
    | SHA256
    | SHA384
    | SHA512
    | SHA512_224
    | SHA512_256


toString : Alg -> String
toString alg =
    case alg of
        SHA224 ->
            "SHA224"

        SHA256 ->
            "SHA256"

        SHA384 ->
            "SHA384"

        SHA512 ->
            "SHA512"

        SHA512_224 ->
            "SHA512_224"

        SHA512_256 ->
            "SHA512_256"


wordSize : Alg -> Size
wordSize alg =
    case alg of
        SHA224 ->
            wordSize SHA256

        SHA256 ->
            Bit32

        SHA384 ->
            wordSize SHA512

        SHA512 ->
            Bit64

        SHA512_224 ->
            wordSize SHA512

        SHA512_256 ->
            wordSize SHA512
