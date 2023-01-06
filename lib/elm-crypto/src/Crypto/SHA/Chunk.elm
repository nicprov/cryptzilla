module Crypto.SHA.Chunk
    exposing
        ( Chunk
        , next
        , sizeInBits
        , sizeInBytes
        , sizeInWords
        )

import Crypto.SHA.Alg as Alg exposing (Alg(..))
import Word exposing (Word)


type alias Chunk =
    List Word


next : Alg -> List Word -> ( Maybe Chunk, List Word )
next alg words =
    let
        n =
            sizeInWords alg

        chunk =
            List.take n words
    in
        ( if List.isEmpty chunk then
            Nothing
          else
            Just chunk
        , List.drop n words
        )


{-|

    import Crypto.SHA.Alg exposing (Alg(..))

    sizeInBits SHA224
    --> 512

    sizeInBits SHA512
    --> 1024

-}
sizeInBits : Alg -> Int
sizeInBits =
    sizeInBytes >> (*) 8


sizeInBytes : Alg -> Int
sizeInBytes alg =
    case alg of
        SHA224 ->
            sizeInBytes SHA256

        SHA256 ->
            64

        SHA384 ->
            sizeInBytes SHA512

        SHA512 ->
            128

        SHA512_224 ->
            sizeInBytes SHA512

        SHA512_256 ->
            sizeInBytes SHA512


{-|

    import Crypto.SHA.Alg exposing (Alg(..))

    sizeInWords SHA256
    --> 16

    sizeInWords SHA384
    --> 16

-}
sizeInWords : Alg -> Int
sizeInWords alg =
    -- words / chunk
    (//)
        -- bytes / chunk
        (sizeInBytes alg)
        -- bytes / word
        (Word.sizeInBytes <| Alg.wordSize <| alg)
