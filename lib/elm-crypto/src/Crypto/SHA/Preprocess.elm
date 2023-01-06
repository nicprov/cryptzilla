module Crypto.SHA.Preprocess exposing (calculateK, preprocess)

{-| SHA-2 preprocess.
-}

import Crypto.SHA.Alg exposing (Alg(..))
import Crypto.SHA.Chunk as Chunk
import Word.Bytes as Bytes


{-| Append 1 + K zeros + size of message.

    import Word.Bytes exposing (fromUTF8)
    import Crypto.SHA.Alg exposing (Alg(..))

    preprocess SHA256 []
    --> 0x80 :: (List.repeat 63 0x00)

    preprocess SHA512 []
    --> 0x80 :: (List.repeat 127 0x00)

    let
        x = preprocess SHA256 (fromUTF8 "I ❤ cheese")
    in
        ( x |> List.length
        , x |> List.reverse |> List.head
        )
    --> ( 64
    --> , Just <| (9 + 3) * 8
    --> )

    let
        y = preprocess SHA512 (fromUTF8 "I ❤ cheese")
    in
        ( y |> List.length
        , y |> List.reverse |> List.head
        )
    --> ( 128
    --> , Just <| (9 + 3) * 8
    --> )

-}
preprocess : Alg -> List Int -> List Int
preprocess alg message =
    List.append message <| postfix alg (8 * List.length message)


postfix : Alg -> Int -> List Int
postfix alg messageSize =
    List.concat
        [ [ 0x80 ]
        , List.repeat ((calculateK alg messageSize - 7) // 8) 0x00
        , Bytes.fromInt (messageSizeBytes alg) messageSize
        ]


{-| Calculate the amount of 0 bit padding.

    import Crypto.SHA.Alg exposing (Alg(..))

    calculateK SHA256 0
    --> (512 - 64 - 1)

    calculateK SHA256 (512 - 64 - 1)
    --> 0

    calculateK SHA512 (1024 - 128 - 1)
    --> 0

    calculateK SHA256 (512 - 64 - 1 + 8)
    --> (512 - 8)

    calculateK SHA256 (512 - 64 - 1 - 8)
    --> 8

    calculateK SHA384 (1024 - 128 - 1 + 16)
    --> (1024 - 16)

    calculateK SHA384 (1024 - 128 - 1 - 16)
    --> 16

-}
calculateK : Alg -> Int -> Int
calculateK alg l =
    let
        c =
            Chunk.sizeInBits alg
    in
        modBy c
            (c
                - 1
                - (8 * messageSizeBytes alg)
                - (modBy c l)
            )


messageSizeBytes : Alg -> Int
messageSizeBytes alg =
    case alg of
        SHA224 ->
            messageSizeBytes SHA256

        SHA256 ->
            8

        SHA384 ->
            messageSizeBytes SHA512

        SHA512 ->
            16

        SHA512_224 ->
            messageSizeBytes SHA512

        SHA512_256 ->
            messageSizeBytes SHA512
