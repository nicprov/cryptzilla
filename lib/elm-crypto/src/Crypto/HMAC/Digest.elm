module Crypto.HMAC.Digest exposing (digestBytes)

import Array exposing (Array)
import Bitwise
import Word exposing (Word)


type alias Hash =
    List Int -> Array Word


digestBytes : Hash -> Int -> List Int -> List Int -> Array Word
digestBytes hash blockSize key message =
    key
        |> normalizeKey hash blockSize
        |> hmac_ hash message


hmac_ : Hash -> List Int -> List Int -> Array Word
hmac_ hash message key =
    let
        oKeyPad =
            key
                |> List.map (Bitwise.xor 0x5C)

        iKeyPad =
            key
                |> List.map (Bitwise.xor 0x36)
    in
    List.append iKeyPad message
        |> hash
        |> Word.toBytes
        |> List.append oKeyPad
        |> hash


normalizeKey : Hash -> Int -> List Int -> List Int
normalizeKey hash blockSize key =
    case compare blockSize <| List.length key of
        EQ ->
            key

        GT ->
            key
                |> padEnd blockSize

        LT ->
            key
                |> hash
                |> Word.toBytes
                |> padEnd blockSize


padEnd : Int -> List Int -> List Int
padEnd blockSize bytes =
    List.append bytes <|
        List.repeat
            (blockSize - List.length bytes)
            0
