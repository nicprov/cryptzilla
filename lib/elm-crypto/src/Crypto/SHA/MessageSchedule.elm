module Crypto.SHA.MessageSchedule exposing (MessageSchedule, fromChunk)

import Array exposing (Array)
import Crypto.SHA.Alg exposing (Alg(..))
import Crypto.SHA.Chunk exposing (Chunk)
import Crypto.SHA.Constants exposing (roundConstants)
import Word exposing (Word)


type alias MessageSchedule =
    Array Word


{-| Copy chunk into first 16 words w[0..15] of the message schedule array.

Extend the first 16 words into the remaining 48 words w[16..63] of the message schedule array:
for i from 16 to 63

-}
fromChunk : Alg -> Chunk -> MessageSchedule
fromChunk alg chunk =
    let
        n =
            List.length (roundConstants alg)
    in
    List.foldl (nextPart alg) (Array.fromList chunk) (List.range 16 (n - 1))


{-| Get the next value in the message schedule.

s0 := (w[i-15] rightrotate 7) xor (w[i-15] rightrotate 18) xor (w[i-15] rightshift 3)
s1 := (w[i-2] rightrotate 17) xor (w[i-2] rightrotate 19) xor (w[i-2] rightshift 10)
w[i] := w[i-16] + s0 + w[i-7] + s1

-}
nextPart : Alg -> Int -> MessageSchedule -> MessageSchedule
nextPart alg i w =
    let
        i15 =
            at (i - 15) w

        i2 =
            at (i - 2) w

        s0 =
            sigma0 alg i15

        s1 =
            sigma1 alg i2
    in
    Array.append
        w
        (Array.fromList
            [ at (i - 16) w
                |> Word.add s0
                |> Word.add (at (i - 7) w)
                |> Word.add s1
            ]
        )


sigma0 : Alg -> Word -> Word
sigma0 alg word =
    case alg of
        SHA224 ->
            sigma0 SHA256 word

        SHA384 ->
            sigma0 SHA512 word

        SHA256 ->
            Word.rotateRightBy 7 word
                |> Word.xor (Word.rotateRightBy 18 word)
                |> Word.xor (Word.shiftRightZfBy 3 word)

        SHA512 ->
            Word.rotateRightBy 1 word
                |> Word.xor (Word.rotateRightBy 8 word)
                |> Word.xor (Word.shiftRightZfBy 7 word)

        SHA512_224 ->
            sigma0 SHA512 word

        SHA512_256 ->
            sigma0 SHA512 word


sigma1 : Alg -> Word -> Word
sigma1 alg word =
    case alg of
        SHA224 ->
            sigma1 SHA256 word

        SHA384 ->
            sigma1 SHA512 word

        SHA256 ->
            Word.rotateRightBy 17 word
                |> Word.xor (Word.rotateRightBy 19 word)
                |> Word.xor (Word.shiftRightZfBy 10 word)

        SHA512 ->
            Word.rotateRightBy 19 word
                |> Word.xor (Word.rotateRightBy 61 word)
                |> Word.xor (Word.shiftRightZfBy 6 word)

        SHA512_224 ->
            sigma1 SHA512 word

        SHA512_256 ->
            sigma1 SHA512 word


at : Int -> MessageSchedule -> Word
at i =
    Array.get i
        >> Maybe.withDefault Word.Mismatch
