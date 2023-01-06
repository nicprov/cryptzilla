module Crypto.SHA.Process exposing (chunks)

{-| Chunk processing.

    import Array
    import Crypto.SHA.Types exposing (Alg(..))

-}

import Array exposing (Array)
import Crypto.SHA.Alg exposing (Alg(..))
import Crypto.SHA.Chunk as Chunk exposing (Chunk)
import Crypto.SHA.Constants exposing (initialHashValues, roundConstants)
import Crypto.SHA.MessageSchedule as MessageSchedule exposing (MessageSchedule)
import Crypto.SHA.Types exposing (..)
import Word exposing (Word)


{-| Process chunks producing an array of hashed words.
-}
chunks : Alg -> Array Word -> Array Word
chunks alg words =
    chunks_ alg (Array.toList words) (initialHashValues alg)
        |> workingVarsToWords alg


chunks_ : Alg -> List Word -> WorkingVars -> WorkingVars
chunks_ alg words currentHash =
    case Chunk.next alg words of
        ( Nothing, _ ) ->
            currentHash

        ( Just chunk, rest ) ->
            let
                vars =
                    chunk
                        |> MessageSchedule.fromChunk alg
                        |> compressLoop alg currentHash
                        |> addWorkingVars currentHash
            in
            chunks_ alg rest vars


compressLoop : Alg -> WorkingVars -> MessageSchedule -> WorkingVars
compressLoop alg workingVars messageSchedule =
    List.foldl
        (compress alg)
        workingVars
        (List.map2 (\a b -> ( a, b )) (roundConstants alg) (Array.toList messageSchedule))


{-| for i from 0 to 63
S1 := (e rightrotate 6) xor (e rightrotate 11) xor (e rightrotate 25)
ch := (e and f) xor ((not e) and g)
temp1 := h + S1 + ch + k[i] + w[i]
S0 := (a rightrotate 2) xor (a rightrotate 13) xor (a rightrotate 22)
maj := (a and b) xor (a and c) xor (b and c)
temp2 := S0 + maj

        h := g
        g := f
        f := e
        e := d + temp1
        d := c
        c := b
        b := a
        a := temp1 + temp2

-}
compress : Alg -> ( Word, Word ) -> WorkingVars -> WorkingVars
compress alg ( k, w ) { a, b, c, d, e, f, g, h } =
    let
        s1 =
            -- S1 := (e rightrotate 6) xor (e rightrotate 11) xor (e rightrotate 25)
            sum1 alg e

        ch =
            -- ch := (e and f) xor ((not e) and g)
            Word.and e f
                |> Word.xor (Word.and g <| Word.complement e)

        temp1 =
            -- temp1 := h + S1 + ch + k[i] + w[i]
            h
                |> Word.add s1
                |> Word.add ch
                |> Word.add k
                |> Word.add w

        s0 =
            -- S0 := (a rightrotate 2) xor (a rightrotate 13) xor (a rightrotate 22)
            sum0 alg a

        maj =
            -- maj := (a and b) xor (a and c) xor (b and c)
            Word.and a b
                |> Word.xor (Word.and a c)
                |> Word.xor (Word.and b c)

        temp2 =
            -- temp2 := S0 + maj
            Word.add s0 maj
    in
    -- h := g
    -- g := f
    -- f := e
    -- e := d + temp1
    -- d := c
    -- c := b
    -- b := a
    -- a := temp1 + temp2
    WorkingVars
        (Word.add temp1 temp2)
        a
        b
        c
        (Word.add d temp1)
        e
        f
        g


sum1 : Alg -> Word -> Word
sum1 alg word =
    case alg of
        SHA224 ->
            sum1 SHA256 word

        SHA384 ->
            sum1 SHA512 word

        SHA256 ->
            Word.rotateRightBy 6 word
                |> Word.xor (Word.rotateRightBy 11 word)
                |> Word.xor (Word.rotateRightBy 25 word)

        SHA512 ->
            Word.rotateRightBy 14 word
                |> Word.xor (Word.rotateRightBy 18 word)
                |> Word.xor (Word.rotateRightBy 41 word)

        SHA512_224 ->
            sum1 SHA512 word

        SHA512_256 ->
            sum1 SHA512 word


sum0 : Alg -> Word -> Word
sum0 alg word =
    case alg of
        SHA224 ->
            sum0 SHA256 word

        SHA384 ->
            sum0 SHA512 word

        SHA256 ->
            Word.rotateRightBy 2 word
                |> Word.xor (Word.rotateRightBy 13 word)
                |> Word.xor (Word.rotateRightBy 22 word)

        SHA512 ->
            Word.rotateRightBy 28 word
                |> Word.xor (Word.rotateRightBy 34 word)
                |> Word.xor (Word.rotateRightBy 39 word)

        SHA512_224 ->
            sum0 SHA512 word

        SHA512_256 ->
            sum0 SHA512 word
