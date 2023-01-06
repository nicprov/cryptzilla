module Crypto.SHA.Types exposing
    ( RoundConstants
    , WorkingVars
    , addWorkingVars
    , workingVarsToWords
    )

import Array exposing (Array)
import Crypto.SHA.Alg exposing (Alg(..))
import Word exposing (Word(..))


type alias RoundConstants =
    List Word


type alias WorkingVars =
    { a : Word
    , b : Word
    , c : Word
    , d : Word
    , e : Word
    , f : Word
    , g : Word
    , h : Word
    }


addWorkingVars : WorkingVars -> WorkingVars -> WorkingVars
addWorkingVars x y =
    WorkingVars
        (Word.add x.a y.a)
        (Word.add x.b y.b)
        (Word.add x.c y.c)
        (Word.add x.d y.d)
        (Word.add x.e y.e)
        (Word.add x.f y.f)
        (Word.add x.g y.g)
        (Word.add x.h y.h)


workingVarsToWords : Alg -> WorkingVars -> Array Word
workingVarsToWords alg { a, b, c, d, e, f, g, h } =
    case alg of
        SHA224 ->
            Array.fromList [ a, b, c, d, e, f, g ]

        SHA256 ->
            Array.fromList [ a, b, c, d, e, f, g, h ]

        SHA384 ->
            Array.fromList [ a, b, c, d, e, f ]

        SHA512 ->
            Array.fromList [ a, b, c, d, e, f, g, h ]

        SHA512_224 ->
            [ a, b, c, d ]
                |> List.concatMap toSingleWord
                |> List.take 7
                |> Array.fromList

        SHA512_256 ->
            Array.fromList [ a, b, c, d ]


toSingleWord : Word -> List Word
toSingleWord word =
    case word of
        D xh xl ->
            [ W xh, W xl ]

        _ ->
            [ word ]
