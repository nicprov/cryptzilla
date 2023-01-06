module Crypto.SHA exposing (digest)

import Array exposing (Array)
import Crypto.SHA.Alg as Alg exposing (Alg(..))
import Crypto.SHA.Preprocess
import Crypto.SHA.Process
import Word exposing (Word)


digest : Alg -> List Int -> Array Word
digest alg =
    Crypto.SHA.Preprocess.preprocess alg
        >> Word.fromBytes (Alg.wordSize alg)
        >> Crypto.SHA.Process.chunks alg
