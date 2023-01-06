module TestHelpers exposing (Sha2Suite, runSha2Suite)

import Crypto.SHA
import Crypto.SHA.Alg exposing (Alg)
import Expect
import Test exposing (Test, describe, test)
import Word.Hex as Hex


runSha2Suite : Sha2Suite -> Test
runSha2Suite ( name, hash, vectors ) =
    describe name
        (List.indexedMap (testVector name hash) vectors)


testVector : String -> Alg -> Int -> ( String, String ) -> Test
testVector name alg num ( md, msg ) =
    test (name ++ ": " ++ String.fromInt num) <|
        \_ ->
            Expect.equal
                md
                (Hex.toByteList msg
                    |> (Crypto.SHA.digest alg >> Hex.fromWordArray)
                )


type alias Sha2Suite =
    ( String, Alg, List ( String, String ) )
