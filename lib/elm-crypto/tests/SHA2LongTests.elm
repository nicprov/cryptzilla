module SHA2LongTests exposing (all)

import Crypto.SHA.Alg exposing (Alg(..))
import SHA.SHA224LongMsg
import SHA.SHA256LongMsg
import SHA.SHA384LongMsg
import SHA.SHA512LongMsg
import SHA.SHA512_224LongMsg
import SHA.SHA512_256LongMsg
import Test exposing (Test, describe, test)
import TestHelpers exposing (Sha2Suite, runSha2Suite)


all : Test
all =
    describe "SHA-2 on long messages"
        (List.map runSha2Suite suites)


suites : List Sha2Suite
suites =
    [ ( "SHA224 Long", SHA224, SHA.SHA224LongMsg.vectors )
    , ( "SHA256 Long", SHA256, SHA.SHA256LongMsg.vectors )
    , ( "SHA384 Long", SHA384, SHA.SHA384LongMsg.vectors )
    , ( "SHA512 Long", SHA512, SHA.SHA512LongMsg.vectors )
    , ( "SHA512/224 Long", SHA512_224, SHA.SHA512_224LongMsg.vectors )
    , ( "SHA512/256 Long", SHA512_256, SHA.SHA512_256LongMsg.vectors )
    ]
