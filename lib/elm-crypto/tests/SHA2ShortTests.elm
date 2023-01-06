module SHA2ShortTests exposing (all)

import Crypto.SHA.Alg exposing (Alg(..))
import SHA.SHA224ShortMsg
import SHA.SHA256ShortMsg
import SHA.SHA384ShortMsg
import SHA.SHA512ShortMsg
import SHA.SHA512_224ShortMsg
import SHA.SHA512_256ShortMsg
import Test exposing (Test, describe, test)
import TestHelpers exposing (Sha2Suite, runSha2Suite)


all : Test
all =
    describe "SHA-2 on short messages"
        (List.map runSha2Suite suites)


suites : List Sha2Suite
suites =
    [ ( "SHA224 Short", SHA224, SHA.SHA224ShortMsg.vectors )
    , ( "SHA256 Short", SHA256, SHA.SHA256ShortMsg.vectors )
    , ( "SHA384 Short", SHA384, SHA.SHA384ShortMsg.vectors )
    , ( "SHA512 Short", SHA512, SHA.SHA512ShortMsg.vectors )
    , ( "SHA512/224 Short", SHA512_224, SHA.SHA512_224ShortMsg.vectors )
    , ( "SHA512/256 Short", SHA512_256, SHA.SHA512_256ShortMsg.vectors )
    ]
