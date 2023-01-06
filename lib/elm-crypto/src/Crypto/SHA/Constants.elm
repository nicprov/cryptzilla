module Crypto.SHA.Constants exposing (initialHashValues, roundConstants)

import Crypto.SHA.Alg exposing (Alg(..))
import Crypto.SHA.Types exposing (RoundConstants, WorkingVars)
import Word exposing (Word(..))


initialHashValues : Alg -> WorkingVars
initialHashValues alg =
    case alg of
        SHA224 ->
            WorkingVars
                (W 0xC1059ED8)
                (W 0x367CD507)
                (W 0x3070DD17)
                (W 0xF70E5939)
                (W 0xFFC00B31)
                (W 0x68581511)
                (W 0x64F98FA7)
                (W 0xBEFA4FA4)

        SHA256 ->
            WorkingVars
                (W 0x6A09E667)
                (W 0xBB67AE85)
                (W 0x3C6EF372)
                (W 0xA54FF53A)
                (W 0x510E527F)
                (W 0x9B05688C)
                (W 0x1F83D9AB)
                (W 0x5BE0CD19)

        SHA384 ->
            WorkingVars
                (D 0xCBBB9D5D 0xC1059ED8)
                (D 0x629A292A 0x367CD507)
                (D 0x9159015A 0x3070DD17)
                (D 0x152FECD8 0xF70E5939)
                (D 0x67332667 0xFFC00B31)
                (D 0x8EB44A87 0x68581511)
                (D 0xDB0C2E0D 0x64F98FA7)
                (D 0x47B5481D 0xBEFA4FA4)

        SHA512 ->
            WorkingVars
                (D 0x6A09E667 0xF3BCC908)
                (D 0xBB67AE85 0x84CAA73B)
                (D 0x3C6EF372 0xFE94F82B)
                (D 0xA54FF53A 0x5F1D36F1)
                (D 0x510E527F 0xADE682D1)
                (D 0x9B05688C 0x2B3E6C1F)
                (D 0x1F83D9AB 0xFB41BD6B)
                (D 0x5BE0CD19 0x137E2179)

        SHA512_224 ->
            WorkingVars
                -- http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
                -- Section 5.3.6.1
                (D 0x8C3D37C8 0x19544DA2)
                (D 0x73E19966 0x89DCD4D6)
                (D 0x1DFAB7AE 0x32FF9C82)
                (D 0x679DD514 0x582F9FCF)
                (D 0x0F6D2B69 0x7BD44DA8)
                (D 0x77E36F73 0x04C48942)
                (D 0x3F9D85A8 0x6A1D36C8)
                (D 0x1112E6AD 0x91D692A1)

        SHA512_256 ->
            WorkingVars
                -- http://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf
                -- Section 5.3.6.2
                (D 0x22312194 0xFC2BF72C)
                (D 0x9F555FA3 0xC84C64C2)
                (D 0x2393B86B 0x6F53B151)
                (D 0x96387719 0x5940EABD)
                (D 0x96283EE2 0xA88EFFE3)
                (D 0xBE5E1E25 0x53863992)
                (D 0x2B0199FC 0x2C85B8AA)
                (D 0x0EB72DDC 0x81C52CA2)


roundConstants : Alg -> RoundConstants
roundConstants alg =
    case alg of
        SHA224 ->
            roundConstants SHA256

        SHA256 ->
            [ W 0x428A2F98
            , W 0x71374491
            , W 0xB5C0FBCF
            , W 0xE9B5DBA5
            , W 0x3956C25B
            , W 0x59F111F1
            , W 0x923F82A4
            , W 0xAB1C5ED5
            , W 0xD807AA98
            , W 0x12835B01
            , W 0x243185BE
            , W 0x550C7DC3
            , W 0x72BE5D74
            , W 0x80DEB1FE
            , W 0x9BDC06A7
            , W 0xC19BF174
            , W 0xE49B69C1
            , W 0xEFBE4786
            , W 0x0FC19DC6
            , W 0x240CA1CC
            , W 0x2DE92C6F
            , W 0x4A7484AA
            , W 0x5CB0A9DC
            , W 0x76F988DA
            , W 0x983E5152
            , W 0xA831C66D
            , W 0xB00327C8
            , W 0xBF597FC7
            , W 0xC6E00BF3
            , W 0xD5A79147
            , W 0x06CA6351
            , W 0x14292967
            , W 0x27B70A85
            , W 0x2E1B2138
            , W 0x4D2C6DFC
            , W 0x53380D13
            , W 0x650A7354
            , W 0x766A0ABB
            , W 0x81C2C92E
            , W 0x92722C85
            , W 0xA2BFE8A1
            , W 0xA81A664B
            , W 0xC24B8B70
            , W 0xC76C51A3
            , W 0xD192E819
            , W 0xD6990624
            , W 0xF40E3585
            , W 0x106AA070
            , W 0x19A4C116
            , W 0x1E376C08
            , W 0x2748774C
            , W 0x34B0BCB5
            , W 0x391C0CB3
            , W 0x4ED8AA4A
            , W 0x5B9CCA4F
            , W 0x682E6FF3
            , W 0x748F82EE
            , W 0x78A5636F
            , W 0x84C87814
            , W 0x8CC70208
            , W 0x90BEFFFA
            , W 0xA4506CEB
            , W 0xBEF9A3F7
            , W 0xC67178F2
            ]

        SHA384 ->
            roundConstants SHA512

        SHA512 ->
            [ D 0x428A2F98 0xD728AE22
            , D 0x71374491 0x23EF65CD
            , D 0xB5C0FBCF 0xEC4D3B2F
            , D 0xE9B5DBA5 0x8189DBBC
            , D 0x3956C25B 0xF348B538
            , D 0x59F111F1 0xB605D019
            , D 0x923F82A4 0xAF194F9B
            , D 0xAB1C5ED5 0xDA6D8118
            , D 0xD807AA98 0xA3030242
            , D 0x12835B01 0x45706FBE
            , D 0x243185BE 0x4EE4B28C
            , D 0x550C7DC3 0xD5FFB4E2
            , D 0x72BE5D74 0xF27B896F
            , D 0x80DEB1FE 0x3B1696B1
            , D 0x9BDC06A7 0x25C71235
            , D 0xC19BF174 0xCF692694
            , D 0xE49B69C1 0x9EF14AD2
            , D 0xEFBE4786 0x384F25E3
            , D 0x0FC19DC6 0x8B8CD5B5
            , D 0x240CA1CC 0x77AC9C65
            , D 0x2DE92C6F 0x592B0275
            , D 0x4A7484AA 0x6EA6E483
            , D 0x5CB0A9DC 0xBD41FBD4
            , D 0x76F988DA 0x831153B5
            , D 0x983E5152 0xEE66DFAB
            , D 0xA831C66D 0x2DB43210
            , D 0xB00327C8 0x98FB213F
            , D 0xBF597FC7 0xBEEF0EE4
            , D 0xC6E00BF3 0x3DA88FC2
            , D 0xD5A79147 0x930AA725
            , D 0x06CA6351 0xE003826F
            , D 0x14292967 0x0A0E6E70
            , D 0x27B70A85 0x46D22FFC
            , D 0x2E1B2138 0x5C26C926
            , D 0x4D2C6DFC 0x5AC42AED
            , D 0x53380D13 0x9D95B3DF
            , D 0x650A7354 0x8BAF63DE
            , D 0x766A0ABB 0x3C77B2A8
            , D 0x81C2C92E 0x47EDAEE6
            , D 0x92722C85 0x1482353B
            , D 0xA2BFE8A1 0x4CF10364
            , D 0xA81A664B 0xBC423001
            , D 0xC24B8B70 0xD0F89791
            , D 0xC76C51A3 0x0654BE30
            , D 0xD192E819 0xD6EF5218
            , D 0xD6990624 0x5565A910
            , D 0xF40E3585 0x5771202A
            , D 0x106AA070 0x32BBD1B8
            , D 0x19A4C116 0xB8D2D0C8
            , D 0x1E376C08 0x5141AB53
            , D 0x2748774C 0xDF8EEB99
            , D 0x34B0BCB5 0xE19B48A8
            , D 0x391C0CB3 0xC5C95A63
            , D 0x4ED8AA4A 0xE3418ACB
            , D 0x5B9CCA4F 0x7763E373
            , D 0x682E6FF3 0xD6B2B8A3
            , D 0x748F82EE 0x5DEFB2FC
            , D 0x78A5636F 0x43172F60
            , D 0x84C87814 0xA1F0AB72
            , D 0x8CC70208 0x1A6439EC
            , D 0x90BEFFFA 0x23631E28
            , D 0xA4506CEB 0xDE82BDE9
            , D 0xBEF9A3F7 0xB2C67915
            , D 0xC67178F2 0xE372532B
            , D 0xCA273ECE 0xEA26619C
            , D 0xD186B8C7 0x21C0C207
            , D 0xEADA7DD6 0xCDE0EB1E
            , D 0xF57D4F7F 0xEE6ED178
            , D 0x06F067AA 0x72176FBA
            , D 0x0A637DC5 0xA2C898A6
            , D 0x113F9804 0xBEF90DAE
            , D 0x1B710B35 0x131C471B
            , D 0x28DB77F5 0x23047D84
            , D 0x32CAAB7B 0x40C72493
            , D 0x3C9EBE0A 0x15C9BEBC
            , D 0x431D67C4 0x9C100D4C
            , D 0x4CC5D4BE 0xCB3E42B6
            , D 0x597F299C 0xFC657E2A
            , D 0x5FCB6FAB 0x3AD6FAEC
            , D 0x6C44198C 0x4A475817
            ]

        SHA512_224 ->
            roundConstants SHA512

        SHA512_256 ->
            roundConstants SHA512
