# elm-crypto

[![live-demo](https://img.shields.io/badge/live-demo-blue.svg)](http://elm-crypto.betweenconcepts.com)
[![elm-package](https://img.shields.io/badge/elm-1.1.2-blue.svg)](http://package.elm-lang.org/packages/ktonon/elm-crypto/latest)
[![CircleCI](https://img.shields.io/circleci/project/github/ktonon/elm-crypto.svg)](https://circleci.com/gh/ktonon/elm-crypto)

Cryptography with HMAC and SHA.

Use SHA-2 hashing functions directly:

```elm
import Crypto.Hash

Crypto.Hash.sha512 ""
--> "cf83e1357eefb8bdf1542850d66d8007d620e4050b5715dc83f4a921d36ce9ce47d0d13c5d85f2b0ff8318d2877eec2f63b931bd47417a81a538327af927da3e"

Crypto.Hash.sha224 "The quick brown fox jumps over the lazy dog"
--> "730e109bd7a8a32b1cb9d9a09aa2325d2430587ddbc0c38bad911525"
```

Or with HMAC:

```elm
import Crypto.HMAC exposing (sha256, sha512)

Crypto.HMAC.digest sha256 "key" "The quick brown fox jumps over the lazy dog"
--> "f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8"

Crypto.HMAC.digest sha512 "key" "I ❤ cheese"
--> "a885c96140f95cb0b326306edfba49afbb5d38d3a7ed6ccfd67153429cbd3c56d0c514fcaa53b710bb7ba6cc0dfedfdb4d53795acbeb48eb23aa93e5ce9760dd"
```

## Validation

Not officially validated through [CAVP][], but unit tested against published test vectors:

* SHA-2 functions are unit tested against the [FIPS 180-4][] test vectors (short and long messages).
* HMAC with SHA-2 is unit tested against [RFC4231][].

[CAVP]:http://csrc.nist.gov/groups/STM/cavp/
[FIPS 180-4]:http://csrc.nist.gov/groups/STM/cavp/secure-hashing.html#shavs
[RFC4231]:https://tools.ietf.org/rfc/rfc4231.txt
[SHA-2]:https://en.wikipedia.org/wiki/SHA-2
