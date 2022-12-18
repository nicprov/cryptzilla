module AWS.Uri exposing (percentEncode)

{-| Helper functions for encoding URIs.

AWS encodes more characters than `encodeURIComponent` does, so a special
implementation is needed.


# Helpers

@docs percentEncode

-}

import Char
import Dict exposing (Dict)
import Regex
import Url
import Word.Hex as Hex


{-| We don't use Url.percentEncode because it misses some characters. It uses the
native `encodeURIComponent` under the hood:

    encodeURIComponent escapes all characters except the following:
    alphabetic, decimal digits, - _ . ! ~ * ' ( )

    - from https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent

For AWS only "Unreserved Characters" are allowed.
See <http://tools.ietf.org/html/rfc3986>
Section 2.3

So basically we need to also cover: ! \* ' ( )

-}
percentEncode : String -> String
percentEncode x =
    x
        |> Url.percentEncode
        |> Regex.replace
            (Regex.fromString "[!*'()]" |> Maybe.withDefault Regex.never)
            (\match ->
                match.match
                    |> String.toList
                    |> List.head
                    |> Maybe.map
                        (\char ->
                            char
                                |> Char.toCode
                                |> Hex.fromByte
                                |> String.toUpper
                                |> (++) "%"
                        )
                    |> Maybe.withDefault ""
            )
