module AWS.Credentials exposing
    ( Credentials, AccessKeyId, SecretAccessKey
    , fromAccessKeys, withSessionToken
    )

{-| A set of AWS credentials consists of an acces key id, a secret access key
and an optional session token.

Note that credentials are sensitive and should not be bundled with web
applications; it is a simple matter to extract them if they are. If your Elm
code is running inside a Lambda function, this may not be a concern. If you Elm
code is running in a browser it is definitely a security concern. If you are
looking for a way to obtain credentials through user sign in against AWS Congito,
this authentication package will do that:

<https://package.elm-lang.org/packages/the-sett/elm-auth-aws/latest/>

@docs Credentials, AccessKeyId, SecretAccessKey
@docs fromAccessKeys, withSessionToken

-}


{-| Holds AWS credentials.
-}
type alias Credentials =
    { accessKeyId : String
    , secretAccessKey : String
    , sessionToken : Maybe String
    }


{-| The AWS access key ID.
-}
type alias AccessKeyId =
    String


{-| The AWS secret access key.
-}
type alias SecretAccessKey =
    String


{-| Create AWS credentials given an access key and secret key.
-}
fromAccessKeys : AccessKeyId -> SecretAccessKey -> Credentials
fromAccessKeys keyId secretKey =
    { accessKeyId = keyId
    , secretAccessKey = secretKey
    , sessionToken = Nothing
    }


{-| Sets the session token.
-}
withSessionToken : String -> Credentials -> Credentials
withSessionToken token creds =
    { creds | sessionToken = Just token }
