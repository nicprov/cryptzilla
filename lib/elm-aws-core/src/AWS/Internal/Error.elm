module AWS.Internal.Error exposing (Error(..))

{-| Internal representation of errors from HTTP calls. Either at the HTTP protocol
level, or at the AWS application level.
-}

import Http


{-| The HTTP calls made to AWS can produce errors in two ways. The first is the
normal `Http.Error` responses. The second is an error message at the application
level from one of the AWS service endpoints.

Only some endpoints can produce application level errors, in which case their error
type can be given as `Never`.

-}
type Error err
    = HttpError Http.Error
    | AWSError err
