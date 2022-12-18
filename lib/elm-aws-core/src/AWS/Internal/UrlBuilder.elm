module AWS.Internal.UrlBuilder exposing (url, urlBytes)

import AWS.Internal.Request exposing (Request, BytesRequest)
import AWS.Internal.Service as Service exposing (Service)
import AWS.Uri
import Dict exposing (Dict)
import String
import Url


{-| Builds the URL for invoking a `Service` with a request.

This consists of combing together the host name, path and query string to form
the complete URL.

-}
url : Service -> Request err a -> String
url service { path, query } =
    "https://"
        ++ Service.host service
        ++ path
        ++ queryString query

urlBytes : Service -> BytesRequest err a -> String
urlBytes service { path, query } =
    "https://"
        ++ Service.host service
        ++ path
        ++ queryString query

queryString : List ( String, String ) -> String
queryString params =
    case params of
        [] ->
            ""

        _ ->
            params
                |> List.foldl
                    (\( key, val ) qs ->
                        qs |> add (AWS.Uri.percentEncode key) val
                    )
                    Dict.empty
                |> render


render : Dict String (List String) -> String
render qs =
    let
        flatten ( k, xs ) =
            List.map (\x -> k ++ "=" ++ Url.percentEncode x) xs
    in
    Dict.toList qs
        |> List.concatMap flatten
        |> String.join "&"
        |> (++) "?"


add : String -> String -> Dict String (List String) -> Dict String (List String)
add k v qs =
    let
        prepend maybeXs =
            case maybeXs of
                Nothing ->
                    Just [ v ]

                Just xs ->
                    Just (v :: xs)
    in
    Dict.update k prepend qs
