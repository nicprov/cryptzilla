----------------------------------------------------------------------
--
-- S3Example.elm
-- Example of using the S3 library
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------


module S3Example exposing (..)

import Browser
import Debug exposing (log)
import Dict exposing (Dict)
import Html
    exposing
        ( Attribute
        , Html
        , a
        , br
        , button
        , div
        , h2
        , input
        , option
        , p
        , select
        , span
        , table
        , td
        , text
        , textarea
        , th
        , tr
        )
import Html.Attributes
    exposing
        ( checked
        , cols
        , disabled
        , href
        , name
        , rows
        , selected
        , size
        , style
        , target
        , type_
        , value
        )
import Html.Events exposing (on, onClick, onInput, targetValue)
import Json.Decode as JD
import List.Extra as LE
import S3 exposing (readAccounts)
import S3.Types
    exposing
        ( Account
        , Bucket
        , Error(..)
        , Key
        , KeyInfo
        , KeyList
        , QueryElement(..)
        )
import Task


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { display : String
    , accounts : List Account
    , account : Account
    , bucket : Bucket
    , key : Key
    , keyList : Maybe KeyList
    , text : String
    , mimetype : String
    , headers : List ( String, String )
    }


type Msg
    = SetAccount String
    | ReceiveAccounts (Result Error (List Account))
    | ReceiveGetObject (Result Error ( String, Dict String String ))
    | ReceiveGetHeaders (Result Error (Dict String String))
    | SetBucket Bucket
    | ListBucket
    | ReceiveListBucket (Result Error KeyList)
    | SetKey Key
    | GetObject
    | GetKey Key
    | SetText String
    | SetMimetype String
    | PutObject
    | ReceivePutObject (Result Error String)
    | DeleteObject
    | ReceiveDeleteObject (Result Error String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { display = "Fetching accounts..."
      , accounts = []
      , account = defaultAccount
      , bucket = "No bucket"
      , key = ""
      , keyList = Nothing
      , text = ""
      , mimetype = "plain"
      , headers = []
      }
    , Task.attempt ReceiveAccounts (readAccounts Nothing)
    )


listBucket : Model -> Cmd Msg
listBucket model =
    S3.listKeys model.bucket
        |> S3.addQuery [ MaxKeys 100 ]
        |> S3.send model.account
        |> Task.attempt ReceiveListBucket


getObject : Model -> Cmd Msg
getObject model =
    S3.getObjectWithHeaders model.bucket model.key
        |> S3.send model.account
        |> Task.attempt ReceiveGetObject


getHeaders : Model -> Cmd Msg
getHeaders model =
    S3.getHeaders model.bucket model.key
        |> S3.send model.account
        |> Task.attempt ReceiveGetHeaders


putObject : Model -> Cmd Msg
putObject model =
    let
        body =
            S3.stringBody ("text/" ++ model.mimetype ++ "; charset=utf-8") model.text
    in
    S3.putPublicObject model.bucket model.key body
        |> S3.send model.account
        |> Task.attempt ReceivePutObject


deleteObject : Model -> Cmd Msg
deleteObject model =
    S3.deleteObject model.bucket model.key
        |> S3.send model.account
        |> Task.attempt ReceiveDeleteObject


defaultAccount : Account
defaultAccount =
    { name = "No account"
    , region = Nothing
    , accessKey = ""
    , secretKey = ""
    , buckets = [ "No bucket" ]
    , isDigitalOcean = False
    }


findAccount : Model -> String -> Account
findAccount model name =
    case LE.find (\a -> a.name == name) model.accounts of
        Nothing ->
            defaultAccount

        Just a ->
            a


stringEqual : String -> String -> Bool
stringEqual s1 s2 =
    String.toLower s1 == String.toLower s2


headersMimetype : List ( String, String ) -> String
headersMimetype headers =
    case
        LE.find
            (\header ->
                stringEqual "content-type" <| Tuple.first header
            )
            headers
    of
        Nothing ->
            "plain"

        Just ( _, mimetype ) ->
            if String.startsWith "text/html" <| String.toLower mimetype then
                "html"

            else
                "plain"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAccount name ->
            let
                account =
                    findAccount model name

                bucket =
                    case account.buckets of
                        b :: _ ->
                            b

                        _ ->
                            "No bucket"
            in
            ( { model
                | account = account
                , bucket = bucket
                , display = "Account: " ++ name
                , keyList = Nothing
              }
            , Cmd.none
            )

        SetBucket bucket ->
            ( { model | bucket = bucket }
            , Cmd.none
            )

        ListBucket ->
            ( { model | display = "Getting bucket listing..." }
            , listBucket model
            )

        ReceiveListBucket result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok keys ->
                    ( { model
                        | display = "Bucket listing received."
                        , keyList = Just keys
                      }
                    , Cmd.none
                    )

        SetKey key ->
            ( { model | key = key }
            , Cmd.none
            )

        GetObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )

            else
                ( { model | display = "Fetching " ++ model.key ++ "..." }
                , getObject model
                )

        ReceiveGetObject result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok ( res, headers ) ->
                    ( { model
                        | display = "Got " ++ model.key
                        , text = res
                        , mimetype = headersMimetype <| Dict.toList headers
                      }
                    , getHeaders model
                    )

        ReceiveGetHeaders result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok headers ->
                    ( { model | headers = Dict.toList headers }
                    , Cmd.none
                    )

        GetKey key ->
            ( { model | key = key }
            , Task.perform (\_ -> GetObject) <| Task.succeed ()
            )

        SetText text ->
            ( { model | text = text }
            , Cmd.none
            )

        SetMimetype mimetype ->
            ( { model | mimetype = mimetype }
            , Cmd.none
            )

        PutObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )

            else
                ( { model | display = "Writing " ++ model.key ++ "..." }
                , putObject model
                )

        ReceivePutObject result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok res ->
                    ( { model | display = "Put " ++ model.key }
                    , Cmd.none
                    )

        DeleteObject ->
            if model.key == "" then
                ( { model | display = "Blank key." }
                , Cmd.none
                )

            else
                ( { model | display = "Deleting " ++ model.key ++ "..." }
                , deleteObject model
                )

        ReceiveDeleteObject result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok res ->
                    ( { model | display = "Deleted " ++ model.key }
                    , Cmd.none
                    )

        ReceiveAccounts result ->
            case result of
                Err err ->
                    ( { model | display = Debug.toString err }
                    , Cmd.none
                    )

                Ok accounts ->
                    let
                        account =
                            case accounts of
                                a :: _ ->
                                    a

                                _ ->
                                    defaultAccount
                    in
                    ( { model
                        | accounts = accounts
                        , account = account
                        , bucket =
                            case account.buckets of
                                b :: _ ->
                                    b

                                _ ->
                                    "No bucket"
                        , display = "Accounts received."
                      }
                    , Cmd.none
                    )


view : Model -> Html Msg
view model =
    div
        [ style "margin-left" "3em"
        ]
        [ p [] [ text model.display ]
        , p []
            [ text "Account: "
            , accountSelector model
            ]
        , p []
            [ text "Bucket: "
            , bucketSelector model
            , text " "
            , button [ onClick ListBucket ]
                [ text "List Bucket" ]
            ]
        , p []
            [ text "Key: "
            , input
                [ type_ "text"
                , size 40
                , value model.key
                , onInput SetKey
                ]
                []
            , text " "
            , button [ onClick GetObject ]
                [ text "Get" ]
            , text " "
            , button [ onClick DeleteObject ]
                [ text "Delete" ]
            ]
        , p []
            [ text "URL: "
            , let
                request =
                    S3.getObject model.bucket model.key

                url =
                    S3.requestUrl model.account request
              in
              text url
            ]
        , p []
            [ input
                [ type_ "radio"
                , name "mimetype"
                , onClick (SetMimetype "plain")
                , checked <| model.mimetype == "plain"
                ]
                []
            , text " plain "
            , input
                [ type_ "radio"
                , name "mimetype"
                , onClick (SetMimetype "html")
                , checked <| model.mimetype == "html"
                ]
                []
            , text " html "
            , button [ onClick PutObject ]
                [ text "Put" ]
            ]
        , p []
            [ textarea
                [ cols 80
                , rows 20
                , value model.text
                , onInput SetText
                ]
                []
            ]
        , p []
            [ text "Headers: "
            , text <| Debug.toString model.headers
            ]
        , p []
            [ showKeys model ]
        ]


accountSelector : Model -> Html Msg
accountSelector model =
    select [ on "change" (JD.map SetAccount targetValue) ]
        (List.map (accountOption model) model.accounts)


accountOption : Model -> Account -> Html Msg
accountOption model account =
    option
        [ value account.name
        , selected (model.account.name == account.name)
        ]
        [ text account.name ]


bucketSelector : Model -> Html Msg
bucketSelector model =
    select [ on "change" (JD.map SetBucket targetValue) ]
        (List.map (bucketOption model) model.account.buckets)


bucketOption : Model -> String -> Html Msg
bucketOption model bucket =
    option
        [ value bucket
        , selected (model.bucket == bucket)
        ]
        [ text bucket ]


thText : String -> Html Msg
thText string =
    th [] [ text string ]


tdAlignHtml : String -> Html Msg -> Html Msg
tdAlignHtml alignment html =
    td
        [ style "padding-left" "1em"
        , style "padding-right" "1em"
        , style "text-align" alignment
        ]
        [ html ]


tdAlignText : String -> String -> Html Msg
tdAlignText alignment string =
    tdAlignHtml alignment <| text string


tdHtml : Html Msg -> Html Msg
tdHtml html =
    tdAlignHtml "left" html


tdText : String -> Html Msg
tdText string =
    tdAlignText "left" string


s3UrlPrefix : String
s3UrlPrefix =
    "https://s3.amazonaws.com/"


digitalOceanUrlPrefix : String
digitalOceanUrlPrefix =
    "https://nyc3.digitaloceanspaces.com/"


keyUrl : Model -> String -> String
keyUrl model key =
    let
        prefix =
            if model.account.isDigitalOcean then
                digitalOceanUrlPrefix

            else
                s3UrlPrefix
    in
    prefix ++ model.bucket ++ "/" ++ key


showKeys : Model -> Html Msg
showKeys model =
    case model.keyList of
        Nothing ->
            text ""

        Just keyList ->
            table [] <|
                List.concat
                    [ [ tr []
                            [ thText "Key"
                            , thText "Size"
                            , thText "Modified"
                            , thText "Owner"
                            ]
                      ]
                    , keyRows keyList model
                    ]


keyRows : KeyList -> Model -> List (Html Msg)
keyRows keyList model =
    List.map (keyRow model) keyList.keys


link : String -> (String -> Msg) -> Html Msg
link string msg =
    a
        [ href "#"
        , onClick <| msg string
        ]
        [ text string ]


keyRow : Model -> KeyInfo -> Html Msg
keyRow model info =
    tr []
        [ tdHtml <|
            span []
                [ link info.key GetKey
                , text " "
                , a
                    [ href <| keyUrl model info.key
                    , target "_blank"
                    ]
                    [ text "*" ]
                ]
        , tdAlignText "right" <| String.fromInt info.size
        , tdText info.lastModified
        , case info.owner of
            Nothing ->
                tdText "<none>"

            Just owner ->
                tdText owner.displayName
        ]
