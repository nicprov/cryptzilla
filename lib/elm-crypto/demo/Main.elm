module Main exposing (main)

import Crypto.HMAC exposing (Hash, sha224, sha256, sha384, sha512, sha512_224, sha512_256)
import Html exposing (Html, div, h1, input, span, text)
import Html.Attributes exposing (contenteditable, style, value)
import Html.Events exposing (onInput)


main : Program Never Model Msg
main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { text : String
    , secret : String
    }


init : Model
init =
    { text = "The quick brown fox jumps over the lazy dog"
    , secret = "key"
    }



-- UPDATE


type Msg
    = SetText String
    | SetSecret String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetText w ->
            ( { model | text = w }, Cmd.none )

        SetSecret w ->
            ( { model | secret = w }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [ style [ ( "margin", "50px" ) ] ] <|
        (++)
            [ h1 [] [ text "elm-crypto demo" ]
            , div []
                [ col1 "message"
                , col2a SetText model.text
                ]
            , div []
                [ col1 "key"
                , col2a SetSecret model.secret
                ]
            , div []
                [ col1 "HMAC SHA" ]
            ]
            (digests model)


digests : Model -> List (Html Msg)
digests model =
    [ { hash = sha224, label = "224" }
    , { hash = sha256, label = "256" }
    , { hash = sha384, label = "384" }
    , { hash = sha512, label = "512" }
    , { hash = sha512_224, label = "512/224" }
    , { hash = sha512_256, label = "512/256" }
    ]
        |> List.map
            (\{ hash, label } ->
                div []
                    [ col1 label
                    , col2b <| Crypto.HMAC.digest hash model.secret model.text
                    ]
            )


col1 : String -> Html Msg
col1 w =
    span
        [ style
            [ ( "display", "inline-block" )
            , ( "padding", "10px" )
            , ( "padding-left", "0" )
            , ( "width", "100px" )
            ]
        ]
        [ text w ]


col2a : (String -> Msg) -> String -> Html Msg
col2a msg w =
    input
        [ value w
        , inputStyle
        , onInput msg
        ]
        []


col2b : String -> Html Msg
col2b w =
    input
        [ value w
        , contenteditable False
        , inputStyle
        ]
        []


inputStyle : Html.Attribute Msg
inputStyle =
    style
        [ ( "padding", "4px" )
        , ( "width", "700px" )
        ]
