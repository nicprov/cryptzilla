module Common.Footer exposing (..)

import Html exposing (Html, a, footer, p, text)
import Html.Attributes as Attr exposing (href, target)

version: String
version =
    "1.2.1"

viewFooter: Html msg
viewFooter =
    footer
        [ Attr.class "py-3 my-4"
        ]
        [ p
            [ Attr.class "text-center text-muted"
            ]
            [ text "Cryptzilla is developed by "
            , a [ href "https://nicolasprovencher.com"
                , target "_blank"
                ]
                [ text "Nicolas Provencher" ]
            , text " under "
            , a [ href "https://github.com/nicprov/cryptzilla/blob/master/LICENSE"
                , target "_blank"
                ]
                [ text " GPL-3.0 License" ]
            , text " | Version: "
            , a [ href ("https://github.com/nicprov/cryptzilla/releases/tag/" ++ version)
                , target "_blank"
                ]
                [ text version ]
            ]
        ]
