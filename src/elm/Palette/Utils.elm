module Palette.Utils exposing (..)

import Element exposing (Attribute, Element, html, htmlAttribute)
import Html as H
import Html.Attributes as HA


attributeNone : Attribute msg
attributeNone =
    htmlAttribute <| HA.class ""


ellipsis : String -> Element msg
ellipsis t =
    html <|
        H.span
            [ HA.style "display" "inline-block"
            , HA.style "overflow" "hidden"
            , HA.style "text-overflow" "ellipsis"
            , HA.style "line-height" "inherit"
            ]
            [ H.text t
            ]
