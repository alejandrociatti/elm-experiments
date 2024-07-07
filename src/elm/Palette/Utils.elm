module Palette.Utils exposing (..)

import Element exposing (Attribute, htmlAttribute)
import Html.Attributes as HA


attributeNone : Attribute msg
attributeNone =
    htmlAttribute <| HA.class ""
