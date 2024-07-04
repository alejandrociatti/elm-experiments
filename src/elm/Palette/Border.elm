module Palette.Border exposing (..)

import Element exposing (Attribute)
import Element.Border as Border


edges :
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }
edges =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


roundRight : Int -> Attribute msg
roundRight radius =
    Border.roundEach { edges | topRight = radius, bottomRight = radius }


roundLeft : Int -> Attribute msg
roundLeft radius =
    Border.roundEach { edges | topLeft = radius, bottomLeft = radius }
