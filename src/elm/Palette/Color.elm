module Palette.Color exposing (..)

import Element exposing (Color, rgb, rgb255)


transparent : Color
transparent =
    Element.rgba255 0 0 0 0


white : Color
white =
    rgb255 255 255 255


offwhite : Color
offwhite =
    rgb255 179 186 197


black : Color
black =
    rgb 0 0 0


gray24 : Color
gray24 =
    rgb255 24 24 24


gray : Color
gray =
    rgb255 35 35 39
