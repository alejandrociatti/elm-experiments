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



{- #2b2b2b -}


gray35 : Color
gray35 =
    rgb255 35 35 39


gray43 : Color
gray43 =
    rgb255 43 43 43



{- #769aa5 -}


blue : Color
blue =
    rgb255 118 154 165



{- #8dd6ff -}


lightBlue : Color
lightBlue =
    rgb255 141 214 255


red : Color
red =
    rgb255 204 120 50



{- #a5c25c -}


green : Color
green =
    rgb255 165 194 92



{- #9876aa -}


purple : Color
purple =
    rgb255 152 118 170



{- #d5d49c -}


yellow : Color
yellow =
    rgb255 213 212 156
