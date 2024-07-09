module Palette.Navbar exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Palette.Color exposing (..)
import Palette.Spacing exposing (..)


navbar : List (Element msg) -> Element msg
navbar elements =
    row
        [ width fill
        , paddingXY s5 s3
        , spacing s5
        , Background.gradient { angle = degrees 90, steps = [ black, gray24 ] }
        , Font.color white
        ]
        [ el [] <|
            link []
                { url = "/"
                , label = text "some logo"
                }
        , row
            [ alignRight, spacing s1 ]
            elements
        ]
