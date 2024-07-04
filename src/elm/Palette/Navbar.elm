module Palette.Navbar exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Palette.Color exposing (..)
import Palette.Spacing exposing (..)


navbar : Element msg
navbar =
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
            [ link [ Border.color white, Border.width 1, Border.rounded s1, padding s1 ]
                { url = "/wfc"
                , label = text "wfc 1.0"
                }
            , link [ Border.color white, Border.width 1, Border.rounded s1, padding s1 ]
                { url = "/wfc-graphicsvg"
                , label = text "wfc-alpha"
                }
            ]
        ]
