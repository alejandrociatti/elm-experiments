module Palette.Navbar exposing (..)

import Element exposing (..)
import Element.Background as Background
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
        [ el [] <| text "some logo"
        , row
            [ alignRight ]
            [ link []
                { url = "/graphicsvg"
                , label = text "link1"
                }
            ]
        ]
