module Utils.Canvas exposing (..)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Canvas.Settings.Advanced as Canvas


transformAndRender : ( Int, Int ) -> ( Float, Float ) -> ( Float, Float ) -> Canvas.Renderable -> Canvas.Renderable
transformAndRender ( width, height ) ( x, y ) ( w, h ) renderable =
    let
        scaleX =
            w / toFloat width

        scaleY =
            h / toFloat height
    in
    Canvas.group
        [ Canvas.transform
            [ Canvas.translate x y
            , Canvas.scale scaleX scaleY
            ]
        ]
        [ renderable ]


transform : ( Int, Int ) -> ( Float, Float ) -> ( Float, Float ) -> List Canvas.Setting
transform ( width, height ) ( x, y ) ( w, h ) =
    let
        scaleX =
            w / toFloat width

        scaleY =
            h / toFloat height
    in
    [ Canvas.transform
        [ Canvas.translate x y
        , Canvas.scale scaleX scaleY
        ]
    ]


translateTo : ( Float, Float ) -> List Canvas.Setting
translateTo ( x, y ) =
    [ Canvas.transform [ Canvas.translate x y ] ]
