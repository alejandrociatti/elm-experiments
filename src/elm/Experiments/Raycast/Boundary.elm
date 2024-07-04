module Experiments.Raycast.Boundary exposing (..)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color


type alias Boundary =
    { start : ( Float, Float )
    , end : ( Float, Float )
    }


draw : Boundary -> Canvas.Renderable
draw { start, end } =
    Canvas.shapes [ Canvas.stroke Color.white ]
        [ Canvas.path start [ Canvas.lineTo end ]
        ]


create : Float -> Float -> Float -> Float -> Boundary
create x1 y1 x2 y2 =
    { start = ( x1, y1 )
    , end = ( x2, y2 )
    }


moveBy : Float -> Float -> Boundary -> Boundary
moveBy x y { start, end } =
    { start = ( Tuple.first start + x, Tuple.second start + y )
    , end = ( Tuple.first end + x, Tuple.second end + y )
    }
