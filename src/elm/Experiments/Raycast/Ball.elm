module Experiments.Raycast.Ball exposing (..)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color


type alias Ball =
    { position : ( Float, Float )
    , radius : Float
    }


create : Float -> Float -> Float -> Ball
create x y radius =
    { position = ( x, y )
    , radius = radius
    }


draw : Ball -> Canvas.Renderable
draw { position, radius } =
    Canvas.shapes [ Canvas.stroke Color.white, Canvas.fill Color.white ]
        [ Canvas.circle position radius
        ]
