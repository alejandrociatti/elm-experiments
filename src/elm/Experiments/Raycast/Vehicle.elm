module Experiments.Raycast.Vehicle exposing (..)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color
import Experiments.Raycast.Ball as Ball
import Experiments.Raycast.Boundary exposing (Boundary)
import Experiments.Raycast.Ray as Ray exposing (Ray)


type alias Vehicle =
    { position : ( Float, Float )
    , rays : List Ray
    }


create : Float -> Float -> Vehicle
create x y =
    { position = ( x, y )
    , rays = createRays x y
    }


createRays : Float -> Float -> List Ray
createRays x y =
    List.range 0 360
        |> List.filter (\angle -> modBy 10 angle == 0)
        |> List.map (toFloat >> turns >> (\n -> n / 360))
        |> List.map (\angle -> Ray.create x y ( cos angle, sin angle ))


update : Float -> Float -> Vehicle -> Vehicle
update x y vehicle =
    { vehicle
        | position = ( x, y )
        , rays = List.map (Ray.move x y) vehicle.rays
    }


draw : List Boundary -> Vehicle -> Canvas.Renderable
draw walls { position, rays } =
    let
        vehicle =
            Ball.draw (Ball.create (Tuple.first position) (Tuple.second position) 3)

        rays_ =
            rays
                |> List.map (Ray.cast walls)
                |> List.filterMap identity
                |> List.map (drawRay position)
    in
    Canvas.group [] <|
        vehicle
            :: rays_


drawRay : ( Float, Float ) -> ( Float, Float ) -> Canvas.Renderable
drawRay ( x1, y1 ) ( x2, y2 ) =
    Canvas.shapes [ Canvas.stroke Color.white ]
        [ Canvas.path ( x1, y1 ) [ Canvas.lineTo ( x2, y2 ) ]
        ]
