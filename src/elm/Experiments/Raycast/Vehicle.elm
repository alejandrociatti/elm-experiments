module Experiments.Raycast.Vehicle exposing (..)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color
import Experiments.Raycast.Ball as Ball
import Experiments.Raycast.Boundary exposing (Boundary)
import Experiments.Raycast.Ray as Ray exposing (Ray)


type alias Vehicle =
    { position : ( Float, Float )
    , direction : ( Float, Float )
    , rays : List Ray
    , fov : Int
    }


create : Float -> Float -> Vehicle
create x y =
    { position = ( x, y )
    , direction = ( 1, 0 )
    , rays = createRays x y
    , fov = 60
    }


lookAt : ( Float, Float ) -> Vehicle -> Vehicle
lookAt ( x, y ) vehicle =
    { vehicle
        | direction = ( x - Tuple.first vehicle.position, y - Tuple.second vehicle.position )
    }


createRays : Float -> Float -> List Ray
createRays x y =
    List.range 0 359
        -- |> List.filter (\angle -> modBy 10 angle == 0)
        |> List.map (toFloat >> turns >> (\n -> n / 360))
        |> List.map (\angle -> Ray.create x y ( cos angle, sin angle ))


update : Float -> Float -> Vehicle -> Vehicle
update x y vehicle =
    { vehicle
        | position = ( x, y )
        , rays = List.map (Ray.move x y) vehicle.rays
    }


drawTop : List Boundary -> Vehicle -> Canvas.Renderable
drawTop walls { position, rays } =
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


drawFOV : List Boundary -> Vehicle -> Canvas.Renderable
drawFOV walls { position, rays } =
    let
        rays_ =
            rays
                |> List.map (Ray.cast walls)
                |> List.filterMap identity
                |> List.map (drawRay position)
    in
    Canvas.group [] rays_


drawRay : ( Float, Float ) -> ( Float, Float ) -> Canvas.Renderable
drawRay ( x1, y1 ) ( x2, y2 ) =
    Canvas.shapes [ Canvas.stroke <| Color.rgba 1 1 1 0.2 ]
        [ Canvas.path ( x1, y1 ) [ Canvas.lineTo ( x2, y2 ) ]
        ]
