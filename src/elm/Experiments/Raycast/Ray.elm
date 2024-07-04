module Experiments.Raycast.Ray exposing (Ray, cast, collisions, create, debugDraw, lookAt, move)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color
import Experiments.Raycast.Boundary exposing (Boundary)
import Experiments.Raycast.Vector2 as Vector


type alias Ray =
    { position : ( Float, Float )
    , direction : ( Float, Float )
    }


move : Float -> Float -> Ray -> Ray
move x y { direction } =
    { position = ( x, y )
    , direction = direction
    }


lookAt : ( Float, Float ) -> Ray -> Ray
lookAt ( x, y ) { position } =
    { position = position
    , direction = Vector.normalize ( x - Tuple.first position, y - Tuple.second position )
    }


debugDraw : Ray -> Canvas.Renderable
debugDraw { position, direction } =
    let
        finalPosition =
            Tuple.mapBoth
                (\x -> x + Tuple.first direction * 100)
                (\y -> y + Tuple.second direction * 100)
                position
    in
    Canvas.shapes [ Canvas.stroke Color.red ]
        [ Canvas.path position [ Canvas.lineTo finalPosition ]
        ]


create : Float -> Float -> ( Float, Float ) -> Ray
create x1 y1 direction =
    { position = ( x1, y1 )
    , direction = direction
    }


collisions : List Boundary -> Ray -> Bool
collisions boundaries ray =
    boundaries
        |> List.map (collide ray)
        |> List.all identity


collide : Ray -> Boundary -> Bool
collide { position, direction } boundary =
    let
        ( x1, y1 ) =
            boundary.start

        ( x2, y2 ) =
            boundary.end

        ( x3, y3 ) =
            position

        ( x4, y4 ) =
            ( x3 + Tuple.first direction, y3 + Tuple.second direction )

        -- cancel if denomiantor is 0
        denominator =
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

        t =
            ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denominator

        u =
            -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denominator
    in
    if denominator == 0 || t < 0 || t > 1 || u < 0 then
        False

    else
        True


cast : List Boundary -> Ray -> Maybe ( Float, Float )
cast boundaries ray =
    boundaries
        |> List.map (cast_ ray)
        |> List.filterMap identity
        |> List.sortBy (Vector.distance ray.position)
        |> List.head


cast_ : Ray -> Boundary -> Maybe ( Float, Float )
cast_ { position, direction } boundary =
    let
        ( x1, y1 ) =
            boundary.start

        ( x2, y2 ) =
            boundary.end

        ( x3, y3 ) =
            position

        ( x4, y4 ) =
            ( x3 + Tuple.first direction, y3 + Tuple.second direction )

        -- cancel if denomiantor is 0
        denominator =
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

        t =
            ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denominator

        u =
            -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denominator
    in
    if denominator == 0 || t < 0 || t > 1 || u < 0 then
        Nothing

    else
        Just ( x1 + t * (x2 - x1), y1 + t * (y2 - y1) )


cameraCast : ( Float, Float ) -> List Boundary -> Ray -> Maybe ( Float, Float )
cameraCast heading boundaries ray =
    boundaries
        |> List.map (cameraCast_ heading ray)
        |> List.filterMap identity
        |> List.sortBy (Vector.distance ray.position)
        |> List.head


cameraCast_ : ( Float, Float ) -> Ray -> Boundary -> Maybe ( Float, Float )
cameraCast_ heading { position, direction } boundary =
    let
        ( x1, y1 ) =
            boundary.start

        ( x2, y2 ) =
            boundary.end

        ( x3, y3 ) =
            position

        ( x4, y4 ) =
            ( x3 + Tuple.first direction, y3 + Tuple.second direction )

        -- cancel if denomiantor is 0
        denominator =
            (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

        t =
            ((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4)) / denominator

        u =
            -((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3)) / denominator
    in
    if denominator == 0 || t < 0 || t > 1 || u < 0 then
        Nothing

    else
        Just ( x1 + t * (x2 - x1), y1 + t * (y2 - y1) )
