module Experiments.Raycast.Camera exposing (Vehicle, changeFov, create, drawFOV, drawTop, move, rotate)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color
import Experiments.Raycast.Ball as Ball
import Experiments.Raycast.Boundary exposing (Boundary)
import Experiments.Raycast.Ray as Ray exposing (Ray)
import Experiments.Raycast.Vector2 as Vector
import Json.Decode exposing (maybe)
import Utils.Canvas exposing (transform, translateTo)


type alias Vehicle =
    { position : ( Float, Float )
    , direction : Int
    , rays : List Ray
    , fov : Int
    }


create : Float -> Float -> Vehicle
create x y =
    let
        fov =
            60
    in
    { position = ( x, y )
    , direction = 0
    , rays = createRays fov 0 x y
    , fov = fov
    }


changeFov : Int -> Vehicle -> Vehicle
changeFov fov vehicle =
    update
        vehicle.direction
        fov
        (Tuple.first vehicle.position)
        (Tuple.second vehicle.position)
        vehicle


rotate : Int -> Vehicle -> Vehicle
rotate direction vehicle =
    update
        direction
        vehicle.fov
        (Tuple.first vehicle.position)
        (Tuple.second vehicle.position)
        vehicle


move : Float -> Float -> Vehicle -> Vehicle
move x y vehicle =
    update
        vehicle.direction
        vehicle.fov
        x
        y
        vehicle


createRays : Int -> Int -> Float -> Float -> List Ray
createRays direction fov x y =
    List.range direction (direction + fov - 1)
        -- |> List.filter (\angle -> modBy 10 angle == 0)
        |> List.map (toFloat >> turns >> (\n -> n / 360))
        |> List.map (\angle -> Ray.create x y ( cos angle, sin angle ))


update : Int -> Int -> Float -> Float -> Vehicle -> Vehicle
update angle fov x y vehicle =
    { vehicle
        | fov = fov
        , position = ( x, y )
        , direction = angle
        , rays = createRays angle fov x y
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


drawRay : ( Float, Float ) -> ( Float, Float ) -> Canvas.Renderable
drawRay ( x1, y1 ) ( x2, y2 ) =
    Canvas.shapes [ Canvas.stroke <| Color.rgba 1 1 1 0.2 ]
        [ Canvas.path ( x1, y1 ) [ Canvas.lineTo ( x2, y2 ) ]
        ]


drawFOV : Bool -> Int -> Int -> List Boundary -> Vehicle -> Canvas.Renderable
drawFOV projection width height walls { position, rays, fov } =
    let
        heading =
            rays
                |> List.drop (List.length rays // 2)
                |> List.head
                |> Maybe.map .direction
                |> Maybe.withDefault ( 0, 1 )

        rays_ =
            rays
                |> List.map (\ray -> ( ray, Ray.cast walls ray ))
                |> List.indexedMap (drawFovFromRay projection heading width height position)

        -- i have 1 px per fov, and want them to fill the whole width
        -- so if width = 500 and fov = 60 then scaleX = 500 / 60
    in
    Canvas.group
        (transform
            ( fov, round <| toFloat height )
            ( 0, 0 )
            ( toFloat width, toFloat height / 4 )
        )
        [ Canvas.group (translateTo ( 0, toFloat height ))
            rays_
        ]


drawFovFromRay : Bool -> ( Float, Float ) -> Int -> Int -> ( Float, Float ) -> Int -> ( Ray, Maybe ( Float, Float ) ) -> Canvas.Renderable
drawFovFromRay projection heading width height ( x1, y1 ) index ( { direction }, maybeCollisionPoint ) =
    case maybeCollisionPoint of
        Just ( x2, y2 ) ->
            let
                angle =
                    Vector.heading heading - Vector.heading direction

                distance =
                    if projection then
                        cos angle * Vector.distance ( x1, y1 ) ( x2, y2 )

                    else
                        Vector.distance ( x1, y1 ) ( x2, y2 )

                wallHeight =
                    (toFloat height / 2 / distance) * toFloat height / 4
            in
            Canvas.shapes [ Canvas.stroke <| Color.rgba 1 1 1 <| brightness (toFloat width) distance ]
                [ Canvas.path ( toFloat index, -wallHeight ) [ Canvas.lineTo ( toFloat index, wallHeight ) ]
                ]

        Nothing ->
            Canvas.shapes [] []


brightness : Float -> Float -> Float
brightness width distance =
    let
        maxDistance =
            width / 2

        sqrtMaxDistance =
            sqrt maxDistance
    in
    if distance <= 0 then
        1

    else if distance >= maxDistance then
        0

    else
        (sqrtMaxDistance - sqrt distance) / sqrtMaxDistance
