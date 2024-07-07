module Experiments.Snake.Player exposing (..)

import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color
import Utils.Number exposing (constrain)
import Utils.Vector2 as V


type alias Snake =
    { direction : ( Int, Int )
    , body : List ( Int, Int )
    , speed : Int
    }


init : Snake
init =
    { direction = ( 0, 0 )
    , body = [ ( 8, 8 ) ]
    , speed = 1
    }


update : ( Int, Int ) -> Snake -> Snake
update direction snake =
    let
        newDirection =
            if V.isZero direction then
                snake.direction

            else
                direction

        moveBody d =
            V.add newDirection d
                |> Tuple.mapBoth
                    (\x -> constrain 0 29 x)
                    (\y -> constrain 0 29 y)

        newBody =
            snake.body
                |> List.map moveBody
    in
    { snake | direction = newDirection, body = newBody }


render : Snake -> Canvas.Renderable
render snake =
    let
        bodyRenderable position =
            Canvas.rect position 1 1
    in
    Canvas.shapes [ Canvas.fill Color.white ] <|
        List.map (bodyRenderable << V.fromInt) snake.body



-- TODO enlarge if eating


eat : Snake -> ( Int, Int ) -> ( Bool, Snake )
eat snake food =
    snake.body
        |> List.head
        |> Maybe.map
            (\head ->
                if head == food then
                    ( True, snake )

                else
                    ( False, snake )
            )
        |> Maybe.withDefault ( False, snake )
