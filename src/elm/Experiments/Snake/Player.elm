module Experiments.Snake.Player exposing (..)

import Bitwise exposing (and)
import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color
import Utils.Number exposing (constrain, constrainReverse)
import Utils.Vector2 as V


type alias Snake =
    { direction : ( Int, Int )
    , body : List ( Int, Int )
    , size : Int
    , speed : Int
    , living : Bool
    }


init : Snake
init =
    { direction = ( 0, 0 )
    , body = [ ( 15, 15 ) ]
    , size = 1
    , speed = 1
    , living = True
    }


logicUpdate : ( Int, Int ) -> Snake -> Snake
logicUpdate direction snake =
    { snake | direction = direction }


update : Snake -> Snake
update snake =
    let
        moveBody d =
            V.add snake.direction d
                |> Tuple.mapBoth
                    (\x -> constrainReverse 0 29 x)
                    (\y -> constrainReverse 0 29 y)

        movedHead : Maybe ( Int, Int )
        movedHead =
            snake.body
                |> List.head
                |> Maybe.map moveBody

        movedBody : List ( Int, Int )
        movedBody =
            snake.body
                |> List.take (snake.size - 1)

        tail : List ( Int, Int )
        tail =
            snake.body
                |> List.drop 1

        living =
            snake.living
                && (movedHead
                        |> Maybe.map (\head -> not (List.member head tail))
                        |> Maybe.withDefault True
                   )

        newBody =
            movedHead
                |> Maybe.map (\head -> head :: movedBody)
                |> Maybe.withDefault snake.body
    in
    if living then
        { snake | body = newBody }

    else
        { snake | living = living }


render : Snake -> Canvas.Renderable
render snake =
    let
        bodyRenderable position =
            Canvas.rect position 1 1

        attrs =
            if snake.living then
                [ Canvas.fill Color.white ]

            else
                [ Canvas.fill Color.red ]
    in
    Canvas.shapes attrs <|
        List.map (bodyRenderable << V.fromInt) snake.body



-- TODO enlarge if eating


eat : Snake -> ( Int, Int ) -> ( Bool, Snake )
eat snake food =
    snake.body
        |> List.head
        |> Maybe.map
            (\head ->
                if head == food then
                    ( True, { snake | size = snake.size + 1 } )

                else
                    ( False, snake )
            )
        |> Maybe.withDefault ( False, snake )
