module Experiments.Snake.Snake exposing (..)

import Bitwise exposing (and)
import Canvas as Canvas
import Canvas.Settings as Canvas
import Color as Color
import Html exposing (a)
import Palette.Color exposing (..)
import Utils.Canvas as CU
import Utils.Number exposing (constrain, constrainReverse)
import Utils.Vector2 as V


type alias Snake =
    { body : List ( Int, Int )
    , direction : ( Int, Int )
    , living : Bool
    , size : Int
    , speed : Int
    , state : State
    }


type State
    = Blocked
    | Waiting


init : Snake
init =
    { body = [ ( 15, 15 ) ]
    , direction = ( 0, 0 )
    , living = True
    , size = 1
    , speed = 1
    , state = Waiting
    }


logicUpdate : ( Float, Float ) -> Snake -> Snake
logicUpdate rawDirection snake =
    let
        direction =
            blockAndNormalizeDirection snake.direction rawDirection

        newState =
            if direction == ( 0, 0 ) then
                Waiting

            else
                Blocked
    in
    if snake.state == Waiting then
        { snake | direction = direction, state = newState }

    else
        snake


blockAndNormalizeDirection : ( Int, Int ) -> ( Float, Float ) -> ( Int, Int )
blockAndNormalizeDirection current raw =
    raw
        |> blockDiagonals current
        |> blockReverse current
        |> continueOnZero current


blockDiagonals : ( Int, Int ) -> ( Float, Float ) -> ( Int, Int )
blockDiagonals current raw =
    if V.magnitude raw > 1 then
        -- block diagonals
        case current of
            ( 0, 0 ) ->
                ( 0, 0 )

            ( 0, _ ) ->
                ( V.toInt raw |> Tuple.first, 0 )

            ( _, 0 ) ->
                ( 0, V.toInt raw |> Tuple.second )

            _ ->
                current

    else
        V.toInt raw


blockReverse : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
blockReverse current new =
    if current == V.negate new then
        current

    else
        new


continueOnZero : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int )
continueOnZero current new =
    if new == ( 0, 0 ) then
        current

    else
        new


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
        { snake | body = newBody, state = Waiting }

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
                [ Canvas.fill (CU.color red) ]
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
