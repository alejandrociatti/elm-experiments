module Tile exposing (..)
import GraphicSVG as Graphics exposing (Shape)


type Rule a
    = AllSides a
    | Symmetrical a a a a

type Direction
    = Up
    | Right
    | Down
    | Left

type alias Tile a msg =
    { rule : Rule a
    , view : Shape msg
    }

rotate : Tile a msg -> Tile a msg
rotate tile =
    case tile.rule of
        AllSides _ ->
            tile

        Symmetrical up right down left ->
            { rule = Symmetrical left up right down
            , view = tile.view |> Graphics.rotate (degrees -90) 
            }


filter : Direction -> Tile a msg -> Tile a msg -> Bool
filter direction tile tileToCheck =
    let
        existingTileRule =
            getDirectionValue (oppositeDirection direction) tile

        toCheckTileRule =
            getDirectionValue direction tileToCheck
    in
    existingTileRule == toCheckTileRule
     


oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left -> 
            Right

        Right ->
            Left


getDirectionValue : Direction -> Tile a msg -> a
getDirectionValue direction tile =
    case tile.rule of
        AllSides a ->
            a

        Symmetrical up right down left ->
            case direction of
                Up -> 
                    up

                Right ->
                    right

                Down ->
                    down

                Left ->
                    left

