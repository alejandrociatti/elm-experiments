module Experiments.WFC.Common.Tile exposing (..)


type Rule a
    = AllSides a
    | Symmetrical a a a a


type Direction
    = Up
    | Right
    | Down
    | Left


type alias Tile a b =
    { rule : Rule a
    , view : b
    }


rotate : (b -> b) -> Tile a b -> Tile a b
rotate rotateView tile =
    case tile.rule of
        AllSides _ ->
            tile

        Symmetrical up right down left ->
            { rule = Symmetrical left up right down
            , view = tile.view |> rotateView
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
