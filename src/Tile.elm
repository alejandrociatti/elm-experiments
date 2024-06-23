module Tile exposing (..)
import GraphicSVG as Graphics exposing (Shape)


type Rule a
    = AllSides a
    | Symmetrical a a a a

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
            , view = tile.view |> Graphics.rotate (degrees 90) 
            }