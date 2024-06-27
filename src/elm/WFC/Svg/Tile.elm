module WFC.Svg.Tile exposing (..)

import GraphicSVG as Graphics
import WFC.Common.Tile as Tile
import WFC.Svg.Types exposing (Tile)


rotate : Tile -> Tile
rotate tile =
    Tile.rotate (Graphics.rotate (degrees -90)) tile
