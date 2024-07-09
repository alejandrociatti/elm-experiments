module Experiments.WFC.Svg.Tile exposing (..)

import GraphicSVG as Graphics
import Experiments.WFC.Common.Tile as Tile
import Experiments.WFC.Svg.Types exposing (Tile)


rotate : Tile -> Tile
rotate tile =
    Tile.rotate (Graphics.rotate (degrees -90)) tile
