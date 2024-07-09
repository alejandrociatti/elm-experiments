module Experiments.WFC.Svg.Tile exposing (..)

import Experiments.WFC.Common.Tile as Tile
import Experiments.WFC.Svg.Types exposing (Tile)
import GraphicSVG as Graphics


rotate : Tile -> Tile
rotate tile =
    Tile.rotate (Graphics.rotate (degrees -90)) tile
